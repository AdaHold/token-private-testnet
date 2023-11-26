{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MintingBurningFT where

import           Plutus.V1.Ledger.Value ( TokenName(unTokenName), adaSymbol, adaToken, flattenValue
                                        , singleton, valueOf )
import           Plutus.V2.Ledger.Api   ( Address(Address), BuiltinData
                                        , Credential(PubKeyCredential, ScriptCredential)
                                        , CurrencySymbol, MintingPolicy, PubKeyHash(PubKeyHash)
                                        , ScriptContext(scriptContextTxInfo)
                                        , StakingCredential(StakingHash), TxInInfo(txInInfoResolved)
                                        , TxInfo(txInfoInputs, txInfoOutputs, txInfoMint, txInfoWdrl)
                                        , TxOut(txOutValue, txOutAddress)
                                        , ValidatorHash(ValidatorHash), Value(getValue)
                                        , mkMintingPolicyScript )

import           PlutusTx               ( applyCode, compile, liftCode )
import           PlutusTx.AssocMap      ( lookup, member )
import           PlutusTx.Prelude       ( ($), (&&), (*), (+), (-), (.), (<>), (==), (>), (||), Bool
                                        , Maybe(..), abs, divide, find, foldl, indexByteString
                                        , isNothing, otherwise, sliceByteString, traceError )
import           PlutusTx.Ratio         ( denominator, numerator, unsafeRatio )

import           Prelude                ( IO )

import           Treasury               ( HoldDatum(..), Lovelace, Percentage, TokenAmount
                                        , parseHoldDatum )

import           Utilities              ( wrapPolicy, writePolicyToFile )

----------------------------------------------------------------------------------------------------
---------------------------------------- ON-CHAIN VALIDATOR ----------------------------------------
data UTxO = UTxO { uValue :: Value
                 , uDatum :: HoldDatum
                 }

data Token = Token { tPolicy   :: CurrencySymbol
                   , tName     :: TokenName
                   , tAmount   :: TokenAmount
                   , tLovelace :: Lovelace
                   , tMint     :: Bool
                   }

{-# INLINEABLE mkPolicy #-}
mkPolicy :: CurrencySymbol -> () -> ScriptContext -> Bool
mkPolicy nftPolicy _ ctx = paramsIsValid
    && (if tMint holdToken then mintIsValid else burnIsValid) || traceError "Amount"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    paramsIsValid :: Bool
    paramsIsValid = dTreasuryTax treasuryInDatum == dTreasuryTax treasuryOutDatum
        && dTreasuryTaxBenefit treasuryInDatum == dTreasuryTaxBenefit treasuryOutDatum
        && dDevReward treasuryInDatum == dDevReward treasuryOutDatum
        && dDevRewardAddress treasuryInDatum == dDevRewardAddress treasuryOutDatum
        && let (k1, k2, k3)    = dGovernanceKeys treasuryInDatum
               (k1', k2', k3') = dGovernanceKeys treasuryOutDatum
           in k1 == k1' && k2 == k2' && k3 == k3' || traceError "Params"

    mintIsValid :: Bool
    mintIsValid = isNothing mCollateralIn
        && dHoldAmount treasuryInDatum + tAmount holdToken == dHoldAmount treasuryOutDatum
        && if treasuryAddr == collateralAddr
           then uValue treasuryIn <> valueChange == uValue treasuryOut
               && dCollateralAmount treasuryInDatum == dCollateralAmount treasuryOutDatum
               && dTreasuryAmount treasuryInDatum + tLovelace holdToken
               == dTreasuryAmount treasuryOutDatum
           else uValue treasuryIn == uValue treasuryOut && uValue collateralOut == valueChange
               && dCollateralAmount treasuryInDatum + tLovelace holdToken
               == dCollateralAmount treasuryOutDatum
               && dTreasuryAmount treasuryInDatum == dTreasuryAmount treasuryOutDatum
               && collateralDatumIsValid
      where
        valueChange :: Value
        valueChange = singleton adaSymbol adaToken (tLovelace holdToken)
            <> singleton (tPolicy holdToken) collateralTokenName (tAmount holdToken)

        collateralDatumIsValid :: Bool
        collateralDatumIsValid = let (_, _, b, _) = inData
                                     cd@CollateralDatum {} = uDatum collateralOut
                                     t = dTreasuryTax treasuryInDatum
                                 in dTreasuryTax cd
                                    == if numerator b == 0
                                       then t
                                       else t * unsafeRatio (denominator b - numerator b)
                                                            (denominator b)

    burnIsValid :: Bool
    burnIsValid = dHoldAmount treasuryInDatum - tAmount holdToken == dHoldAmount treasuryOutDatum
        && if treasuryAddr == collateralAddr
           then uValue treasuryIn <> singleton adaSymbol adaToken (0 - tLovelace holdToken)
               <> singleton (tPolicy holdToken) collateralTokenName (0 - tAmount holdToken)
               == uValue treasuryOut
               && dCollateralAmount treasuryInDatum == dCollateralAmount treasuryOutDatum
               && dTreasuryAmount treasuryInDatum - tLovelace holdToken
               == dTreasuryAmount treasuryOutDatum
           else outIsValid && devRewardIsValid
      where
        outIsValid :: Bool
        outIsValid = let tAmtIn   = valueOf (uValue collateralIn)
                                            (tPolicy holdToken)
                                            collateralTokenName
                         (tax, _) = taxRewardAmount
                         aAmtIn   = valueOf (uValue collateralIn) adaSymbol adaToken
                         tAmtDiff = tAmtIn - tAmount holdToken
                         aAmtDiff = (tAmtDiff * aAmtIn) `divide` (tAmtIn * 1000000)
                         aAmt     = aAmtIn - tLovelace holdToken + tax
                         taDiff   = aAmt - aAmtDiff
                     in if tAmount holdToken == tAmtIn
                        then uValue treasuryIn <> singleton adaSymbol adaToken aAmt
                            == uValue treasuryOut && dCollateralAmount treasuryInDatum - aAmtIn
                            == dCollateralAmount treasuryOutDatum && dTreasuryAmount treasuryInDatum
                            + aAmt == dTreasuryAmount treasuryOutDatum
                        else uValue treasuryIn <> singleton adaSymbol adaToken taDiff
                            == uValue treasuryOut
                            && uValue collateralOut == singleton adaSymbol adaToken aAmtDiff
                            <> singleton (tPolicy holdToken) collateralTokenName tAmtDiff
                            && dCollateralAmount treasuryInDatum - aAmtIn + aAmtDiff
                            == dCollateralAmount treasuryOutDatum && dTreasuryAmount treasuryInDatum
                            + taDiff == dTreasuryAmount treasuryOutDatum

        devRewardIsValid :: Bool
        devRewardIsValid = let (_, reward)  = taxRewardAmount
                               Just devIn   = mDevIn
                               rewardChange = singleton adaSymbol adaToken reward
                           in reward == 0 || uValue devOut
                              == if isNothing mDevIn
                                 then rewardChange
                                 else uValue devIn <> rewardChange

        taxRewardAmount :: (Lovelace, Lovelace)
        taxRewardAmount = if dHoldAmount treasuryInDatum == tAmount holdToken
                          then (0, 0)
                          else let tR = dTreasuryTax $ uDatum collateralIn
                                   tT = (tLovelace holdToken * numerator tR)
                                       `divide` denominator tR
                                   dR = tT `divide` 10
                               in (tT - dR, dR)

    utxo :: TxOut -> UTxO
    utxo o = UTxO { uValue = txOutValue o
                  , uDatum = case parseHoldDatum o of
                        Just d -> d
                        _      -> traceError "Datum"
                  }

    holdToken :: Token
    holdToken = case flattenValue $ txInfoMint info of
        [(p1, n1, a1), (p2, n2, a2)] ->
            if p1 == p2 && (n1 == collateralTokenName || n2 == collateralTokenName) && a1 == a2
            then Token { tPolicy   = p1
                       , tName     = if n1 == collateralTokenName then n2 else n1
                       , tAmount   = abs a1
                       , tLovelace = let n = dCollateralAmount treasuryInDatum
                                             + dTreasuryAmount treasuryInDatum + withdrawalAmount
                                     in if dHoldAmount treasuryInDatum == a1
                                        then n
                                        else if n == dHoldAmount treasuryInDatum * 1000000
                                             then a1 * 1000000
                                             else (a1 * n) `divide` dHoldAmount treasuryInDatum
                       , tMint     = a1 > 0 -- mint: amount > 0, burn: amount < 0
                       }
            else traceError "Token"
        _ -> traceError "Minting"

    collateralTokenName :: TokenName
    collateralTokenName = adaToken

    collateralAddr :: Address
    collateralAddr = let n = unTokenName $ tName holdToken
                         h = sliceByteString 1 56 n
                     in Address treasuryAddrCred $ Just $ StakingHash $ case indexByteString n 0 of
                            t | t == 0xE0 -> PubKeyCredential $ PubKeyHash h
                              | t == 0xF0 -> ScriptCredential $ ValidatorHash h
                              | otherwise -> traceError "Address"

    treasuryAddr :: Address
    treasuryAddr = let (a, _) = treasuryInData
                   in a

    treasuryAddrCred :: Credential
    treasuryAddrCred = let (Address cred _) = treasuryAddr
                       in cred

    treasuryAddrStCred :: StakingCredential
    treasuryAddrStCred = let (Address _ (Just stCred)) = treasuryAddr
                         in stCred

    treasuryIn :: UTxO
    treasuryIn = let (_, u) = treasuryInData
                 in u

    treasuryInDatum :: HoldDatum
    treasuryInDatum = uDatum treasuryIn

    collateralIn :: UTxO
    collateralIn = case mCollateralIn of
        Just u -> u
        _      -> traceError "Collateral"

    mCollateralIn :: Maybe UTxO
    mCollateralIn = let (t, l, p, _) = inData
                    in if t > 0
                       then Just UTxO { uValue = singleton adaSymbol adaToken l
                                            <> singleton (tPolicy holdToken) collateralTokenName t
                                      , uDatum = CollateralDatum { dTreasuryTax = p
                                                                 }
                                      }
                       else Nothing

    mDevIn :: Maybe UTxO
    mDevIn = let (_, _, _, u) = inData
             in u

    treasuryOut :: UTxO
    treasuryOut = case outData of
        (Just u, _, _) -> u
        _ -> traceError "Treasury"

    treasuryOutDatum :: HoldDatum
    treasuryOutDatum = uDatum treasuryOut

    collateralOut :: UTxO
    collateralOut = case mCollateralOut of
        Just u -> u
        _      -> traceError "Collateral"

    mCollateralOut :: Maybe UTxO
    mCollateralOut = let (_, u, _) = outData
                     in u

    devOut :: UTxO
    devOut = case outData of
        (_, _, Just u) -> u
        _ -> traceError "Dev"

    withdrawalAmount :: Lovelace
    withdrawalAmount = case lookup treasuryAddrStCred $ txInfoWdrl info of
        Just a -> a
        _      -> 0

    treasuryInData :: (Address, UTxO)
    treasuryInData = case find (member nftPolicy . getValue . txOutValue . txInInfoResolved) $
        txInfoInputs info of
            Just i -> (txOutAddress $ txInInfoResolved i, utxo $ txInInfoResolved i)
            _      -> traceError "Treasury"

    --         token total  ada total tax/benefit dev reward
    inData :: (TokenAmount, Lovelace, Percentage, Maybe UTxO)
    inData = foldl f (0, 0, z, Nothing) $ txInfoInputs info
      where
        z :: Percentage
        z = unsafeRatio 0 1

        b :: Value -> Percentage
        b v = foldl (\tb (cs, p) -> if p > tb && member cs (getValue v) then p else tb)
                    z $ dTreasuryTaxBenefit treasuryInDatum

        f :: (TokenAmount, Lovelace, Percentage, Maybe UTxO)
          -> TxInInfo
          -> (TokenAmount, Lovelace, Percentage, Maybe UTxO)
        f (tT, aT, tR, rU) i = case txInInfoResolved i of
            o | txOutAddress o == treasuryAddr -> (tT, aT, tR, rU)
              | let (Address cr _) = txOutAddress o
                    tA             = valueOf (txOutValue o) (tPolicy holdToken) collateralTokenName
                    tS             = tT + tA
                    aS             = aT + valueOf (txOutValue o) adaSymbol adaToken
                    nTR            = case dTreasuryTax $ uDatum (utxo o) of
                        dTR | tT == 0 || tR == dTR -> dTR
                            | otherwise -> unsafeRatio (tT * numerator tR) (denominator tR * tS)
                                + unsafeRatio (tA * numerator dTR) (denominator dTR * tS)
                  , cr == treasuryAddrCred && tA > 0 ->
                  if txOutAddress o == collateralAddr
                  then (tS, aS, nTR, rU)
                  else traceError "Input"
              | tMint holdToken ->
                  let tB = b $ txOutValue o
                  in (tT, aT, if tB > tR then tB else tR, rU)
              | txOutAddress o == dDevRewardAddress treasuryInDatum -> (tT, aT, tR, Just $ utxo o)
              | otherwise -> (tT, aT, tR, rU)

    --           treasury   collateral  dev reward
    outData :: (Maybe UTxO, Maybe UTxO, Maybe UTxO)
    outData = foldl f (Nothing, Nothing, Nothing) $ txInfoOutputs info
      where
        f :: (Maybe UTxO, Maybe UTxO, Maybe UTxO) -> TxOut -> (Maybe UTxO, Maybe UTxO, Maybe UTxO)
        f (tU, cU, rU)
          o | txOutAddress o == treasuryAddr = (Just $ utxo o, cU, rU)
            | txOutAddress o == collateralAddr = (tU, Just $ utxo o, rU)
            | txOutAddress o == dDevRewardAddress treasuryInDatum = (tU, cU, Just $ utxo o)
            | otherwise = (tU, cU, rU)

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: CurrencySymbol -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

policy :: CurrencySymbol -> MintingPolicy
policy nftPolicy = mkMintingPolicyScript $
    $$( compile [|| mkWrappedPolicy ||] ) `applyCode` liftCode nftPolicy

----------------------------------------------------------------------------------------------------
----------------------------------------- HELPER FUNCTIONS -----------------------------------------
savePolicy :: CurrencySymbol -> IO ()
savePolicy nftPolicy = writePolicyToFile "assets/mintingBurningFT.plutus" $ policy nftPolicy
