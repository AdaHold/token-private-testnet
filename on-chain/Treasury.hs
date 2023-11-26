{-# LANGUAGE DataKinds #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Treasury where

import           Plutus.V2.Ledger.Api      ( Address, BuiltinData, CurrencySymbol, Datum(Datum)
                                           , OutputDatum(..), PubKeyHash
                                           , TxInfo(txInfoOutputs, txInfoMint), TxOut(txOutAddress)
                                           , Validator, Value(getValue), mkValidatorScript )
import           Plutus.V2.Ledger.Contexts ( ScriptContext(scriptContextTxInfo)
                                           , TxInInfo(txInInfoResolved)
                                           , TxOut(txOutValue, txOutDatum), findOwnInput
                                           , txSignedBy )

import           PlutusTx                  ( applyCode, compile, fromBuiltinData, liftCode, makeLift
                                           , unstableMakeIsData )
import           PlutusTx.AssocMap         ( member )
import           PlutusTx.Prelude          ( ($), (&&), (.), (<=), (==), (>=), (||), Bool, Integer
                                           , Maybe(Just), all, find, traceError )
import           PlutusTx.Ratio            ( Rational, denominator, numerator )

import           Prelude                   ( IO )

import           Utilities                 ( wrapValidator, writeValidatorToFile )

----------------------------------------------------------------------------------------------------
--------------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ---------------------------------
type Lovelace = Integer

type TokenAmount = Integer

type Percentage = Rational

data HoldParams = HoldParams { pNFT :: CurrencySymbol
                             , pFT  :: CurrencySymbol
                             }

makeLift ''HoldParams

data HoldDatum = CollateralDatum { dTreasuryTax :: Percentage
                                 }
               | TreasuryDatum { dHoldAmount         :: TokenAmount
                               , dCollateralAmount   :: Lovelace
                               , dTreasuryAmount     :: Lovelace
                               , dTreasuryTax        :: Percentage
                               , dTreasuryTaxBenefit :: [(CurrencySymbol, Percentage)]
                               , dDevReward          :: Percentage
                               , dDevRewardAddress   :: Address
                               , dGovernanceKeys     :: (PubKeyHash, PubKeyHash, PubKeyHash)
                               }
               | UnitDatum

unstableMakeIsData ''HoldDatum

data HoldRedeemer = MintOrBurnRedeemer
                  | ParamsRedeemer

unstableMakeIsData ''HoldRedeemer

{-# INLINABLE parseHoldDatum #-}
parseHoldDatum :: TxOut -> Maybe HoldDatum
parseHoldDatum o = case txOutDatum o of
    OutputDatum (Datum dat) -> fromBuiltinData dat
    _ -> traceError "Datum"

{-# INLINABLE signedByGovKeys #-}
signedByGovKeys :: (PubKeyHash, PubKeyHash, PubKeyHash) -> TxInfo -> Bool
signedByGovKeys (k1, k2, k3)
                i = (if txSignedBy i k1 then (||) else (&&)) (txSignedBy i k2) (txSignedBy i k3)

----------------------------------------------------------------------------------------------------
---------------------------------------- ON-CHAIN VALIDATOR ----------------------------------------
{-# INLINEABLE mkValidator #-}
mkValidator :: HoldParams -> HoldDatum -> HoldRedeemer -> ScriptContext -> Bool
mkValidator params d r ctx = case r of
    MintOrBurnRedeemer -> member (pFT params) (getValue $ txInfoMint info) || traceError "Minting"
    ParamsRedeemer     ->
        signedByGovKeys (dGovernanceKeys d) info && treasuryIsValid || traceError "Signing"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    treasuryIsValid :: Bool
    treasuryIsValid = member (pNFT params) (getValue $ txOutValue ownTxOut)
        && dHoldAmount tDatumOut == dHoldAmount d
        && dCollateralAmount tDatumOut == dCollateralAmount d
        && dTreasuryAmount tDatumOut == dTreasuryAmount d
        && dTreasuryTax tDatumOut == dTreasuryTax d && dDevReward tDatumOut == dDevReward d
        && all (\(_, p) -> numerator p >= 0 && numerator p <= denominator p)
               (dTreasuryTaxBenefit tDatumOut)
      where
        ownTxOut :: TxOut
        ownTxOut = let Just i = findOwnInput ctx
                   in txInInfoResolved i

        tDatumOut :: HoldDatum
        tDatumOut = let Just t   = find (\o -> txOutAddress o == txOutAddress ownTxOut
                                         && txOutValue o == txOutValue ownTxOut) $
                            txInfoOutputs info
                        Just dat = parseHoldDatum t
                    in dat

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: HoldParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: HoldParams -> Validator
validator params = mkValidatorScript $
    $$( compile [|| mkWrappedValidator ||] ) `applyCode` liftCode params

----------------------------------------------------------------------------------------------------
----------------------------------------- HELPER FUNCTIONS -----------------------------------------
saveValidator :: CurrencySymbol -> CurrencySymbol -> IO ()
saveValidator nftPolicy ftPolicy = writeValidatorToFile "assets/treasury.plutus" $
    validator HoldParams { pNFT = nftPolicy
                         , pFT  = ftPolicy
                         }
