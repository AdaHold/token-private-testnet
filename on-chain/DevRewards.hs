{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DevRewards where

import           Plutus.V1.Ledger.Value    ( Value(getValue), geq )
import           Plutus.V2.Ledger.Api      ( BuiltinData, CurrencySymbol
                                           , ScriptContext(scriptContextTxInfo)
                                           , TxInInfo(txInInfoResolved)
                                           , TxInfo(txInfoInputs, txInfoOutputs, txInfoReferenceInputs)
                                           , TxOut(txOutAddress, txOutValue), Validator
                                           , mkValidatorScript )
import           Plutus.V2.Ledger.Contexts ( findOwnInput )

import           PlutusTx                  ( applyCode, compile, liftCode )
import           PlutusTx.AssocMap         ( member )
import           PlutusTx.Prelude          ( ($), (&&), (.), (==), (||), Bool, Maybe(Just), isJust
                                           , traceError )

import           Prelude                   ( IO )

import           Treasury                  ( HoldDatum(dGovernanceKeys), HoldRedeemer(..)
                                           , parseHoldDatum, signedByGovKeys )

import           Utilities                 ( wrapValidator, writeValidatorToFile )

----------------------------------------------------------------------------------------------------
---------------------------------------- ON-CHAIN VALIDATOR ----------------------------------------
{-# INLINEABLE mkValidator #-}
mkValidator :: CurrencySymbol -> () -> HoldRedeemer -> ScriptContext -> Bool
mkValidator nftPolicy _ r ctx = case r of
    MintOrBurnRedeemer ->
        txOutValue ownOutput `geq` txOutValue ownInput || traceError "value"
    ParamsRedeemer     ->
        signedByGovKeys (dGovernanceKeys tDatum) info || traceError "Signing"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = let Just oi = findOwnInput ctx
                   a       = txOutAddress $ txInInfoResolved oi
               in case [ o
                       | i <- txInfoInputs info
                       , let o = txInInfoResolved i
                       , txOutAddress o == a
                       ] of [o] -> o
                            _   -> traceError "Input"

    ownOutput :: TxOut
    ownOutput = let a = txOutAddress ownInput
                in case [ o
                        | o <- txInfoOutputs info
                        , txOutAddress o == a && isJust (parseHoldDatum o)
                        ] of [o] -> o
                             _   -> traceError "Output"

    tDatum :: HoldDatum
    tDatum = let [out]    = [ o
                            | i <- txInfoReferenceInputs info
                            , let o = txInInfoResolved i
                            , member nftPolicy $ getValue $ txOutValue o
                            ]
                 Just dat = parseHoldDatum out
             in dat

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator . mkValidator

validator :: CurrencySymbol -> Validator
validator nftPolicy = mkValidatorScript $
    $$( compile [|| mkWrappedValidator ||] ) `applyCode` liftCode nftPolicy

----------------------------------------------------------------------------------------------------
----------------------------------------- HELPER FUNCTIONS -----------------------------------------
saveValidator :: CurrencySymbol -> IO ()
saveValidator nftPolicy = writeValidatorToFile "assets/devRewards.plutus" $ validator nftPolicy
