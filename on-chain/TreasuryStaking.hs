{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TreasuryStaking where

import           Plutus.V1.Ledger.Value ( Value(getValue) )
import           Plutus.V2.Ledger.Api   ( BuiltinData, CurrencySymbol
                                        , ScriptContext(scriptContextTxInfo, scriptContextPurpose)
                                        , ScriptPurpose(Certifying, Rewarding), StakeValidator
                                        , TxInInfo(txInInfoResolved)
                                        , TxInfo(txInfoMint, txInfoInputs), TxOut(txOutValue)
                                        , mkStakeValidatorScript )

import           PlutusTx               ( applyCode, compile, liftCode )
import           PlutusTx.AssocMap      ( member )
import           PlutusTx.Prelude       ( ($), (.), (||), Bool(False), Maybe(Just), find
                                        , traceError )

import           Prelude                ( IO )

import           Treasury               ( HoldDatum(dGovernanceKeys), HoldParams(..), parseHoldDatum
                                        , signedByGovKeys )

import           Utilities              ( wrapStakeValidator, writeStakeValidatorToFile )

----------------------------------------------------------------------------------------------------
---------------------------------------- ON-CHAIN VALIDATOR ----------------------------------------
{-# INLINABLE mkStakeValidator #-}
mkStakeValidator :: HoldParams -> () -> ScriptContext -> Bool
mkStakeValidator params () ctx = case scriptContextPurpose ctx of
    Rewarding _  -> member (pFT params) (getValue $ txInfoMint info) || traceError "Minting"
    Certifying _ -> signedByGovKeys (dGovernanceKeys td) info || traceError "Signing"
    _            -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    td :: HoldDatum
    td = let Just i = find (member (pNFT params) . getValue . txOutValue . txInInfoResolved) $
                 txInfoInputs info
             Just d = parseHoldDatum $ txInInfoResolved i
         in d

{-# INLINABLE mkWrappedStakeValidator #-}
mkWrappedStakeValidator :: HoldParams -> BuiltinData -> BuiltinData -> ()
mkWrappedStakeValidator = wrapStakeValidator . mkStakeValidator

stakeValidator :: HoldParams -> StakeValidator
stakeValidator params = mkStakeValidatorScript $
    $$( compile [|| mkWrappedStakeValidator ||] ) `applyCode` liftCode params

----------------------------------------------------------------------------------------------------
----------------------------------------- HELPER FUNCTIONS -----------------------------------------
saveStakeValidator :: CurrencySymbol -> CurrencySymbol -> IO ()
saveStakeValidator nftPolicy
                   ftPolicy = writeStakeValidatorToFile "./assets/treasuryStaking.plutus" $
    stakeValidator HoldParams { pNFT = nftPolicy
                              , pFT  = ftPolicy
                              }
