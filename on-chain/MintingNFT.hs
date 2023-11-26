{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MintingNFT where

import           Data.String            ( IsString(fromString), String )

import           Plutus.V1.Ledger.Value ( flattenValue )
import           Plutus.V2.Ledger.Api   ( BuiltinData, MintingPolicy
                                        , ScriptContext(scriptContextTxInfo)
                                        , TxInInfo(txInInfoOutRef), TxInfo(txInfoInputs, txInfoMint)
                                        , TxOutRef(TxOutRef), mkMintingPolicyScript )

import           PlutusTx               ( applyCode, compile, liftCode )
import           PlutusTx.Prelude       ( ($), (&&), (.), (==), (||), Bool(False), Integer, any
                                        , traceError )

import           Prelude                ( (/=), IO, read, span, tail )

import           Utilities              ( wrapPolicy, writePolicyToFile )

----------------------------------------------------------------------------------------------------
---------------------------------------- ON-CHAIN VALIDATOR ----------------------------------------
{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkPolicy utxoRef _ ctx = txHasUTxO || traceError "UTxO" && amountIsValid || traceError "Amount"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txHasUTxO :: Bool
    txHasUTxO = any (\i -> txInInfoOutRef i == utxoRef) $ txInfoInputs info

    amountIsValid :: Bool
    amountIsValid = case flattenValue $ txInfoMint info of
        [(_, _, a)] -> a == 1
        _           -> False

{-# INLINEABLE mkWrappedPolicy #-}
mkWrappedPolicy :: TxOutRef -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy = wrapPolicy . mkPolicy

policy :: TxOutRef -> MintingPolicy
policy utxoRef = mkMintingPolicyScript $
    $$( compile [|| mkWrappedPolicy ||] ) `applyCode` liftCode utxoRef

----------------------------------------------------------------------------------------------------
----------------------------------------- HELPER FUNCTIONS -----------------------------------------
savePolicy :: String -> IO ()
savePolicy txIdIdx = let (h, t) = span (/= '#') txIdIdx
                     in writePolicyToFile "assets/mintingNFT.plutus" $ policy $
                        TxOutRef (fromString h) (read (tail t) :: Integer)
