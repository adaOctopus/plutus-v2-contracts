{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module NegativeRTimed where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (POSIXTime,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange), from)
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import           PlutusTx                  (compile, unstableMakeIsData)
import           PlutusTx.Builtins         (BuiltinData, Integer)
import           PlutusTx.Prelude          (Bool, Ord ((<=)), traceIfFalse, check, ($),
                                            (&&))
import           Plutus.V2.Ledger.Api (ScriptContext, UnsafeFromData,
                                       unsafeFromBuiltinData)
-- import           Utilities                 (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

newtype CustomDatum = MkCustomDatum { deadline :: POSIXTime }
unstableMakeIsData ''CustomDatum

{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatum -> Integer -> ScriptContext -> Bool
mkValidator (MkCustomDatum d) r ctx = traceIfFalse "expected a negative redeemer" $ r <= 0 &&
                                      traceIfFalse "deadline not reached" deadlineReached
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from d) $ txInfoValidRange info

{-# INLINABLE wrapValidator #-}
wrapValidator :: ( UnsafeFromData a
                 , UnsafeFromData b
                 )
              => (a -> b -> ScriptContext -> Bool)
              -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator mkValidator

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkWrappedValidator ||])