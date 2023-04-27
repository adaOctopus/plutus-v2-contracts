{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module PTEscrowNFT where

import qualified EscrowNFT       as OnChain
import           Control.Monad           (mapM, replicateM)
import           Plutus.Model            (Ada (Lovelace), DatumMode (HashDatum),
                                          Run, Tx,
                                          TypedValidator (TypedValidator),
                                          UserSpend, ada, adaValue,
                                          defaultBabbage, initMock, mustFail,
                                          newUser, payToKey, payToScript,
                                          runMock, spend, spendScript, submitTx,
                                          toV2, userSpend, utxoAt, valueAt, waitUntil, currentTimeRad, validateIn)
import           Plutus.V2.Ledger.Api    (PubKeyHash, TxOut (txOutValue),
                                          TxOutRef, Value, POSIXTime (POSIXTime, getPOSIXTime))
import           PlutusTx.Builtins       (Integer, mkI)
import           PlutusTx.Prelude        (Bool (..), Eq ((==)),
                                          return, ($), (&&), (.))
import           Prelude                 (IO, Ord ((<), (>)),
                                          mconcat)
import           Test.QuickCheck         (Property, Testable (property),
                                          collect, (==>), Arbitrary (arbitrary), choose)
import           Test.QuickCheck.Monadic (assert, monadic, run)
import           Test.Tasty              (defaultMain, testGroup)
import           Test.Tasty.QuickCheck   as QC (testProperty)