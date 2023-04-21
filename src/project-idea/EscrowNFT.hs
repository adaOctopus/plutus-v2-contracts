{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}

-- | A simple escrow contract, when during the locking, we mint NFT and during unlocking we burn it.

module EscrowNFT
  where

import Cardano.Node.Emulator.Params qualified as Params
import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (POSIXTime, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, getCardanoTxId)
import Ledger qualified
import Ledger.Interval (after, before)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.ValidityInterval qualified as Interval
import Ledger.Typed.Scripts (ScriptContextV2)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value, geq)
import Plutus.V2.Ledger.Api (txInfoValidRange)
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo)
import Plutus.V2.Ledger.Contexts qualified as V2
import Plutus.Script.Utils.Value (TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Prelude (Semigroup (..), foldMap)
import Prelude qualified as Haskell


data EscrowDatum = EscrowDatum {
    lockOwner  :: PaymentPubKeyHash,
    lockAmount :: Haskell.Integer,
    lockNFT    :: TokenName,
    lockKey    :: Haskell.Integer
} deriving (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift 'EscrowDatum