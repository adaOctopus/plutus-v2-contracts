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
{-# LANGUAGE MultiParamTypeClasses  #-}

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
import Plutus.V2.Ledger.Api (txInfoValidRange, Map, DatumHash, Datum)
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo)
import Plutus.V2.Ledger.Contexts qualified as V2
import Plutus.Script.Utils.Value (TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PTXMap
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

-- PlutusTx.makeLift 'EscrowDatum
PlutusTx.unstableMakeIsData 'EscrowDatum


-- | UserAction is simply an integer (lockKey from EscrowDatum) && the tokenName of the NFT minted when user locked funds.

data UserAction = Unlock Haskell.Integer TokenName deriving (Haskell.Show, FromJSON, ToJSON, Generic)

PlutusTx.makeLift ''UserAction
PlutusTx.makeIsDataIndexed ''UserAction
 [
   ( 'Unlock, 0 )
 ]

validatorFunc :: EscrowDatum -> UserAction-> V2.ScriptContext -> Bool
validatorFunc ed (Unlock ig tn) sc = True

  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo sc

    -- | 1. First check if user has the NFT that matches with NFT in the datum
    -- | 2. Second check if the user has the correct password.
    
    -- txInfoData returns a (Map DatumHash Datum) and elems returns list of all the datums.
    attachedData :: [Datum]
    attachedData = PTXMap.elems . PlutusV2.txInfoData $ info