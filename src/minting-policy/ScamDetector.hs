{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE ImportQualifiedPost        #-}


module ScamDetector where

import Cardano.Node.Emulator.Params (testnet)
import Control.Lens (_2, (^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, DecoratedTxOut, POSIXTime, TxOutRef)
import Ledger.Tx (datumInDatumFromQuery, decoratedTxOutDatum)
import Ledger.Tx.Constraints (mustReferenceOutput)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (AsContractError, Contract, Endpoint, Promise, adjustUnbalancedTx, endpoint,
                        findReferenceValidatorScripByHash, logInfo, mkTxConstraints, ownUtxos, selectList, type (.\/),
                        utxosAt, yieldUnbalancedTx)
import Plutus.Script.Utils.Ada (toValue)
import Plutus.Script.Utils.Typed (ScriptContextV2, validatorHash)
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.V2.Ledger.Api (Address, Datum (Datum), Validator)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell