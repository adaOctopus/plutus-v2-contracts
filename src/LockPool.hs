{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module LockPool
  -- ( LockPoolSchema,
  --   StartParams (..),
  --   BuyParams (..),
  --   MintParams (..),
  --   MintIdentifyParams (..),
  --   WinnerParams (..),
  --   ClaimParams (..),
  (
    ParamFilter (..),
    -- lockPool,
    lockPoolSerialised,
    lockPoolSBS,
    curSymbol,
    LockPoolRedeemer (..),
    LockPoolDatum (..),
  )
where

-- BLOCK0

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), fromPlutusData, scriptDataToJson)
import Codec.Serialise
import Control.Monad (void)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Extras as JSON
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Ledger as Ledg
import qualified Ledger.Address as LA
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract
import Plutus.Contract as Contract
import Plutus.V1.Ledger.Address as SpecAddr
import qualified Plutus.V1.Ledger.Scripts as Plutus
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.V1.Ledger.Tx as LedgerTX
import Ledger (ChainIndexTxOut)
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class as BC
import PlutusTx.Prelude
import Prelude (Semigroup (..))
import qualified Prelude as Haskell
import Data.Bits (shift, (.|.))
import Data.Char
import PlutusTx.Builtins

data ParamFilter = ParamFilter
  { ownerPkh :: Ledg.PubKeyHash,
    threadToken :: !Ledg.AssetClass, -- NFT
    identifyToken :: !Ledg.AssetClass -- native token
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''ParamFilter
PlutusTx.makeLift ''ParamFilter

data LockPoolDatum
  = MainDatum
      { nextNumber :: Integer,
        treasuryAccount :: Ledg.PubKeyHash
      }
  | TicketDatum
      { fromNumber :: Integer,
        toNumber :: Integer,
        buyerPkh :: Ledg.PubKeyHash,
        amount :: Integer
      }
  | WinnerDatum
      { winNumber :: Integer
      }
  | RandaoDatum
      { 
      roundNumber :: Integer,
        finalNumber :: BuiltinByteString
      }
  deriving (Show)

PlutusTx.unstableMakeIsData ''LockPoolDatum
PlutusTx.makeLift ''LockPoolDatum

-- check state
instance Eq LockPoolDatum where
  MainDatum nn ta == MainDatum nn' ta' = nn == nn' && ta == ta'
  TicketDatum fn tn bp am == TicketDatum fn' tn' bp' am' = fn == fn' && tn == tn' && bp == bp' && am == am'
  WinnerDatum wn == WinnerDatum wn' = wn == wn'
  _ == _ = False

data LockPoolRedeemer
  = Buy Integer
  | Winner Ledg.AssetClass
  | Test Ledg.AssetClass
  | Claim
  deriving (Show)

instance Eq LockPoolRedeemer where
  Buy nt == Buy nt' = nt == nt'
  Winner _ == Winner _ = True
  Claim == Claim = True
  Test _ == Test _ = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''LockPoolRedeemer
PlutusTx.makeLift ''LockPoolRedeemer

lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

lockPoolDatum :: TxOut -> (Plutus.DatumHash -> Maybe Plutus.Datum) -> Maybe LockPoolDatum
lockPoolDatum o f = do
  dh <- txOutDatum o
  Plutus.Datum d <- f dh
  PlutusTx.fromBuiltinData d

mkLockPoolValidator :: ParamFilter -> LockPoolDatum -> LockPoolRedeemer -> ScriptContext -> Bool
mkLockPoolValidator params dat red ctx =
  case (dat, red) of
    -- next number, treasury account for maindatum
    -- number integer for BUy Redeemer
    (MainDatum nn ta, Buy nt) ->
      traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
        && traceIfFalse "dont have enough money" (checkEnoughFund nt ta)
        && traceIfFalse "wrong output main datum" (outputMainDatum == MainDatum (nn + nt) ta)
        && traceIfFalse "wrong output ticket datum" (outputTicketDatum ownIdentOutputs == TicketDatum nn (nn + nt - 1) (head $ txInfoSignatories $ scriptContextTxInfo ctx) nt)
        && traceIfFalse "wrong output identify token" (ownIdentInputAmount == ownIdentOutputAmount + 1)
    (MainDatum nn ta, Winner assetClass) ->
      traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
        && traceIfFalse "wrong output main datum" (outputMainDatum == MainDatum nn ta)
        && traceIfFalse "wrong output winner datum" (outputWinnerDatum ownIdentOutputs == WinnerDatum  ((getWinnerFromRandao assetClass) `modInteger` nn))
        && traceIfFalse "wrong output identify token" (ownIdentInputAmount == ownIdentOutputAmount + 1)
        && traceIfFalse "only by owner" (txSignedBy info (ownerPkh params))
    (TicketDatum fn tn bp am, Claim) ->
      traceIfFalse "wrong output ticket datum" (outputTicketDatum ownIdentOutputs == TicketDatum fn tn bp am)
        && traceIfFalse "check winner" (checkWinnerTicket fn tn)
        && traceIfFalse "check return identify token for owner" checkReturnIdentToken
        && traceIfFalse "only by buyer" (txSignedBy info bp)
    (WinnerDatum nt, Claim) ->
      traceIfFalse "wrong output ticket datum" (outputTicketDatum ownIdentOutputs == outputTicketDatum (ownIdentUtxos ownIdentInputs))
        && traceIfFalse "check winner" (checkWinnerTicketForWinner nt (outputTicketDatum (ownIdentUtxos ownIdentInputs)))
        && traceIfFalse "check return identify token for owner" checkReturnIdentToken
        && traceIfFalse "only by buyer" (txSignedBy info (getBuyerPK (outputTicketDatum (ownIdentUtxos ownIdentInputs))))
    _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- find winner datum in any input
    ownIdentifyRandaoInput :: AssetClass -> [Ledger.TxInInfo]
    ownIdentifyRandaoInput assetClass = filter (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) assetClass == 1) (txInfoInputs info)
        
    ownIdentifyRandaoOutput :: [Ledger.TxInInfo] -> [Ledger.TxOut]
    ownIdentifyRandaoOutput arr = map txInInfoResolved arr

    -- find output randao datum
    isRandaoDatum :: LockPoolDatum -> Bool
    isRandaoDatum TicketDatum {} = True
    isRandaoDatum _ = False

    checkRandaoDatumType :: Ledger.TxOut -> Bool
    checkRandaoDatumType txOut = maybe False isRandaoDatum (lockPoolDatum txOut (`findDatum` info))

    outputRandaoDatum :: [Ledger.TxOut] -> LockPoolDatum
    outputRandaoDatum [] = traceError "tx randao output datum not found datum data"
    outputRandaoDatum (x : tl) =
      if checkRandaoDatumType x
        then case lockPoolDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputRandaoDatum tl
        else outputRandaoDatum tl

    readInt :: BuiltinByteString -> Integer
    readInt bs = 
          -- mulInt (lengthOfByteString bs) bs 
          indexByteString bs 0 
          * indexByteString bs 1
          * indexByteString bs 2
          * indexByteString bs 3
          * indexByteString bs 4
          * indexByteString bs 5
          * indexByteString bs 6
          * indexByteString bs 8
          * indexByteString bs 9
          * indexByteString bs 10
          * indexByteString bs 11
          * indexByteString bs 12
          * indexByteString bs 13
          * indexByteString bs 14
          * indexByteString bs 15

    -- mulInt :: Integer -> BuiltinByteString -> Integer
    -- mulInt (0 :: Integer) _ = 1 :: Integer
    -- mulInt (1 :: Integer) bs = indexByteString bs 0 
    -- mulInt a bs = indexByteString bs (a - 1) `multiplyInteger` mulInt ( a - 1 ) bs 

    genRandom :: BuiltinByteString -> Integer
    genRandom bbs = readInt bbs

    getWinnerFromRandao :: AssetClass -> Integer  
    getWinnerFromRandao assetClass = genRandom $ finalNumber $ outputRandaoDatum $ ownIdentifyRandaoOutput $ ownIdentifyRandaoInput assetClass
    
    -- genRandom $ finalNumber $ outputRandaoDatum $ ownIdentifyRandaoOutput $ ownIdentifyRandaoInput assetClass

    -- find thread NFT in any input
    ownNftInput :: Bool
    ownNftInput = case find (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (threadToken params) == 1) (txInfoInputs info) of
      Nothing -> False
      Just _ -> True

    -- getContinuingOutputs && find thread NFT in any output need check return for script
    ownNftOutput :: Bool
    ownNftOutput = case find (\x -> Value.assetClassValueOf (txOutValue x) (threadToken params) == 1) (getContinuingOutputs ctx) of
      Nothing -> False
      Just _ -> True

    -- check enough fund (1 ticket == 1000000 lovelace)
    checkEnoughFund :: Integer -> PubKeyHash -> Bool
    checkEnoughFund nt ta = case find (\x -> txOutAddress x == SpecAddr.pubKeyHashAddress ta && lovelaces (txOutValue x) == (nt * 1000000)) (txInfoOutputs info) of
      Nothing -> False
      Just _ -> True

    -- find output main datum
    ownThreshOutput :: Maybe Ledger.TxOut
    ownThreshOutput = find (\x -> (isJust . txOutDatumHash) x && Value.assetClassValueOf (txOutValue x) (threadToken params) == 1) (getContinuingOutputs ctx)

    outputMainDatum :: LockPoolDatum
    outputMainDatum = case ownThreshOutput of
      Nothing -> traceError "tx lock pool output datum not found datum hash"
      Just outDatum -> case lockPoolDatum outDatum (`findDatum` info) of
        Nothing -> traceError "tx lock pool output datum not found datum data"
        Just d -> d

    -- find all of output of script
    ownIdentOutputs :: [Ledger.TxOut]
    ownIdentOutputs = filter (\x -> (isJust . txOutDatumHash) x && Value.assetClassValueOf (txOutValue x) (identifyToken params) == 1) (getContinuingOutputs ctx)

    -- find all of input of script
    ownIdentInputs :: [Ledger.TxInInfo]
    ownIdentInputs = filter (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (identifyToken params) == 1) (txInfoInputs info)

    ownIdentUtxos :: [Ledger.TxInInfo] -> [Ledger.TxOut]
    ownIdentUtxos arr = map txInInfoResolved arr

    -- find output ticket datum
    isTicketDatum :: LockPoolDatum -> Bool
    isTicketDatum TicketDatum {} = True
    isTicketDatum _ = False

    checkTicketDatumType :: Ledger.TxOut -> Bool
    checkTicketDatumType txOut = maybe False isTicketDatum (lockPoolDatum txOut (`findDatum` info))

    outputTicketDatum :: [Ledger.TxOut] -> LockPoolDatum
    outputTicketDatum [] = traceError "tx ticket output datum not found datum data"
    outputTicketDatum (x : tl) =
      if checkTicketDatumType x
        then case lockPoolDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputTicketDatum tl
        else outputTicketDatum tl

    -- find output winner datum
    isWinnerDatum :: LockPoolDatum -> Bool
    isWinnerDatum (WinnerDatum _) = True
    isWinnerDatum _ = False

    checkWinnerDatumType :: Ledger.TxOut -> Bool
    checkWinnerDatumType txOut = maybe False isWinnerDatum (lockPoolDatum txOut (`findDatum` info))

    outputWinnerDatum :: [Ledger.TxOut] -> LockPoolDatum
    outputWinnerDatum [] = traceError "tx winner output datum not found datum data"
    outputWinnerDatum (x : tl) =
      if checkWinnerDatumType x
        then case lockPoolDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputWinnerDatum tl
        else outputWinnerDatum tl

    -- count identity token input
    ownIdentInputAmount :: Integer
    ownIdentInputAmount = case find (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (threadToken params) == 1) (txInfoInputs info) of
      Nothing -> 0
      Just input -> Value.assetClassValueOf (txOutValue $ txInInfoResolved input) (identifyToken params)

    -- count identity token output
    ownIdentOutputAmount :: Integer
    ownIdentOutputAmount = case find (\x -> Value.assetClassValueOf (txOutValue x) (threadToken params) == 1) (getContinuingOutputs ctx) of
      Nothing -> 0
      Just output -> Value.assetClassValueOf (txOutValue output) (identifyToken params)

    -- find winner datum in any input
    ownIdentifyInput :: [TxInInfo]
    ownIdentifyInput = filter (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (identifyToken params) == 1) (txInfoInputs info)

    getTxout :: [Ledger.TxOut]
    getTxout = map txInInfoResolved ownIdentifyInput

    checkWinnerTicket :: Integer -> Integer -> Bool
    checkWinnerTicket fr tt = fr <= winNumber winnerDatum && tt >= winNumber winnerDatum where winnerDatum = outputWinnerDatum getTxout

    checkWinnerTicketForWinner :: Integer -> LockPoolDatum -> Bool
    checkWinnerTicketForWinner nt (TicketDatum fr tt _ _) = fr <= nt && tt >= nt
    checkWinnerTicketForWinner _ _ = False

    getBuyerPK :: LockPoolDatum -> PubKeyHash
    getBuyerPK (TicketDatum _ _ bp _) = bp
    getBuyerPK _ = traceError "dont have buyer pkh"
    -- check enough ident token to return
    checkReturnIdentToken :: Bool
    checkReturnIdentToken = case find (\x -> txOutAddress x == SpecAddr.pubKeyHashAddress (ownerPkh params) && Value.assetClassValueOf (txOutValue x) (identifyToken params) == 1) (txInfoOutputs info) of
      Nothing -> False
      Just _ -> True

data LockPoolType
instance Scripts.ValidatorTypes LockPoolType where
  type DatumType LockPoolType = LockPoolDatum
  type RedeemerType LockPoolType = LockPoolRedeemer

typedLockPoolValidator :: ParamFilter -> Scripts.TypedValidator LockPoolType
typedLockPoolValidator params =
  Scripts.mkTypedValidator @LockPoolType
    ( $$(PlutusTx.compile [||mkLockPoolValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode params
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @LockPoolDatum @LockPoolRedeemer

-- create NFT thread script
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn _ ctx =
  traceIfFalse "UTxO not consumed" hasUTxO
    && traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue (txInfoMint info) of
      [(cs, tn', amt)] -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
      _ -> False

policy :: TxOutRef -> TokenName -> Plutus.MintingPolicy
policy oref tn =
  Plutus.mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' tn' -> Scripts.mkUntypedMintingPolicy $ mkPolicy oref' tn'||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
      `PlutusTx.applyCode` PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = Plutus.Script.Utils.V1.Scripts.scriptCurrencySymbol $ policy oref tn

-- create identify token
mkIdentifyPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkIdentifyPolicy pkh _ ctx = txSignedBy (scriptContextTxInfo ctx) pkh

identifyPolicy :: PubKeyHash -> Plutus.MintingPolicy
identifyPolicy pkh =
  Plutus.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkIdentifyPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

curIdentifySymbol :: PubKeyHash -> CurrencySymbol
curIdentifySymbol = Plutus.Script.Utils.V1.Scripts.scriptCurrencySymbol . identifyPolicy

lockPoolValidator :: ParamFilter ->  Plutus.Validator
lockPoolValidator = Scripts.validatorScript . typedLockPoolValidator

-- {-
--     As a Script
-- -}

lockPoolScript :: ParamFilter -> Plutus.Script
lockPoolScript = Plutus.unValidatorScript . lockPoolValidator

-- {-
--     As a Short Byte String
-- -}

lockPoolSBS :: ParamFilter -> SBS.ShortByteString
lockPoolSBS = SBS.toShort . LBS.toStrict . serialise . lockPoolScript

-- {-
--     As a Serialised Script
-- -}

lockPoolSerialised :: ParamFilter -> PlutusScript PlutusScriptV1
lockPoolSerialised = PlutusScriptSerialised . lockPoolSBS

lockPoolAddress :: ParamFilter -> Ledger.Address
lockPoolAddress = Ledg.scriptAddress . lockPoolValidator

lockPoolOffchainDatums :: Map TxOutRef ChainIndexTxOut -> [(TxOutRef, Maybe LockPoolDatum)]
lockPoolOffchainDatums o = map (\x -> (x, getOffchainDatum x o)) txoutKeys
  where
    txoutKeys = Map.keys o

getLockPoolOffchainDatums :: Integer -> Map TxOutRef ChainIndexTxOut -> [(TxOutRef, Maybe LockPoolDatum)]
getLockPoolOffchainDatums dt o =
  filter (\(_, x) -> isDatumType dt x) arr
  where
    arr = lockPoolOffchainDatums o

checkDatumOffchain :: Integer -> LockPoolDatum -> Bool
checkDatumOffchain 0 (MainDatum _ _) = True
checkDatumOffchain 1 TicketDatum {} = True
checkDatumOffchain 2 (WinnerDatum _) = True
checkDatumOffchain _ _ = False

isDatumType :: Integer -> Maybe LockPoolDatum -> Bool
isDatumType dt x = case x of
  Nothing -> False
  Just a -> checkDatumOffchain dt a

getOffchainDatum :: TxOutRef -> Map TxOutRef ChainIndexTxOut -> Maybe LockPoolDatum
getOffchainDatum txoutRef chainMap = do
  chainIndexTxOut <- Map.lookup txoutRef chainMap
  Plutus.Datum d <- either (const Nothing) Just (txOutDatum chainIndexTxOut)
  PlutusTx.fromBuiltinData d

toCurrencySymbol :: Haskell.String -> CurrencySymbol
toCurrencySymbol str = case JSON.tryDecode $ T.pack str of
  Left _ -> Value.currencySymbol $ fromBuiltin emptyByteString
  Right b -> Value.currencySymbol b

toPkh :: Haskell.String -> PubKeyHash
toPkh str = case JSON.tryDecode $ T.pack str of
  Left _ -> PubKeyHash emptyByteString
  Right b -> PubKeyHash $ toBuiltin b

toTokenName :: Haskell.String -> TokenName
toTokenName = TokenName . BC.toBuiltin . C.pack

-- data MintParams = MintParams
--   { tokenName :: !Haskell.String
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- mintThreadToken :: AsContractError e => MintParams -> Contract w s e ()
-- mintThreadToken mp = do
--   pkh <- Contract.ownPaymentPubKeyHash
--   utxos <- utxosAt $ pubKeyHashAddress pkh
--   case Map.keys utxos of
--     [] -> Contract.logError @Haskell.String "no utxo found"
--     oref : _ -> do
--       let tn = (TokenName . BC.toBuiltin . C.pack . tokenName) mp
--           val = Value.singleton (curSymbol oref tn) tn 1
--           lookups =
--             Constraints.mintingPolicy (policy oref tn)
--               Prelude.<> Constraints.unspentOutputs utxos
--           tx =
--             Constraints.mustMintValue val
--               PlutusTx.Prelude.<> Constraints.mustSpendPubKeyOutput oref
--       logInfo @Haskell.String $ "Thread token: " ++ Haskell.show (curSymbol oref tn)
--       void $ submitTxConstraintsWith @Scripts.Any lookups tx

-- data MintIdentifyParams = MintIdentifyParams
--   { mpTokenName :: Haskell.String,
--     mpAmount :: !Integer,
--     mpOwner :: Haskell.String
--   }
--   deriving stock (Haskell.Eq, Haskell.Show, Generic)
--   deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- mintIdentifyToken :: AsContractError e => MintIdentifyParams -> Contract w s e ()
-- mintIdentifyToken mp = do
--   let ownerPkh = toPkh $ mpOwner mp
--       val = Value.singleton (curIdentifySymbol ownerPkh) (toTokenName (mpTokenName mp)) (mpAmount mp)
--       lookups = Constraints.mintingPolicy $ identifyPolicy ownerPkh
--       tx = Constraints.mustMintValue val
--   void $ submitTxConstraintsWith @Scripts.Any lookups tx

-- data StartParams = StartParams
--   { sOwner :: Haskell.String,
--     sTreasury :: Haskell.String,
--     sNftCurrency :: Haskell.String,
--     sNftTokenName :: Haskell.String,
--     sIdentCurrency :: Haskell.String,
--     sIdentTokenName :: Haskell.String
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- convertToEncodeJson :: BuiltinByteString -> JSON.Value
-- convertToEncodeJson bbs = scriptDataToJson ScriptDataJsonDetailedSchema (fromPlutusData $ PlutusTx.builtinDataToData $ PlutusTx.toBuiltinData bbs)

-- startAction :: AsContractError e => StartParams -> Contract w s e ()
-- startAction sp = do
--   let params =
--         ParamFilter
--           { ownerPkh = toPkh $ sOwner sp,
--             threadToken = Value.AssetClass (toCurrencySymbol $ sNftCurrency sp, toTokenName $ sNftTokenName sp),
--             identifyToken = Value.AssetClass (toCurrencySymbol $ sIdentCurrency sp, toTokenName $ sIdentTokenName sp)
--           }
--       v = Ada.lovelaceValueOf 2000000
--       t = Value.assetClassValue (threadToken params) 1
--       k = Value.assetClassValue (identifyToken params) 1000
--       tx = Constraints.mustPayToTheScript (MainDatum 0 (toPkh $ sTreasury sp)) (v Prelude.<> t Prelude.<> k)

--   ledgerTx <- submitTxConstraints (typedLockPoolValidator params) tx
--   case ledgerTx of
--     Right tx' -> void $ awaitTxConfirmed $ txId tx'
--     Left _ -> logWarn @Haskell.String "no tx"

--   -- let datum = convertToEncodeJson seedHash
--   -- logInfo @Haskell.String $ "made start: " ++ Haskell.show (JSON.encode datum)
--   logInfo @Haskell.String $ "nft currency symbol: " ++ Haskell.show (toCurrencySymbol $ sNftCurrency sp)
--   logInfo @Haskell.String $ "identity currency symbol: " ++ Haskell.show (toCurrencySymbol $ sIdentCurrency sp)

-- data BuyParams = BuyParams
--   { bOwner :: Haskell.String,
--     bNftCurrency :: Haskell.String,
--     bNftTokenName :: Haskell.String,
--     bIdentCurrency :: Haskell.String,
--     bIdentTokenName :: Haskell.String,
--     bTreasury :: Haskell.String,
--     bBuyer :: Haskell.String,
--     bCurrentNextTicket :: Integer,
--     bNumIdentToken :: Integer,
--     bNumTicket :: Integer
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- buyAction :: AsContractError e => BuyParams -> Contract w s e ()
-- buyAction bp = do
--   logInfo @Haskell.String "no running game found"
--   let params =
--         ParamFilter
--           { ownerPkh = toPkh $ bOwner bp,
--             threadToken = Value.AssetClass (toCurrencySymbol $ bNftCurrency bp, toTokenName $ bNftTokenName bp),
--             identifyToken = Value.AssetClass (toCurrencySymbol $ bIdentCurrency bp, toTokenName $ bIdentTokenName bp)
--           }
--   utxos <- utxosAt $ lockPoolAddress params
--   let mainUtxos = getLockPoolOffchainDatums 0 utxos
--       v = Ada.lovelaceValueOf 2000000
--       t = Value.assetClassValue (threadToken params) 1
--       k1 = Value.assetClassValue (identifyToken params) 1
--       k2 = Value.assetClassValue (identifyToken params) (bNumIdentToken bp - 1)
--       datum1 =
--         MainDatum
--           { nextNumber = bCurrentNextTicket bp + bNumTicket bp,
--             treasuryAccount = toPkh $ bTreasury bp
--           }
--       datum2 =
--         TicketDatum
--           { fromNumber = bCurrentNextTicket bp,
--             toNumber = bCurrentNextTicket bp + bNumTicket bp - 1,
--             buyerPkh = toPkh $ bBuyer bp,
--             amount = bNumTicket bp
--           }
--       lookups =
--         Constraints.unspentOutputs utxos
--           Prelude.<> Constraints.otherScript (lockPoolValidator params)
--           Prelude.<> Constraints.typedValidatorLookups (typedLockPoolValidator params)
--       tx =
--         Constraints.mustSpendScriptOutput (fst $ head mainUtxos) (Plutus.Redeemer $ PlutusTx.toBuiltinData $ Buy (bNumTicket bp))
--           PlutusTx.Prelude.<> Constraints.mustPayToTheScript datum1 (v Prelude.<> t Prelude.<> k2)
--           PlutusTx.Prelude.<> Constraints.mustPayToTheScript datum2 (v Prelude.<> k1)
--           PlutusTx.Prelude.<> Constraints.mustPayToPubKey (toPkh $ bTreasury bp) (Ada.lovelaceValueOf $ bNumTicket bp * 1000000)
--   ledgerTx <- submitTxConstraintsWith @LockPoolType lookups tx
--   case ledgerTx of
--     Right tx' -> void $ awaitTxConfirmed $ txId tx'
--     Left _ -> logWarn @Haskell.String "no tx"

-- -- logInfo @Haskell.String $ "made second move: " ++ Haskell.show (rrandom rp)
-- -- logInfo @Haskell.String $ "made second move contribute: " ++ Haskell.show (JSON.encode $ convertToEncodeJson $ sha2_256 (datum `appendByteString` BC.toBuiltin  (C.pack  $ rrandom rp)))

-- data WinnerParams = WinnerParams
--   { wOwner :: Haskell.String,
--     wNftCurrency :: Haskell.String,
--     wNftTokenName :: Haskell.String,
--     wIdentCurrency :: Haskell.String,
--     wIdentTokenName :: Haskell.String,
--     wTreasury :: Haskell.String,
--     wCurrentNextTicket :: Integer,
--     wRandomNumber :: Integer,
--     wNumIdentToken :: Integer
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- setwinnerAction :: AsContractError e => WinnerParams -> Contract w s e ()
-- setwinnerAction sw = do
--   -- let params =
--   --       ParamFilter
--   --         { ownerPkh = toPkh $ wOwner sw,
--   --           threadToken = Value.AssetClass (toCurrencySymbol $ wNftCurrency sw, toTokenName $ wNftTokenName sw),
--   --           identifyToken = Value.AssetClass (toCurrencySymbol $ wIdentCurrency sw, toTokenName $ wIdentTokenName sw)
--   --         }
--   -- utxos <- utxosAt $ lockPoolAddress params
--   -- let mainUtxos = getLockPoolOffchainDatums 0 utxos
--   --     v = Ada.lovelaceValueOf 2000000
--   --     reward = Ada.lovelaceValueOf 20000000
--   --     t = Value.assetClassValue (threadToken params) 1
--   --     k1 = Value.assetClassValue (identifyToken params) 1
--   --     k2 = Value.assetClassValue (identifyToken params) (wNumIdentToken sw - 1)
--   --     datum1 =
--   --       MainDatum
--   --         { nextNumber = wCurrentNextTicket sw,
--   --           treasuryAccount = toPkh $ wTreasury sw
--   --         }
--   --     datum2 =
--   --       WinnerDatum
--   --         { winNumber = wRandomNumber sw
--   --         }
--   --     lookups =
--   --       Constraints.unspentOutputs utxos
--   --         Prelude.<> Constraints.otherScript (lockPoolValidator params)
--   --         Prelude.<> Constraints.typedValidatorLookups (typedLockPoolValidator params)
--   --     tx =
--   --       Constraints.mustSpendScriptOutput (fst $ head mainUtxos) (Redeemer $ PlutusTx.toBuiltinData $ Winner (wRandomNumber sw))
--   --         PlutusTx.Prelude.<> Constraints.mustPayToTheScript datum1 (v Prelude.<> t Prelude.<> k2)
--   --         PlutusTx.Prelude.<> Constraints.mustPayToTheScript datum2 (reward Prelude.<> k1)
--   -- ledgerTx <- submitTxConstraintsWith @LockPoolType lookups tx
--   -- case ledgerTx of
--   --   Right tx' -> void $ awaitTxConfirmed $ txId tx'
--   --   Left _ -> logWarn @Haskell.String "no tx"
--   logInfo @Haskell.String $ "made second move: "

-- data ClaimParams = ClaimParams
--   { clOwner :: Haskell.String,
--     clNftCurrency :: Haskell.String,
--     clNftTokenName :: Haskell.String,
--     clIdentCurrency :: Haskell.String,
--     clIdentTokenName :: Haskell.String,
--     clTreasury :: Haskell.String,
--     clRandomNumber :: Integer,
--     clBuyer :: Haskell.String,
--     clCurrentNextTicket :: Integer,
--     clNumIdentToken :: Integer,
--     clNumTicket :: Integer
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- claimAction :: AsContractError e => ClaimParams -> Contract w s e ()
-- claimAction cl = do
--   let params =
--         ParamFilter
--           { ownerPkh = toPkh $ clOwner cl,
--             threadToken = Value.AssetClass (toCurrencySymbol $ clNftCurrency cl, toTokenName $ clNftTokenName cl),
--             identifyToken = Value.AssetClass (toCurrencySymbol $ clIdentCurrency cl, toTokenName $ clIdentTokenName cl)
--           }
--   utxos <- utxosAt $ lockPoolAddress params
--   let ticketUtxos = getLockPoolOffchainDatums 1 utxos
--       winnerUtxos = getLockPoolOffchainDatums 2 utxos
--       v = Ada.lovelaceValueOf 2000000
--       reward = Ada.lovelaceValueOf 20000000
--       k1 = Value.assetClassValue (identifyToken params) 1
--       k2 = Value.assetClassValue (identifyToken params) 1
--       datum =
--         TicketDatum
--           { fromNumber = clCurrentNextTicket cl,
--             toNumber = clCurrentNextTicket cl + clNumTicket cl - 1,
--             buyerPkh = toPkh $ clBuyer cl,
--             amount = clNumTicket cl
--           }

--       lookups =
--         Constraints.unspentOutputs utxos
--           Prelude.<> Constraints.otherScript (lockPoolValidator params)
--           Prelude.<> Constraints.typedValidatorLookups (typedLockPoolValidator params)
--       tx =
--         Constraints.mustSpendScriptOutput (fst $ head ticketUtxos) (Plutus.Redeemer $ PlutusTx.toBuiltinData Claim)
--           PlutusTx.Prelude.<> Constraints.mustSpendScriptOutput (fst $ head winnerUtxos) (Plutus.Redeemer $ PlutusTx.toBuiltinData Claim)
--           PlutusTx.Prelude.<> Constraints.mustPayToTheScript datum (v Prelude.<> k2)
--           PlutusTx.Prelude.<> Constraints.mustPayToPubKey (buyerPkh datum) reward
--           PlutusTx.Prelude.<> Constraints.mustPayToPubKey (ownerPkh params) k1
--   ledgerTx <- submitTxConstraintsWith @LockPoolType lookups tx
--   case ledgerTx of
--     Right tx' -> void $ awaitTxConfirmed $ txId tx'
--     Left _ -> logWarn @Haskell.String "no tx"
--   logInfo @Haskell.String $ "made third move: "

-- type LockPoolSchema =
--   Endpoint "mint" MintParams
--     .\/ Endpoint "mintid" MintIdentifyParams
--     .\/ Endpoint "start" StartParams
--     .\/ Endpoint "buy" BuyParams
--     .\/ Endpoint "setwinner" WinnerParams
--     .\/ Endpoint "claim" ClaimParams

-- start :: AsContractError e => Promise () LockPoolSchema e ()
-- start = endpoint @"start" startAction

-- buy :: AsContractError e => Promise () LockPoolSchema e ()
-- buy = endpoint @"buy" buyAction

-- setwinner :: AsContractError e => Promise () LockPoolSchema e ()
-- setwinner = endpoint @"setwinner" setwinnerAction

-- mintIdTk :: AsContractError e => Promise () LockPoolSchema e ()
-- mintIdTk = endpoint @"mintid" mintIdentifyToken

-- mintThTk :: AsContractError e => Promise () LockPoolSchema e ()
-- mintThTk = endpoint @"mint" mintThreadToken

-- claim :: AsContractError e => Promise () LockPoolSchema e ()
-- claim = endpoint @"claim" claimAction

-- lockPool :: AsContractError e => Contract () LockPoolSchema e ()
-- -- BLOCK10

-- lockPool = selectList [start, mintThTk, mintIdTk, buy, setwinner, claim]