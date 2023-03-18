{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Randao
  (
  --   RandaoSchema,
  --   StartParams (..),
  --   CommitParams (..),
  --   RevealParams (..),
  --   MintParams (..),
  --   MintIdentifyParams (..),
    ParamFilter (..),
    -- randao,
    randaoSerialised,
    randaoSBS,
    curSymbol,
    RandaoRedeemer (..),
    RandaoDatum (..),
  )
where

-- BLOCK0

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1, ScriptDataJsonSchema (ScriptDataJsonDetailedSchema), fromPlutusData, scriptDataToJson)
import Codec.Serialise
-- import Control.Monad (void)
-- import qualified Data.Aeson as JSON
import qualified Data.Aeson.Extras as JSON
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Ledger
import qualified Ledger.Ada as Ada
-- import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Playground.Contract
-- import Plutus.Contract as Contract
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class as BC
import PlutusTx.Prelude
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import  Plutus.V1.Ledger.Tx (ChainIndexTxOut)
-- import Prelude (Semigroup (..))
import qualified Prelude as Haskell

data ParamFilter = ParamFilter
  { ownerPkh :: PubKeyHash,
    threadToken :: !AssetClass, -- NFT
    identifyToken :: !AssetClass -- native token
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''ParamFilter
PlutusTx.makeLift ''ParamFilter

-- data DatumType = COMMIT | REVEAL
--   deriving (Show)

-- instance Eq DatumType where
--   COMMIT == COMMIT = True
--   REVEAL == REVEAL = True
--   _ == _ = False

-- PlutusTx.unstableMakeIsData ''DatumType
-- PlutusTx.makeLift ''DatumType

data RandaoDatum
  = MainDatum
      { 
        roundNumber :: Integer,
        contributorPkh :: PubKeyHash,
        startSeedHash :: BuiltinByteString,
        contributeNumber :: BuiltinByteString,
        revealNumber :: Integer,
        commitDeadline :: POSIXTime,
        revealDeadline :: POSIXTime,
        closeDeadline :: POSIXTime
      }
  | CommitDatum
      { roundNumber :: Integer,
        contributorPkh :: PubKeyHash,
        contributeHash :: BuiltinByteString
      }
  | ClaimRewardDatum
      { roundNumber :: Integer,
        contributorPkh :: PubKeyHash
      }
  | EndRoundDatum
      { roundNumber :: Integer,
        contributorPkh :: PubKeyHash,
        finalNumber :: BuiltinByteString,
        revealNumber :: Integer,
        amount :: Integer
      }
  | InfoRoundDatum
      { roundNumber :: Integer,
        contributorPkh :: PubKeyHash,
        finalNumber :: BuiltinByteString
      }
  deriving (Show)

PlutusTx.unstableMakeIsData ''RandaoDatum
-- use makeIsDataIndexed -> to specify indexing of the datum constructors
PlutusTx.makeLift ''RandaoDatum

-- check state
instance Eq RandaoDatum where
  MainDatum p1 p2 p3 p4 p5 p6 p7 p8 == MainDatum p1' p2' p3' p4' p5' p6' p7' p8' = p1 == p1' && p2 == p2' && p3 == p3' && p4 == p4' && p5 == p5' && p6 == p6' && p7 == p7' && p8 == p8'
  CommitDatum p1 p2 p3 == CommitDatum p1' p2' p3' = p1 == p1' && p2 == p2' && p3 == p3'
  ClaimRewardDatum p1 p2 == ClaimRewardDatum p1' p2' = p1 == p1' && p2 == p2'
  EndRoundDatum p1 p2 p3 p4 p5 == EndRoundDatum p1' p2' p3' p4' p5' = p1 == p1' && p2 == p2' && p3 == p3' && p4 == p4' && p5 == p5'
  InfoRoundDatum p1 p2 p3 == InfoRoundDatum p1' p2' p3' = p1 == p1' && p2 == p2' && p3 == p3'
  _ == _ = False

data RandaoRedeemer
  = Commit
      { roundNumberCommit :: Integer,
        contributorPkhCommit :: PubKeyHash,
        hashCommit :: BuiltinByteString
      }
  | Reveal
      { roundNumberReveal :: Integer,
        contributorPkhCommit :: PubKeyHash,
        randomNumber :: BuiltinByteString
      }
  | Close
      { roundNumberClose :: Integer,
        contributorPkhCommit :: PubKeyHash,
        oldSeed :: BuiltinByteString,
        newSeedHash :: BuiltinByteString,
        amountClose :: Integer ,
        commitNewDeadline :: POSIXTime,
        revealNewDeadline :: POSIXTime,
        closeDNeweadline :: POSIXTime
      }
  | Claim
      { roundNumberClaim :: Integer,
        contributorPkhCommit :: PubKeyHash
      }
  | Info
      { roundNumberInfo :: Integer,
        contributorPkhCommit :: PubKeyHash
      }
  deriving (Show)

PlutusTx.unstableMakeIsData ''RandaoRedeemer
-- use makeIsDataIndexed -> to specify indexing of the datum constructors
PlutusTx.makeLift ''RandaoRedeemer

lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

randaoDatum :: TxOut -> (Plutus.DatumHash -> Maybe Plutus.Datum) -> Maybe RandaoDatum
randaoDatum o f = do
  dh <- txOutDatum o
  Plutus.Datum d <- f dh
  PlutusTx.fromBuiltinData d

mkRandaoValidator :: ParamFilter -> RandaoDatum -> RandaoRedeemer -> ScriptContext -> Bool
mkRandaoValidator params dat red ctx =
  case (dat, red) of
    -- commit phase
    (MainDatum ran p2 p3 p4 p5 com revl cls, Commit rn sm ch) ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "wrong output main datum" (outputMainDatum == MainDatum ran p2 p3 p4 p5 com revl cls)
        && traceIfFalse "wrong output commit datum" (outputCommitDatum ownIdentOutputs == CommitDatum ran sm ch)
        && traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
        && traceIfFalse "wrong output identify token on main datum" (ownIdentInputAmountMainDatum == ownIdentOutputAmountMainDatum + 1)
        && traceIfFalse "wrong output identify token on commit datum" (ownIdentOutputAmountCommitDatum (txInfoOutputs info) == 1)
        && traceIfFalse "wrong available commit time" (before com (txInfoValidRange info))
        && traceIfFalse "exceed reveal time" (after revl (txInfoValidRange info))
    -- reveal phase
    (MainDatum ran p2 p3 cn p4 com revl cls, Reveal rn sm nu) ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "wrong output main datum" (outputMainDatum == MainDatum ran p2 p3 (sha2_256 (cn `appendByteString` nu)) (p4 + 1) com revl cls)
        && traceIfFalse "wrong output claim datum" (outputClaimRewardDatum (txInfoOutputs info) == ClaimRewardDatum ran sm)
        && traceIfFalse "wrong input commit datum" (outputCommitDatum (ownIdentUtxos ownIdentInputs) == CommitDatum ran sm (sha2_256 nu))
        && traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
        && traceIfFalse "wrong output identify token on main datum" (ownIdentInputAmountMainDatum == ownIdentOutputAmountMainDatum)
        && traceIfFalse "wrong output identify token on claim reward datum" (ownIdentOutputAmountClaimRewardDatum (txInfoOutputs info) == 1)
        && traceIfFalse "wrong available reveal time" (before revl (txInfoValidRange info))
        && traceIfFalse "exceed close time" (after cls (txInfoValidRange info))
    (CommitDatum ran p2 nuh, Reveal rn sm nu) ->
       traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "can not prove commitment number" (nuh == sha2_256 nu)
        && traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
    -- close and new round
    (MainDatum ran p2 sh cn p4 com revl cls, Close rn sm sd sdh amt com' revl' cls') ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "wrong output main datum" (outputMainDatum == MainDatum (ran + 1) sm sdh sdh 0 com' revl' cls')
        && traceIfFalse "wrong output close datum" (outputEndRoundDatum ownIdentOutputs == EndRoundDatum ran sm (sha2_256 (cn `appendByteString` sd)) p4 amt)
        && traceIfFalse "wrong output reward" (ownRewardOutputAmount == amt * p4 + 3000000)
        && traceIfFalse "wrong output info datum" (outputInfoRoundDatum ownIdentOutputs == InfoRoundDatum ran sm (sha2_256 (cn `appendByteString` sd)))
        && traceIfFalse "can not prove seed number" (sh == sha2_256 sd)
        && traceIfFalse "token missing from input" ownNftInput
        && traceIfFalse "token missing from output" ownNftOutput
        && traceIfFalse "wrong output identify token on main datum" (ownIdentInputAmountMainDatum == ownIdentOutputAmountMainDatum + 2)
        && traceIfFalse "wrong output identify token on end round datum" (ownIdentOutputAmountEndRoundDatum (txInfoOutputs info) == 1)
        && traceIfFalse "wrong output identify token on info round datum" (ownIdentOutputAmountInfoRoundDatum (txInfoOutputs info) == 1)
        && traceIfFalse "wrong available close time" (before cls (txInfoValidRange info))
    -- claim reward
    (EndRoundDatum ran p2 p3 p4 p5, Claim rn sm) ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "empty reward" (p4 - 1 >= 0)
        && traceIfFalse "wrong output end round datum" (outputEndRoundDatum ownIdentOutputs == EndRoundDatum ran p2 p3 (p4 - 1) p5) 
        && traceIfFalse "wrong output reward" (ownRewardOutputAmount == (p4 -1) * p5 + 3000000)
        && traceIfFalse "check return identify token for owner" checkReturnIdentTokenForOwner
        && traceIfFalse "only by claimer" (txSignedBy info (getClaimerPK (outputClaimRewardDatum (ownIdentUtxos ownIdentInputs))))
        && traceIfFalse "wrong output identify token on end round datum" (ownIdentOutputAmountEndRoundDatum (txInfoOutputs info) == 1)
    (ClaimRewardDatum ran p2, Claim rn sm) ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "check return identify token for owner" checkReturnIdentTokenForOwner
        && traceIfFalse "only by claimer" (txSignedBy info p2)
        && traceIfFalse "wrong output identify token on end round datum" (ownIdentOutputAmountEndRoundDatum (txInfoOutputs info) == 1)
    -- get randao random number for third party
    (InfoRoundDatum ran p2 p3, Info rn sm) ->
      traceIfFalse "wrong round number" (ran == rn)
        && traceIfFalse "wrong output info round datum" (outputInfoRoundDatum ownIdentOutputs == InfoRoundDatum ran p2 p3)
        && traceIfFalse "wrong output identify token on info round datum" (ownIdentOutputAmountInfoRoundDatum (txInfoOutputs info) == 1)
    _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

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

    -- find output main datum
    ownThreshOutput :: Maybe Ledger.TxOut
    ownThreshOutput = find (\x -> (isJust . txOutDatumHash) x && Value.assetClassValueOf (txOutValue x) (threadToken params) == 1) (getContinuingOutputs ctx)

    outputMainDatum :: RandaoDatum
    outputMainDatum = case ownThreshOutput of
      Nothing -> traceError "tx randao output datum not found datum hash"
      Just outDatum -> case randaoDatum outDatum (`findDatum` info) of
        Nothing -> traceError "tx randao output datum not found datum data"
        Just d -> d

    -- find output commit datum
    isCommitDatum :: RandaoDatum -> Bool
    isCommitDatum CommitDatum {} = True
    isCommitDatum _ = False

    checkCommitDatumType :: Ledger.TxOut -> Bool
    checkCommitDatumType txOut = maybe False isCommitDatum (randaoDatum txOut (`findDatum` info))

    outputCommitDatum :: [Ledger.TxOut] -> RandaoDatum
    outputCommitDatum [] = traceError "tx commit output datum not found datum data"
    outputCommitDatum (x : tl) =
      if checkCommitDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputCommitDatum tl
        else outputCommitDatum tl

    ownIdentOutputAmountCommitDatum :: [Ledger.TxOut] -> Integer
    ownIdentOutputAmountCommitDatum [] = 0
    ownIdentOutputAmountCommitDatum (x : tl) =
      if checkCommitDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> Value.assetClassValueOf (txOutValue x) (identifyToken params) 
          Nothing -> ownIdentOutputAmountCommitDatum tl
        else ownIdentOutputAmountCommitDatum tl

    -- find all of output of script
    ownIdentOutputs :: [Ledger.TxOut]
    ownIdentOutputs = filter (\x -> (isJust . txOutDatumHash) x && Value.assetClassValueOf (txOutValue x) (identifyToken params) == 1) (getContinuingOutputs ctx)

    -- count identity token input on main datum
    ownIdentInputAmountMainDatum :: Integer
    ownIdentInputAmountMainDatum = case find (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (threadToken params) == 1) (txInfoInputs info) of
      Nothing -> 0
      Just input -> Value.assetClassValueOf (txOutValue $ txInInfoResolved input) (identifyToken params)

    -- count identity token output on main datum
    ownIdentOutputAmountMainDatum :: Integer
    ownIdentOutputAmountMainDatum = case find (\x -> Value.assetClassValueOf (txOutValue x) (threadToken params) == 1) (getContinuingOutputs ctx) of
      Nothing -> 0
      Just output -> Value.assetClassValueOf (txOutValue output) (identifyToken params)

    -- find output end round datum
    isEndRoundDatum :: RandaoDatum -> Bool
    isEndRoundDatum EndRoundDatum {} = True
    isEndRoundDatum _ = False

    checkEndRoundDatumType :: Ledger.TxOut -> Bool
    checkEndRoundDatumType txOut = maybe False isEndRoundDatum (randaoDatum txOut (`findDatum` info))

    outputEndRoundDatum :: [Ledger.TxOut] -> RandaoDatum
    outputEndRoundDatum [] = traceError "tx commit output datum not found datum data"
    outputEndRoundDatum (x : tl) =
      if checkEndRoundDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputEndRoundDatum tl
        else outputEndRoundDatum tl

    ownIdentOutputAmountEndRoundDatum :: [Ledger.TxOut] -> Integer
    ownIdentOutputAmountEndRoundDatum [] = 0
    ownIdentOutputAmountEndRoundDatum (x : tl) =
      if checkEndRoundDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> Value.assetClassValueOf (txOutValue x) (identifyToken params) 
          Nothing -> ownIdentOutputAmountEndRoundDatum tl
        else ownIdentOutputAmountEndRoundDatum tl

    -- count amount reward output on end round
    ownRewardOutputAmount :: Integer
    ownRewardOutputAmount = case find (\x -> Value.assetClassValueOf (txOutValue x) (identifyToken params) == 1 && checkEndRoundDatumType x) (getContinuingOutputs ctx) of
      Nothing -> 0
      Just output -> lovelaces (txOutValue output)


    -- find output info round datum
    isInfoRoundDatum :: RandaoDatum -> Bool
    isInfoRoundDatum InfoRoundDatum {} = True
    isInfoRoundDatum _ = False

    checkInfoRoundDatumType :: Ledger.TxOut -> Bool
    checkInfoRoundDatumType txOut = maybe False isInfoRoundDatum (randaoDatum txOut (`findDatum` info))

    outputInfoRoundDatum :: [Ledger.TxOut] -> RandaoDatum
    outputInfoRoundDatum [] = traceError "tx commit output datum not found datum data"
    outputInfoRoundDatum (x : tl) =
      if checkInfoRoundDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputInfoRoundDatum tl
        else outputInfoRoundDatum tl

    ownIdentOutputAmountInfoRoundDatum :: [Ledger.TxOut] -> Integer
    ownIdentOutputAmountInfoRoundDatum [] = 0
    ownIdentOutputAmountInfoRoundDatum (x : tl) =
      if checkInfoRoundDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> Value.assetClassValueOf (txOutValue x) (identifyToken params) 
          Nothing -> ownIdentOutputAmountInfoRoundDatum tl
        else ownIdentOutputAmountInfoRoundDatum tl

    -- check enough ident token to refund
    checkReturnIdentTokenForOwner :: Bool
    checkReturnIdentTokenForOwner = case find (\x -> pubKeyOutput x == Just (ownerPkh params) && Value.assetClassValueOf (txOutValue x) (identifyToken params) == 1) (txInfoOutputs info) of
      Nothing -> False
      Just _ -> True

    getClaimerPK :: RandaoDatum -> PubKeyHash
    getClaimerPK (ClaimRewardDatum _ bp) = bp
    getClaimerPK _ = traceError "dont have owner pkh"

    -- find output claim reward datum
    isClaimRewardDatum :: RandaoDatum -> Bool
    isClaimRewardDatum ClaimRewardDatum {} = True
    isClaimRewardDatum _ = False

    checkClaimRewardDatumType :: Ledger.TxOut -> Bool
    checkClaimRewardDatumType txOut = maybe False isClaimRewardDatum (randaoDatum txOut (`findDatum` info))

    outputClaimRewardDatum :: [Ledger.TxOut] -> RandaoDatum
    outputClaimRewardDatum [] = traceError "tx commit output datum not found datum data"
    outputClaimRewardDatum (x : tl) =
      if checkClaimRewardDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> d
          Nothing -> outputClaimRewardDatum tl
        else outputClaimRewardDatum tl

    ownIdentOutputAmountClaimRewardDatum :: [Ledger.TxOut] -> Integer
    ownIdentOutputAmountClaimRewardDatum [] = 0
    ownIdentOutputAmountClaimRewardDatum (x : tl) =
      if checkClaimRewardDatumType x
        then case randaoDatum x (`findDatum` info) of
          Just d -> Value.assetClassValueOf (txOutValue x) (identifyToken params) 
          Nothing -> ownIdentOutputAmountClaimRewardDatum tl
        else ownIdentOutputAmountClaimRewardDatum tl

    -- find all of input of script
    ownIdentInputs :: [Ledger.TxInInfo]
    ownIdentInputs = filter (\x -> Value.assetClassValueOf (txOutValue $ txInInfoResolved x) (identifyToken params) == 1) (txInfoInputs info)

    ownIdentUtxos :: [Ledger.TxInInfo] -> [Ledger.TxOut]
    ownIdentUtxos arr = map txInInfoResolved arr

data RandaoType

instance Scripts.ValidatorTypes RandaoType where
  type DatumType RandaoType = RandaoDatum
  type RedeemerType RandaoType = RandaoRedeemer

typedRandaoValidator :: ParamFilter -> Scripts.TypedValidator RandaoType
typedRandaoValidator prams =
  Scripts.mkTypedValidator @RandaoType
    ( $$(PlutusTx.compile [||mkRandaoValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode prams
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @RandaoDatum @RandaoRedeemer

--- We should definitely separate those minting policies to different.

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
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

-- create identify token
mkIdentifyPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkIdentifyPolicy pkh _ ctx = txSignedBy (scriptContextTxInfo ctx) pkh

identifyPolicy :: PubKeyHash -> Plutus.MintingPolicy
identifyPolicy pkh =
  Plutus.mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkIdentifyPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

curIdentifySymbol :: PubKeyHash -> CurrencySymbol
curIdentifySymbol = scriptCurrencySymbol . identifyPolicy

randaoValidator :: ParamFilter -> Plutus.Validator
randaoValidator = Scripts.validatorScript . typedRandaoValidator

-- {-
--     As a Script
-- -}

randaoScript :: ParamFilter -> Plutus.Script
randaoScript = Plutus.unValidatorScript . randaoValidator

-- {-
--     As a Short Byte String
-- -}

randaoSBS :: ParamFilter -> SBS.ShortByteString
randaoSBS = SBS.toShort . LBS.toStrict . serialise . randaoScript

-- {-
--     As a Serialised Script
-- -}

randaoSerialised :: ParamFilter -> PlutusScript PlutusScriptV1 -- v2
randaoSerialised = PlutusScriptSerialised . randaoSBS

randaoAddress :: ParamFilter -> Ledger.Address
randaoAddress = scriptAddress . randaoValidator

randaoDatumOffChain :: Map TxOutRef ChainIndexTxOut -> Maybe RandaoDatum
randaoDatumOffChain o = do
  let txout = Map.elems o
  Plutus.Datum d <- either (const Nothing) Just ((_ciTxOutDatum . head) txout)
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