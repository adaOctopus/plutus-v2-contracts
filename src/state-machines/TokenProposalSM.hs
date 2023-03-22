{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
{-# LANGUAGE InstanceSigs #-}
-- You need to use all of these to get coverage


-----------------------------------------------------------------------------------------------------------------------
-- | A State Machine for users to cast votes based on Quadratic Formula for being AGAINST OR INFAVOR of CRYPTO PROJECTS
-----------------------------------------------------------------------------------------------------------------------

module TokenProposalSM where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import GHC.Generics (Generic)
import Ledger (Address, POSIXTime)
import Ledger.Address.Orphans ()
import Ledger.Tx.Constraints (TxConstraints)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (AsContractError (_ContractError), Contract, ContractError, Endpoint, Promise, endpoint,
                        selectList, type (.\/))
import Plutus.Contract.Secrets (SecretArgument, escape_sha2_256, extractSecret)
import Plutus.Contract.StateMachine (State (State, stateData, stateValue), Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V2.Scripts (MintingPolicyHash)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False, True), BuiltinByteString, Eq, Maybe (Just, Nothing), check, sha2_256, toBuiltin,
                         traceIfFalse, ($), (&&), (-), (.), (<$>), (<>), (==), (>>))

import Plutus.Contract.Test.Coverage.Analysis
import PlutusTx.Coverage
import Prelude qualified as Haskell
import GHC.Conc (ThreadStatus(ThreadRunning))

data TPParam = TPParam {

    tpParamLockAddress :: Address,
    -- ^ Payment address of the wallet locking some funds in exchange of vote credits
    tpParamLockAmount :: Haskell.Integer
--     tpParamStartTime :: POSIXTime
    -- ^ starting time of the TokenProposalMachine
} deriving (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''TPParam


-- Data types to be used in our StateMachines Input Actions
newtype TokenPolicyHash = TokenPolicyHash BuiltinByteString
   deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
   deriving stock (Haskell.Show, Generic)
   deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''TokenPolicyHash

-- ^ this is to identify the Project for Which someone is casting a vote
newtype InFavorVote     = InFavorVotes BuiltinByteString
  deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''InFavorVote


newtype AgainstVote     = AgainstVotes BuiltinByteString
  deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving stock (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''AgainstVote

data VoteChoice      = InFavorVote 
     | AgainstVote
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''VoteChoice
PlutusTx.makeLift ''VoteChoice

instance Eq VoteChoice where
     (==) :: VoteChoice -> VoteChoice -> Bool
     InFavorVote == InFavorVote  = True
     AgainstVote == AgainstVote  = True
     _ == _                      = False

-- All The Actions the SM accepts
data SMInputActions = 
     MintSMToken
     | GetCreditVotes Address Haskell.Integer
     ----------------- ^ Address to receive credit votes, and Integer is the amount of credits requested to be checked
     | CastTokenVote TokenPolicyHash VoteChoice Address Haskell.Integer Haskell.Integer
     ----------------- ^ 2 initial arguments are self-explanatory the 1st Integer is the amount of credit votes given 
     ------------------  the 2nd is the Amount for credits received initial
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)


PlutusTx.unstableMakeIsData ''SMInputActions
PlutusTx.makeLift ''SMInputActions     

-- The Possible States of the SM
data TPSMState = 
     RunSM MintingPolicyHash TokenName MintingPolicyHash TokenName
     -- ^ Only GetCrediitVotes action is allowed here (we pass hash and name of SMToken, and hash and name of The votecredit tokens)
     | LockedFundsGotCredits MintingPolicyHash Haskell.Integer Address
     -- ^ Only CastTokenVote action is allowed here
       ----------------------------------------- ^ this is the amount of ADA locked in Lovelace and the Address of the person who locked the funds
     | VoteHasBeenCasted MintingPolicyHash TokenPolicyHash VoteChoice Haskell.Integer Address
       ----------------------------------- ^Project the vote is for, The Vote Choice, The number of Votes, the Address of the Voter
     | Finished
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''TPSMState
PlutusTx.makeLift ''TPSMState

instance Eq TPSMState where
    {-# INLINABLE (==) #-}
    (RunSM mph tn mph1 tn1) == (RunSM mph' tn' mph1' tn1')                                    = (mph == mph') && (mph1 == mph1') && (tn1 == tn1') && (tn == tn') 
    (LockedFundsGotCredits mph hintr adr) == (LockedFundsGotCredits mph' hintr' adr')         = (mph == mph') && (hintr == hintr') && (adr == adr')
    (VoteHasBeenCasted mph tph vc hintr adr) == (VoteHasBeenCasted mph' tph' vc' hintr' adr') =  (mph == mph') && (tph == tph') && (vc == vc') && (hintr == hintr') && (adr == adr')
    Finished == Finished                                                                      = True
    _ == _                                                                                    = traceIfFalse "states not equal" False


-- | The token that represents the RIGHT TO CAST VOTES
newtype VotePermiToken = VotePermiToken { unVPToken :: Value }
    deriving newtype (Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''VotePermiToken
PlutusTx.makeLift ''VotePermiToken

permitToken :: MintingPolicyHash -> TokenName -> Value
permitToken mps tn = Value.singleton (Value.mpsSymbol mps) tn 1


-- | The token that represents the VOTE CREDITS
newtype VoteCreditToken = VoteCreditToken { unVCToken :: Value }
    deriving newtype (Eq, Haskell.Show)

PlutusTx.unstableMakeIsData ''VoteCreditToken
PlutusTx.makeLift ''VoteCreditToken

creditToken :: MintingPolicyHash -> TokenName -> Haskell.Integer -> Value
creditToken mps = Value.singleton (Value.mpsSymbol mps)


-- | Dividing lovelace with 1000000 to find exact ada
-- | We calculate the square root of that number 
-- | We round that number for simplicity. 
-- | The result number is the amount of VoteCredits
quadraticVoteRatio :: (Haskell.Floating a, Haskell.RealFrac a, Haskell.Integral b) => a -> b
quadraticVoteRatio adaFunds = Haskell.round . Haskell.sqrt $ (adaFunds Haskell./ 1000000)

-- Schema of the statemachine consisting of two endpoints with their parameters
-- We will update the parameters, is just for compiling for now
type TPPStateMachineSchema =
        Endpoint "lock" TPParam
        .\/ Endpoint "cast-vote" TPParam

-- | Error tracking data types
data TPError =
    TPContractError ContractError
    | TPSMError SM.SMContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''TPError

instance AsContractError TPError where
    _ContractError = _TPContractError . _ContractError

instance SM.AsSMContractError TPError where
    _SMContractError = _TPSMError . SM._SMContractError


-- The 'Token Proposal Param' parameter is not used in the validation. It is meant to
-- parameterize the script address depending based on the value of 'TPParam'.
{-# INLINABLE transitionFunction #-}
transitionFunction
    :: TPParam
    -> State TPSMState
    -> SMInputActions
    -> Maybe (TxConstraints Void Void, State TPSMState)
transitionFunction TPParam{tpParamLockAddress=adr,tpParamLockAmount=adaV} State{stateData=oldStateData, stateValue=oldStateValue} input = case (oldStateData, input) of 
     (RunSM mph tn mph1 tn1, MintSMToken) -> 
          let constraints = Constraints.mustMintCurrency mph tn 1 
                         <> Constraints.mustMintCurrency mph1 tn1 amountToMint
                         <> Constraints.mustPayToAddress adr (creditToken mph1 tn1 amountToMint) in
               Just (
                    constraints,
                    State {
                         stateData  = LockedFundsGotCredits mph adaV adr,
                         stateValue = Ada.lovelaceValueOf adaV
                    }
               )
          where 
               amountToMint = quadraticVoteRatio . Haskell.fromIntegral $ adaV
     -- | Think of the next step how it should be, and all the possible states.
     _                          -> Nothing


-- | Boiler Plate code for SMs to get machine, mkValidator, and get the typedValidator
--------------------------------------------------------------------------------------
type TokenProposalStateMachine = SM.StateMachine TPSMState SMInputActions

{-# INLINABLE machineF #-}
machineF :: TPParam -> TokenProposalStateMachine
machineF tpParam = SM.mkStateMachine Nothing (transitionFunction tpParam) isFinal where
    isFinal Finished = True
    isFinal _        = False


{-# INLINABLE mkValidator #-}
mkValidator :: TPParam -> V2.ValidatorType TokenProposalStateMachine
mkValidator tpParam = SM.mkValidator (machineF tpParam)

typedValidator :: TPParam -> V2.TypedValidator TokenProposalStateMachine
typedValidator = V2.mkTypedValidatorParam @TokenProposalStateMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

mintingPolicy :: TPParam -> Scripts.MintingPolicy
mintingPolicy gp = Scripts.forwardingMintingPolicy $ typedValidator gp

client :: TPParam -> SM.StateMachineClient TPSMState SMInputActions
client tpp = SM.mkStateMachineClient $ SM.StateMachineInstance (machineF tpp) $ typedValidator tpp

-- | THis is for test coverage (i do not understand it yet, but included it anyways because, why not make things even more complicated? :P)
covIdx :: CoverageIndex
covIdx = $refinedCoverageIndex $$(PlutusTx.compile [|| \a b c d -> check (mkValidator a b c d) ||])


-- | This is the actual functionality of the points
-- | Starting with locking funds in exchange for vote credits