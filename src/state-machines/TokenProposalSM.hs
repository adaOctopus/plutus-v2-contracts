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
    tpParamStartTime :: POSIXTime
    -- ^ starting time of the TokenProposalMachine
} deriving (Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''TPParam


-- Data types to be used in our StateMachines Input Actions
type TokenPolicyHash = BuiltinByteString
-- ^ this is to identify the Project for Which someone is casting a vote
type InFavorVote     = BuiltinByteString
type AgainstVote     = BuiltinByteString
data VoteChoice      = InFavorVote 
     | AgainstVote
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

instance Eq VoteChoice where
     InFavorVote == InFavorVote  = True
     AgainstVote == AgainstVote  = True
     _ == _                      = False

-- All The Actions the SM accepts
data SMInputActions = InstantiateSM 
     | GetCreditVotes Address Haskell.Integer
     ----------------- ^ Address to receive credit votes, and Integer is the amount of credits requested to be checked
     | CastTokenVote TokenPolicyHash VoteChoice Address Haskell.Integer Haskell.Integer
     ----------------- ^ 2 initial arguments are self-explanatory the 1st Integer is the amount of credit votes given 
     ------------------  the 2nd is the Amount for credits received initial
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

-- The Possible States of the SM
data TPSMState = 
     Running
     -- ^ Only GetCrediitVotes action is allowed here
     | DistributedVoteCredits MintingPolicyHash Haskell.Integer Address
     -- ^ Only CastTokenVote action is allowed here
       ----------------------------------------- ^ this is the amount of ADA locked in Lovelace and the Address of the person who locked the funds
     | VoteHasBeenCasted MintingPolicyHash TokenPolicyHash VoteChoice Haskell.Integer Address
       ----------------------------------- ^Project the vote is for, The Vote Choice, The number of Votes, the Address of the Voter
     | Finished
     deriving stock (Haskell.Show, Generic)
     deriving anyclass (ToJSON, FromJSON)

instance Eq TPSMState where
    {-# INLINABLE (==) #-}
    Running == Running                                                                        = True 
    (DistributedVoteCredits mph hintr adr) == (DistributedVoteCredits mph' hintr' adr')       = (mph == mph') && (hintr == hintr') && (adr == adr')
    (VoteHasBeenCasted mph tph vc hintr adr) == (VoteHasBeenCasted mph' tph' vc' hintr' adr') =  (mph == mph') && (tph == tph') && (vc == vc') && (hintr == hintr') && (adr == adr')
    Finished == Finished                                                                      = True
    _ == _                                                                                    = traceIfFalse "states not equal" False


-- | The token that represents the right to cast votes
newtype VotePermiToken = VotePermiToken { unVPToken :: Value }
    deriving newtype (Eq, Haskell.Show)

-- Schema of the statemachine consisting of two endpoints with their parameters
-- We will update the parameters, is just for compiling for now
type TPPStateMachineSchema =
        Endpoint "lock" TPParam
        .\/ Endpoint "cast-vote" TPParam