module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import qualified Cardano.Api as CAPI
-- import qualified PlutusCore as PCD
import Data.String          (IsString (..))
import qualified Data.ByteString.Base16 as B16
import System.Environment   (getArgs)
import CryptoToken          (policyV2, writeSerialisedScriptV2)
import           Ledger.Value                         as Value
import qualified Ledger.Address                       as LAD
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V1.Ledger.Contexts            as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx                             as PlutusTx
import qualified PlutusTx.Code                        as PTCD
import Data.ByteString.Lazy.Internal
import qualified         Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (pack, unpack)
import System.Environment
import Data.List
-- import qualified Data.Map as M (Map, empty, insert, lookup)
import Data.Char (ord)
import qualified CryptoToken as CT 
-- (toBBString, toBString, OwnerInfo, MyRedeemer, writeJSON)


main :: IO ()
main = do
    [file, wAddr, uName] <- getArgs
    let un   = CT.toBBString uName
        wa   = CT.toBBString  wAddr
        fwg  = CT.unsafePaymentPubKeyHash $ CT.unsafeReadAddress wAddr
        fwa  = LAD.unPaymentPubKeyHash $ CT.unsafePaymentPubKeyHash $ CT.unsafeReadAddress wAddr
        realDatum = CT.OwnerInfo {

            CT.walletAddress = fwa,
            CT.userName      = un
        }
    jsonFile <- CT.writeJSON file realDatum
    --     p    = policyV2 tn
    -- e <- writeSerialisedScriptV2 tn
    -- take integer as head
    -- take string as tail
    -- testFile <- writeFile (tail args) (head args)
    print "Everything worked, datum file constructed."
    print fwg
    -- -- testFile <- writeJSON fp nb
    return ()

dataToScriptData :: PlutusTx.Data -> CAPI.ScriptData
dataToScriptData (PlutusTx.Constr n xs) = CAPI.ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.Map xs)      = CAPI.ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs)     = CAPI.ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.I n)         = CAPI.ScriptDataNumber n
dataToScriptData (PlutusTx.B bs)        = CAPI.ScriptDataBytes bs