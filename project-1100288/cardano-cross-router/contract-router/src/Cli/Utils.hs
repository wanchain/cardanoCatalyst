module Cli.Utils where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as Text

import Cardano.Binary qualified as CBOR
import PlutusLedgerApi.V2 (Data, ExBudget)

import Plutarch.Api.V1 (PCredential (PScriptCredential))
import Plutarch.Api.V2 (PAddress (PAddress), PMaybeData (..), PStakingCredential (PStakingHash), scriptHash)
import Plutarch.Evaluate (evalScript)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import "liqwid-plutarch-extra" Plutarch.Extra.Script (applyArguments)

import Compilation

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compileTerm x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

{- | Given a `Script`, this function returns a Plutarch level `PAddress` without
a staking part.
-}
scriptToPAddress :: Script -> Term s PAddress
scriptToPAddress script =
  let
    phash = pconstant $ scriptHash script
    pscriptCred = pcon $ PScriptCredential $ pdcons # pdata phash #$ pdnil
   in
    pcon $
      PAddress $
        pdcons @"credential"
          # pdata pscriptCred
          #$ pdcons @"stakingCredential"
          # pdata (pcon $ PDNothing pdnil)
          #$ pdnil

{- | Given a `Script`, this function returns a Plutarch level
`PStakingCredential`.
-}
scriptToPStakingCredential :: forall (s :: S). Script -> Term s PStakingCredential
scriptToPStakingCredential script =
  let
    phash = pconstant $ scriptHash script
    pscriptCred = pcon $ PScriptCredential $ pdcons # pdata phash #$ pdnil
   in
    pcon $ PStakingHash $ pdcons # pdata pscriptCred #$ pdnil

writeEvaluatedScript :: String -> FilePath -> Script -> IO ()
writeEvaluatedScript title filepath script = do
  let scriptType = "PlutusScriptV2" :: String
      plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
      content = encodePretty plutusJson
  putStrLn $ "Exporting to: " <> filepath
  LBS.writeFile filepath content
