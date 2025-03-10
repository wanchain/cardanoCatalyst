module Cli.Compilation (compileTerm, targetCompilationDirectory) where

import Data.Text (Text)

import Plutarch (ClosedTerm, Config (..), PType, Script, TracingMode (..), compile)

compileTerm :: forall {a :: PType}. ClosedTerm a -> Either Text Script
compileTerm = compile (Config DoTracing)

targetCompilationDirectory :: FilePath
targetCompilationDirectory = "compiled-testnet"
