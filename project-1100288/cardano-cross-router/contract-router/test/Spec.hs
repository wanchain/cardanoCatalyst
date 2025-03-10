module Main (main) where

import ContractRouterSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $ testGroup "contract-router" [ContractRouterSpec.tests]
