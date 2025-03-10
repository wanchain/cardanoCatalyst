{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.Environment (getArgs)

import Plutarch.Api.V2 (PAddress, scriptHash)
import Plutarch.Prelude
import Plutarch.Script (Script)

import BatchValidator qualified

import Business.MinswapV2 qualified as MinV2
import Business.MinswapV2.Canceller (cancellerStakeValidator)
import Business.MinswapV2.Constants (pminswapAddress)
import Cli.Compilation (compileTerm, targetCompilationDirectory)
import Cli.Utils qualified as Cli
import RouterIn qualified
import RouterOut qualified

mRouterOutPAddresses :: Maybe (ClosedTerm PAddress, ClosedTerm PAddress, Script, Script, Script, Script)
mRouterOutPAddresses =
  case compileTerm cancellerStakeValidator of
    Right cancellerStakingScript ->
      case (compileTerm RouterOut.psingleValidator, compileTerm RouterOut.pstakeValidator) of
        (Right routerOutSingleScript, Right routerOutStakingScript) ->
          case compileTerm (BatchValidator.smartHandleRouteValidatorW # Cli.scriptToPStakingCredential routerOutStakingScript) of
            Right routerOutBatchSpendScript ->
              let
                routerOutSinglePAddress :: ClosedTerm PAddress
                routerOutSinglePAddress = Cli.scriptToPAddress routerOutSingleScript
                routerOutBatchPAddress :: ClosedTerm PAddress
                routerOutBatchPAddress = Cli.scriptToPAddress routerOutBatchSpendScript
               in
                Just
                  ( routerOutSinglePAddress
                  , routerOutBatchPAddress
                  , cancellerStakingScript
                  , routerOutSingleScript
                  , routerOutBatchSpendScript
                  , routerOutStakingScript
                  )
            _ ->
              Nothing
        _ ->
          Nothing
    _ ->
      Nothing

main :: IO ()
main =
  let
    printError e = do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn e
      setSGR [Reset]
    mkFilePath fileName =
      "./"
        <> targetCompilationDirectory
        <> "/"
        <> fileName
        <> ".json"
   in
    do
      allArgs <- getArgs
      case allArgs of
        "minswap-v2" : _ -> do
          setSGR [SetColor Foreground Vivid Blue]
          putStrLn "Exporting Plutarch scripts..."
          setSGR [Reset]
          case mRouterOutPAddresses of
            Just (routerOutSinglePAddress, routerOutBatchPAddress, cancellerStakingScript, routerOutSingleScript, routerOutBatchSpendScript, routerOutStakingScript) ->
              case (compileTerm (RouterIn.psingleValidator # pconstant (scriptHash cancellerStakingScript) # MinV2.validateRouting # routerOutSinglePAddress # pminswapAddress), compileTerm (RouterIn.pstakeValidator # pconstant (scriptHash cancellerStakingScript) # MinV2.validateRouting # routerOutBatchPAddress # pminswapAddress)) of
                (Right routerInSingleScript, Right routerInStakingScript) ->
                  case compileTerm (BatchValidator.smartHandleRouteValidatorW # Cli.scriptToPStakingCredential routerInStakingScript) of
                    Right routerInBatchSpendScript -> do
                      ---- ROUTER IN ------------------------------------------
                      Cli.writeEvaluatedScript "RouterIn Single Spend" (mkFilePath "routerInSingleSpend") routerInSingleScript
                      putStrLn "Successfully exported RouterIn single spend validator"

                      Cli.writeEvaluatedScript "RouterIn Batch Spend Validator" (mkFilePath "routerInBatchSpend") routerInBatchSpendScript
                      putStrLn "Successfully exported RouterIn batch spend validator"

                      Cli.writeEvaluatedScript "RouterIn Staking Validator" (mkFilePath "routerInStaking") routerInStakingScript
                      putStrLn "Successfully exported RouterIn staking validator"

                      ---- ROUTER OUT -----------------------------------------
                      Cli.writeEvaluatedScript "RouterOut Single Spend" (mkFilePath "routerOutSingleSpend") routerOutSingleScript
                      putStrLn "Successfully exported RouterOut single spend validator"

                      Cli.writeEvaluatedScript "RouterOut Batch Spend Validator" (mkFilePath "routerOutBatchSpend") routerOutBatchSpendScript
                      putStrLn "Successfully exported RouterOut batch spend validator"

                      Cli.writeEvaluatedScript "RouterOut Staking Validator" (mkFilePath "routerOutStaking") routerOutStakingScript
                      putStrLn "Successfully exported RouterOut staking validator"

                      ---- CANCELLER ------------------------------------------
                      Cli.writeEvaluatedScript "Canceller Staking Validator" (mkFilePath "cancellerStaking") cancellerStakingScript
                      putStrLn "Successfully exported Canceller staking validator"

                      ---- SUCCESS MESSAGE ------------------------------------
                      setSGR [SetColor Foreground Vivid Green]
                      putStrLn "Done exporting Plutarch scripts, have a great day!"
                      setSGR [Reset]
                    _ ->
                      printError "Something went wrong evaluating RouterIn batch spend script"
                _ ->
                  printError "Something went wrong evaluating RouterIn scripts"
            _ ->
              printError "Something went wrong evaluating RouterOut script addresses"
        _ -> do
          setSGR [SetColor Foreground Vivid Red]
          putStrLn "Please specify business instance"
          setSGR [Reset]
