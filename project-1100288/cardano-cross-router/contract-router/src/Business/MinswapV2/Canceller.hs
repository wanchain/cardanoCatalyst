module Business.MinswapV2.Canceller where

import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import SingleValidator (PSmartHandleDatum (PAdvanced))
import "smart-handles" Utils (pand'List, pconvertChecked, pheadSingleton, presolveDatum)

import RouterConstants (ptreasuryAddress)
import Types
import Utils

import Business.MinswapV2 (PExtraOrderDatum (PEODInlineDatum))
import Business.MinswapV2 qualified as M
import Business.MinswapV2.Constants (pminswapAddress)

cancellerStakeValidator :: Term s PStakeValidator
cancellerStakeValidator = phoistAcyclic $ plam $ \redeemer ctx -> P.do
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  PRewarding _ <- pmatch ctxF.purpose
  infoF <- pletFields @'["inputs", "datums", "mint", "outputs"] ctxF.txInfo
  txInputs <- plet infoF.inputs
  txOuts <- plet infoF.outputs

  let minswapInput =
        pheadSingleton $
          pfilter
            # plam
              ( \i -> P.do
                  let pminswapAddress' = pfield @"address" #$ pfield @"resolved" # i
                  pminswapAddress #== pminswapAddress'
              )
            # pfromData txInputs
  minswapInputF <- pletFields @'["value", "datum"] (pfield @"resolved" # minswapInput)
  let minswapDatum = presolveDatum # minswapInputF.datum # infoF.datums
      orderDatum = pconvertChecked @M.POrderDatum (pto minswapDatum)
  PEODInlineDatum ((pfield @"hash" #) -> successDatumHashBS) <- pmatch (pfield @"successReceiverDatum" # orderDatum)
  treasuryOutputF <- pletFields @'["address", "value", "datum"] (phead # pfromData txOuts)
  POutputDatum _ <- pmatch treasuryOutputF.datum -- any datum is acceptable
  let resolvedAdvancedDatum = pconvertChecked @PSmartHandleDatum redeemer
      resolvedAdvancedDatumHashBS = phashData $ pdata redeemer
  PAdvanced ((pfield @"extraInfo" #) -> extraInfo) <- pmatch resolvedAdvancedDatum
  let resolvedRouterDatum = pconvertChecked @PRouterDatum (pto $ pfromData extraInfo)
      inTokenMode = pfield @"inTokenMode" # resolvedRouterDatum
      validations =
        pand'List
          [ ptraceIfFalse
              "Cancelled order must be produced at treasury"
              (treasuryOutputF.address #== ptreasuryAddress)
          , ptraceIfFalse
              "Treasury datum must be the resolved value of the hash under order's `successReceiver`"
              (successDatumHashBS #== resolvedAdvancedDatumHashBS)
          , pmatch inTokenMode $ \case
              PMint _ ->
                psingleAssetOfInputIsGettingBurntAndReproduced
                  minswapInputF.value
                  infoF.mint
                  treasuryOutputF.value
              PSend _ ->
                minswapInputF.value #== treasuryOutputF.value
          ]
  pif validations (popaque $ pconstant ()) perror
