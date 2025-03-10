module RouterIn where

import Plutarch.Api.V1.Value (padaSymbol)
import Plutarch.Api.V2 (PAddress, PMaybeData (..), PScriptContext, PScriptHash, PStakeValidator)
import Plutarch.Monadic qualified as P
import Plutarch.Num (pnegate)
import Plutarch.Prelude
import "smart-handles" Utils (PCustomValidator, PRequiredMint (..), PTriple (..), pand'List, pconvertChecked, pgetSingleAssetApartFromADA)

import SingleValidator (PSmartHandleDatum (PAdvanced), PSmartHandleRedeemer, psmartHandleValidator)
import StakingValidator (smartHandleStakeValidatorW)
import Types
import Utils

validateFn ::
  forall (s :: S).
  Term s (PScriptHash :--> PBusinessValidator :--> PAddress :--> PCustomValidator)
validateFn = phoistAcyclic $ plam $ \cancellerScriptHash businessValidator routerOutAddr mOwner routerFee inputValue routerDatumData outputDatum forRouting ctx' -> P.do
  let txInfoField = pfield @"txInfo" # ctx'
      mintField = pfield @"mint" # txInfoField
      routerDatum = pconvertChecked @PRouterDatum routerDatumData
  routerDatumF <- pletFields @'["routerOutRouterFee", "inTokenSymbol", "inTokenTokenName", "inTokenMode", "outTokenSymbol", "outTokenTokenName", "outTokenMode", "businessSpecific"] routerDatum
  pif
    forRouting
    ( let
        inputTokenMatchesDatum's =
          pif
            (routerDatumF.inTokenSymbol #== padaSymbol)
            ( pmatch (pgetSingleAsset inputValue) $ \(PTriple {}) ->
                pconstant True
            )
            ( pmatch
                (pgetSingleAssetApartFromADA inputValue)
                $ \(PTriple inCS inTN _) ->
                  pand'List
                    [ inCS #== routerDatumF.inTokenSymbol
                    , inTN #== routerDatumF.inTokenTokenName
                    ]
            )
        routerOutRouteRequiredMint =
          pmatch routerDatumF.outTokenMode $ \case
            PMint _ ->
              pcon $
                PSingleton $
                  pdcons @"policy"
                    # routerDatumF.outTokenSymbol
                    #$ pdcons @"name"
                    # routerDatumF.outTokenTokenName
                    #$ pdnil
            PSend _ -> pcon $ PNone pdnil
        routerOutDatum =
          PAdvanced $
            pdcons @"mOwner"
              # pdata (pcon $ PDNothing pdnil)
              #$ pdcons @"routerFee"
              # routerDatumF.routerOutRouterFee
              #$ pdcons @"reclaimRouterFee"
              # pdata (pconstant 0)
              #$ pdcons @"routeRequiredMint"
              # pdata routerOutRouteRequiredMint
              #$ pdcons @"reclaimRequiredMint"
              # pdata (pcon $ PNone pdnil)
              #$ pdcons @"extraInfo"
              # pdata routerDatumData
              #$ pdnil
       in
        pand'List
          [ ptraceIfFalse
              "Datum's input token doesn't match the provided token"
              inputTokenMatchesDatum's
          , pmatch mOwner $ \case
              PDJust ((pfield @"_0" #) -> _) ->
                ptraceIfFalse
                  "Business validation failed"
                  ( businessValidator
                      # outputDatum
                      # cancellerScriptHash
                      # routerOutAddr
                      # pcon routerOutDatum
                      # inputValue
                      # routerFee
                      # routerDatum
                  )
              PDNothing _ -> pconstant False
          ]
    )
    ( pand'List
        [ ptraceIfFalse "Invalid output datum" (pdatumIsUnit outputDatum)
        , ptraceIfFalse
            "Input token must be burnt"
            ( pmatch routerDatumF.inTokenMode $ \case
                PMint _ ->
                  pmatch (pgetSingleAssetApartFromADA inputValue) $ \(PTriple inCS inTN inQty) ->
                    pmatch (pgetSingleAssetApartFromADA mintField) $ \(PTriple mintCS mintTN mintQty) ->
                      pand'List
                        [ inCS #== routerDatumF.inTokenSymbol
                        , inTN #== routerDatumF.inTokenTokenName
                        , inCS #== mintCS
                        , inTN #== mintTN
                        , inQty #== (pnegate # mintQty)
                        ]
                PSend _ -> pconstant True
            )
        ]
    )

psingleValidator :: ClosedTerm (PScriptHash :--> PBusinessValidator :--> PAddress :--> PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psingleValidator = plam $ \cancellerScriptHash businessValidator routerOutAddress routingAddress ->
  psmartHandleValidator # (validateFn # cancellerScriptHash # businessValidator # routerOutAddress) # routingAddress

pstakeValidator :: ClosedTerm (PScriptHash :--> PBusinessValidator :--> PAddress :--> PAddress :--> PStakeValidator)
pstakeValidator = plam $ \cancellerScriptHash businessValidator routerOutAddress routingAddress ->
  smartHandleStakeValidatorW # (validateFn # cancellerScriptHash # businessValidator # routerOutAddress) # routingAddress
