module RouterOut where

import Plutarch.Api.V2 (PMaybeData (..), PScriptContext, PStakeValidator)
import Plutarch.Prelude
import "smart-handles" Utils (PCustomValidator, pand'List, pconvertChecked)

import RouterConstants (ptreasuryAddress)
import SingleValidator (PSmartHandleDatum, PSmartHandleRedeemer, psmartHandleValidator)
import StakingValidator (smartHandleStakeValidatorW)
import Types
import Utils

validateFn :: forall (s :: S). Term s PCustomValidator
validateFn = phoistAcyclic $ plam $ \mOwner _routerFee inputValue extraInfoData outputDatum forRouting ctx' ->
  let
    mintField = pfield @"mint" # (pfield @"txInfo" # ctx')
    routerDatum = pconvertChecked @PRouterDatum extraInfoData
    outTokenMode = pfield @"outTokenMode" # routerDatum
   in
    pif
      forRouting
      ( pand'List
          [ ptraceIfFalse "Invalid output datum" (pdatumIsUnit outputDatum)
          , pmatch mOwner $ \case
              PDJust _ -> pconstant False
              PDNothing _ -> pconstant True
          , pmatch outTokenMode $ \case
              PMint _ ->
                psingleAssetOfInputIsGettingBurnt inputValue mintField
              PSend _ ->
                pconstant True
          ]
      )
      perror

psingleValidator :: Term s (PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psingleValidator = psmartHandleValidator # validateFn # ptreasuryAddress

pstakeValidator :: Term s PStakeValidator
pstakeValidator = smartHandleStakeValidatorW # validateFn # ptreasuryAddress
