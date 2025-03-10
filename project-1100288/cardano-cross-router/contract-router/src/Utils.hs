{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils where

import Plutarch.Api.V1 (AmountGuarantees (..), KeyGuarantees (..), PDatum)
import Plutarch.Api.V1.Value (PCurrencySymbol, PTokenName, PValue (..))
import Plutarch.Api.V2 (PDatumHash (PDatumHash), PMaybeData (..), PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Builtin (PIsData (pdataImpl), pasByteStr, pforgetData, pserialiseData)
import Plutarch.Crypto (pblake2b_256)
import Plutarch.DataRepr
import Plutarch.Monadic qualified as P
import Plutarch.Num (pnegate)
import Plutarch.Prelude
import Specialized.Minswap ()
import "smart-handles" Utils (PTriple (..), pand'List, pgetSingleAssetApartFromADA, pheadSingleton, presolveValueToList, psingleAssetToTriple)

data PQuad (a :: PType) (b :: PType) (c :: PType) (d :: PType) (s :: S)
  = PQuad (Term s a) (Term s b) (Term s c) (Term s d)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PQuad a b c d) where type DPTStrat _ = PlutusTypeScott

pdatumIsTagOfInput :: Term s PTxOutRef -> Term s PDatum -> Term s PBool
pdatumIsTagOfInput outRef datum =
  let inputTag = pblake2b_256 # (pserialiseData # pdataImpl outRef)
      outputDatumBS = pasByteStr # pto datum
   in outputDatumBS #== inputTag

phashData :: Term s (PAsData a) -> Term s PByteString
phashData dat = pblake2b_256 # (pserialiseData # pforgetData dat)

-- | Helper function for wrapping a `PDatumHash` under `PMaybeData`.
pdatumHashToPDJust :: Term s (PAsData PDatumHash) -> Term s (PMaybeData (PAsData PDatumHash))
pdatumHashToPDJust d = pcon $ PDJust $ pdcons # pdata d # pdnil

{- | Hashes any `PIsData` such that it can be used as `PDatumHash` and wraps it
under a `PMaybeData`.
-}
hashData :: (PIsData a) => Term s a -> Term s (PMaybeData (PAsData PDatumHash))
hashData d =
  pdatumHashToPDJust $ pdata $ pcon $ PDatumHash $ pblake2b_256 # (pserialiseData # pdataImpl d)

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

-- | Grabs the singular asset in a given `PValue`
pgetSingleAsset ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees)
    (s :: S).
  Term s (PValue anyOrder anyAmount) ->
  Term s (PTriple PCurrencySymbol PTokenName PInteger)
pgetSingleAsset v =
  pelimList
    ( \h t ->
        pelimList
          (\_ _ -> ptraceError "More than one asset was found")
          (psingleAssetToTriple # h)
          t
    )
    (ptraceError "No assets found")
    (presolveValueToList v)

{- | Grabs the singular asset in a given `PValue`, along with the Lovelace
quantity. NOTE that it does NOT check the first asset to be ADA.
-}
pgetLovelaceCountAndSingleAssetApartFromADA ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees)
    (s :: S).
  Term s (PValue anyOrder anyAmount) ->
  Term s (PQuad PInteger PCurrencySymbol PTokenName PInteger)
pgetLovelaceCountAndSingleAssetApartFromADA v =
  pelimList
    ( \ada otherAssets ->
        pmatch (psingleAssetToTriple # ada) $ \(PTriple _ _ lovelaceCount) ->
          pmatch (psingleAssetToTriple # pheadSingleton otherAssets) $ \(PTriple cs tn qty) ->
            pcon $ PQuad lovelaceCount cs tn qty
    )
    (ptraceError "No assets found")
    (presolveValueToList v)

{- | Given the `PValue` from a UTxO and the `PValue` from the mint field, this
function checks to see if the input value has a single asset (apart from
ADA), and that it's also included as a single asset in the mint value with
a negative quantity (i.e. is getting burnt).
-}
psingleAssetOfInputIsGettingBurnt ::
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
psingleAssetOfInputIsGettingBurnt inputVal mintVal =
  let
    inputTriple = pgetSingleAssetApartFromADA inputVal
    mintAsset = pgetSingleAssetApartFromADA mintVal
   in
    pmatch mintAsset $ \(PTriple mintCS mintTN mintQty) ->
      pmatch inputTriple $ \(PTriple inCS inTN inQty) ->
        pand'List
          [ inCS #== mintCS
          , inTN #== mintTN
          , inQty #== (pnegate # mintQty)
          ]

{- | Similar to `psingleAssetOfInputIsGettingBurnt`, but it also validates the
given output contains all the values from input, apart from the ones getting
burnt.
-}
psingleAssetOfInputIsGettingBurntAndReproduced ::
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s PBool
psingleAssetOfInputIsGettingBurntAndReproduced inputVal mintVal outputVal =
  let
    inputTriple = pgetLovelaceCountAndSingleAssetApartFromADA inputVal
    mintAsset = pgetSingleAssetApartFromADA mintVal
    outputTriple = pgetSingleAsset outputVal
   in
    P.do
      PQuad inLovelaces inCS inTN inQty <- pmatch inputTriple
      PTriple mintCS mintTN mintQty <- pmatch mintAsset
      PTriple _ _ outLovelaces <- pmatch outputTriple
      pand'List
        [ inCS #== mintCS
        , inTN #== mintTN
        , inQty #== (pnegate # mintQty)
        , inLovelaces #== outLovelaces
        ]

pdatumIsUnit :: forall (s :: S). Term s PDatum -> Term s PBool
pdatumIsUnit d =
  pforgetData (pconstantData ()) #== pto d
