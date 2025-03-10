module ContractRouterSpec (tests) where

import Data.Either (fromRight)

import PlutusLedgerApi.V1 (Address (Address), Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import PlutusLedgerApi.V1.Value (CurrencySymbol, TokenName)
import PlutusLedgerApi.V2 (ScriptContext, ScriptPurpose (Rewarding, Spending), TxId (TxId), TxOutRef (TxOutRef), adaSymbol, adaToken, singleton)
import PlutusTx (toBuiltinData, toData)
import PlutusTx.Builtins (blake2b_256, serialiseData)

import Plutarch
import Plutarch.Api.V1.Value (padaSymbol, padaToken)
import Plutarch.Api.V2 (PAddress, PCurrencySymbol (PCurrencySymbol), PStakeValidator, PStakingCredential, PTokenName (PTokenName), scriptHash)
import Plutarch.Context (Builder, address, buildRewarding', extraRedeemer, input, output, script, withDatum, withRedeemer, withRef, withValue, withdrawal)
import Plutarch.Prelude
import Plutarch.Test.Precompiled (tryFromPTerm, (@!>), (@>))

import Test.Tasty (TestTree, testGroup)

import BatchValidator (SmartRedeemer (..), smartHandleRouteValidatorW)
import Cli.Utils qualified as Cli
import Compilation (compileTerm)
import RouterConstants (ptreasuryAddress)
import SingleValidator (SmartHandleDatum (..))
import StakingValidator (RouterRedeemer (..))
import "smart-handles" Utils (RequiredMint (None))

import Business.MinswapV2 qualified as MinV2
import Business.MinswapV2.Canceller qualified as Canceller
import Business.MinswapV2.Constants (pbatcherFee, pdeposit, pminswapAddress, pminswapV2LPSymbol)
import Business.MinswapV2.Utils (pcomputeLPAssetName, plovelacesAfterFees)
import RouterIn qualified
import RouterOut qualified
import Types (RouterDatum (RouterDatum), TokenType (Send))

tests :: TestTree
tests = testGroup "Contract Router" [routerInTests, routerOutTests]

alice :: Credential
alice = PubKeyCredential "86ae9eebd8b97944a45201e4aec1330a72291af2d071644bba015959"

batchRedeemer :: RouterRedeemer
batchRedeemer =
  RouterRedeemer
    { inputIdxs = [0]
    , outputIdxs = [0]
    }

routerOutStakingScript :: Script
routerOutStakingScript =
  fromRight undefined $ compileTerm RouterOut.pstakeValidator

routerOutPStakingCredential :: Term s PStakingCredential
routerOutPStakingCredential =
  Cli.scriptToPStakingCredential routerOutStakingScript

routerOutBatchSpendScript :: Script
routerOutBatchSpendScript =
  fromRight undefined $
    compileTerm $
      smartHandleRouteValidatorW # routerOutPStakingCredential

routerOutBatchPAddress :: Term s PAddress
routerOutBatchPAddress = Cli.scriptToPAddress routerOutBatchSpendScript

routerOutStakingCredential :: StakingCredential
routerOutStakingCredential =
  StakingHash $ ScriptCredential $ scriptHash routerOutStakingScript

cancellerStakingScript :: Script
cancellerStakingScript =
  fromRight undefined $ compileTerm Canceller.cancellerStakeValidator

prouterInStakeValidator :: ClosedTerm PStakeValidator
prouterInStakeValidator =
  RouterIn.pstakeValidator
    # pconstant (scriptHash cancellerStakingScript)
    # MinV2.validateRouting
    # routerOutBatchPAddress
    # pminswapAddress

routerInStakingScript :: Script
routerInStakingScript =
  fromRight undefined $ compileTerm prouterInStakeValidator

routerInPStakingCredential :: Term s PStakingCredential
routerInPStakingCredential =
  Cli.scriptToPStakingCredential routerInStakingScript

routerInBatchSpendScript :: Script
routerInBatchSpendScript =
  fromRight undefined $
    compileTerm $
      smartHandleRouteValidatorW # routerInPStakingCredential

routerInStakingCredential :: StakingCredential
routerInStakingCredential =
  StakingHash $ ScriptCredential $ scriptHash routerInStakingScript

minSymbol :: CurrencySymbol
minSymbol = "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72"

pminSymbol :: ClosedTerm PCurrencySymbol
pminSymbol = pcon $ PCurrencySymbol $ phexByteStr "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72"

minToken :: TokenName
minToken = "MIN"

pminToken :: ClosedTerm PTokenName
pminToken = pcon $ PTokenName $ phexByteStr "4d494e"

-- minAssetClass :: AssetClass
-- minAssetClass = assetClass minSymbol minToken

routerInRouterFee :: Integer
routerInRouterFee = 1_000_000

routerInReclaimFee :: Integer
routerInReclaimFee = 500_000

inputTxOutRef :: TxOutRef
inputTxOutRef = TxOutRef (TxId "0000000000000000") 0

aToBDirection :: Bool
aToBDirection = True

inLovelaces :: Integer
inLovelaces = 10_000_000

{- | From 10 ADA, 1 goes to the routing agent, 2 goes to the batcher, 2 is
reserved for deposit, 1 should go to the second router. Therefore, at 0.05
ADA per MIN, it'll be 80 MIN for 4 ADA.
-}
minimumReceive :: Integer
minimumReceive = 80_000_000

killable :: Bool
killable = False

routerDatum :: RouterDatum
routerDatum =
  RouterDatum
    "00"
    "01"
    "02"
    "03"
    routerOutRouterFee
    adaSymbol
    adaToken
    Send
    minSymbol
    minToken
    Send
    (toBuiltinData $ MinV2.MinswapRequest aToBDirection minimumReceive killable)

routerOutRouterFee :: Integer
routerOutRouterFee = 1_000_000

routerOutDatum :: SmartHandleDatum
routerOutDatum =
  Advanced Nothing routerOutRouterFee 0 None None (toBuiltinData routerDatum)

minswapDatum :: MinV2.OrderDatum
minswapDatum =
  MinV2.OrderDatum
    { canceller = MinV2.OAMWithdrawScript (scriptHash cancellerStakingScript)
    , refundReceiver = plift ptreasuryAddress
    , refundReceiverDatum =
        MinV2.EODInlineDatum (blake2b_256 $ serialiseData $ toBuiltinData ())
    , successReceiver = plift routerOutBatchPAddress
    , successReceiverDatum =
        MinV2.EODInlineDatum
          (blake2b_256 $ serialiseData $ toBuiltinData routerOutDatum)
    , lpAsset =
        MinV2.Asset
          (plift pminswapV2LPSymbol)
          (plift $ pcomputeLPAssetName padaSymbol padaToken pminSymbol pminToken)
    , step =
        MinV2.SwapExactIn
          aToBDirection
          ( MinV2.SAOSpecificAmount $
              plift $
                plovelacesAfterFees
                  # pconstant inLovelaces
                  # pconstant routerInRouterFee
                  # pconstant routerOutRouterFee
          )
          minimumReceive
          killable
    , maxBatcherFee = plift pbatcherFee
    , expirySettingOpt = Nothing
    }

routerInInput :: (Builder a) => Bool -> a
routerInInput forRoute =
  input $
    mconcat
      [ script $ scriptHash routerInBatchSpendScript
      , withRef inputTxOutRef
      , withValue (singleton adaSymbol adaToken inLovelaces)
      , withRedeemer (if forRoute then RouteSmart else ReclaimSmart)
      , withDatum $
          Advanced
            (Just $ plift ptreasuryAddress)
            routerInRouterFee
            routerInReclaimFee
            None
            None
            (toBuiltinData routerDatum)
      ]

minswapOutput :: (Builder a) => a
minswapOutput =
  output $
    mconcat
      [ address $ plift pminswapAddress
      , withValue (singleton adaSymbol adaToken (inLovelaces - routerInRouterFee))
      , withDatum minswapDatum
      ]

treasuryOutput :: (Builder a) => Integer -> a
treasuryOutput lovelaceCount =
  output $
    mconcat
      [ address $ plift ptreasuryAddress
      , withValue $ singleton adaSymbol adaToken lovelaceCount
      , withDatum ()
      ]

aliceOutput :: (Builder a) => Integer -> a
aliceOutput lovelaceCount =
  output $
    mconcat
      [ address $ Address alice Nothing
      , withValue $ singleton adaSymbol adaToken lovelaceCount
      , withDatum ()
      ]

batcherOutLovelaces :: Integer
batcherOutLovelaces =
  plift $
    ( plovelacesAfterFees
        # pconstant inLovelaces
        # pconstant routerInRouterFee
        # pconstant routerOutRouterFee
    )
      + pdeposit

routerOutInput :: (Builder a) => a
routerOutInput =
  input $
    mconcat
      [ script $ scriptHash routerOutBatchSpendScript
      , withRef inputTxOutRef
      , withValue (singleton adaSymbol adaToken batcherOutLovelaces)
      , withRedeemer RouteSmart
      , withDatum $
          Advanced
            Nothing
            routerOutRouterFee
            0
            None
            None
            (toBuiltinData routerDatum)
      ]

fromRouterInToMinswapContext :: RouterRedeemer -> ScriptContext
fromRouterInToMinswapContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerInStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) RouteSmart
      , withdrawal routerInStakingCredential 0
      , routerInInput True
      , minswapOutput
      ]

reclaimFromRouterInContext :: RouterRedeemer -> ScriptContext
reclaimFromRouterInContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerInStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) ReclaimSmart
      , withdrawal routerInStakingCredential 0
      , routerInInput False
      , treasuryOutput $ inLovelaces - routerInReclaimFee
      ]

routeFromRouterInToAliceContext :: RouterRedeemer -> ScriptContext
routeFromRouterInToAliceContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerInStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) RouteSmart
      , withdrawal routerInStakingCredential 0
      , routerInInput True
      , aliceOutput $ inLovelaces - routerInRouterFee
      ]

reclaimFromRouterInToAliceContext :: RouterRedeemer -> ScriptContext
reclaimFromRouterInToAliceContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerInStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) ReclaimSmart
      , withdrawal routerInStakingCredential 0
      , routerInInput False
      , aliceOutput $ inLovelaces - routerInReclaimFee
      ]

fromRouterOutToTreasuryContext :: RouterRedeemer -> ScriptContext
fromRouterOutToTreasuryContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerOutStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) RouteSmart
      , withdrawal routerOutStakingCredential 0
      , routerOutInput
      , treasuryOutput $ batcherOutLovelaces - routerOutRouterFee
      ]

routeFromRouterOutToAliceContext :: RouterRedeemer -> ScriptContext
routeFromRouterOutToAliceContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerOutStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) RouteSmart
      , withdrawal routerOutStakingCredential 0
      , routerOutInput
      , aliceOutput $ batcherOutLovelaces - routerOutRouterFee
      ]

reclaimFromRouterOutToAliceContext :: RouterRedeemer -> ScriptContext
reclaimFromRouterOutToAliceContext withdrawRedeemer =
  buildRewarding' $
    mconcat
      [ extraRedeemer (Rewarding routerOutStakingCredential) withdrawRedeemer
      , extraRedeemer (Spending inputTxOutRef) ReclaimSmart
      , withdrawal routerOutStakingCredential 0
      , routerOutInput
      , aliceOutput batcherOutLovelaces
      ]

routerInTests :: TestTree
routerInTests = tryFromPTerm "RouterIn Staking Validator" prouterInStakeValidator $ do
  [toData batchRedeemer, toData (fromRouterInToMinswapContext batchRedeemer)] @> "✅ RouterIn ---Route---> Minswap"
  [toData batchRedeemer, toData (reclaimFromRouterInContext batchRedeemer)] @> "✅ RouterIn --Reclaim--> Treasury"
  [toData batchRedeemer, toData (routeFromRouterInToAliceContext batchRedeemer)] @!> "❌ RouterIn ---Route---> Alice"
  [toData batchRedeemer, toData (reclaimFromRouterInToAliceContext batchRedeemer)] @!> "❌ RouterIn --Reclaim--> Alice"

routerOutTests :: TestTree
routerOutTests = tryFromPTerm "RouterOut Staking Validator" RouterOut.pstakeValidator $ do
  [toData batchRedeemer, toData (fromRouterOutToTreasuryContext batchRedeemer)] @> "✅ RouterOut ---Route---> Treasury"
  [toData batchRedeemer, toData (routeFromRouterOutToAliceContext batchRedeemer)] @!> "❌ RouterOut ---Route---> Alice"
  [toData batchRedeemer, toData (reclaimFromRouterOutToAliceContext batchRedeemer)] @!> "❌ RouterOut --Reclaim--> Alice"
