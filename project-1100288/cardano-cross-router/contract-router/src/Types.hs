module Types where

import PlutusLedgerApi.V2 (BuiltinByteString, BuiltinData, CurrencySymbol, TokenName)
import PlutusTx qualified

import Plutarch.Api.V1.Value (PValue)
import Plutarch.Api.V2 (AmountGuarantees (Positive), KeyGuarantees (Sorted), PAddress, PCurrencySymbol, PDatum, PTokenName, PScriptHash)
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Prelude

import SingleValidator (PSmartHandleDatum)

data TokenType
  = Mint -- for tokens that are mapped from the evm chain
  | Send -- for cardano native tokens

PlutusTx.makeLift ''TokenType
PlutusTx.makeIsDataIndexed
  ''TokenType
  [ ('Mint, 0)
  , ('Send, 1)
  ]

data PTokenType (s :: S)
  = PMint (Term s (PDataRecord '[]))
  | PSend (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PTokenType where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PTokenType

instance PUnsafeLiftDecl PTokenType where type PLifted PTokenType = TokenType
deriving via (DerivePConstantViaData TokenType PTokenType) instance PConstantDecl TokenType

--------------------------------------------------------------------------------

data RouterDatum = RouterDatum
  { uniqueId :: BuiltinByteString
  , inPairId :: BuiltinByteString
  , outPairId :: BuiltinByteString
  , evmReceiver :: BuiltinByteString
  , routerOutRouterFee :: Integer
  , inTokenSymbol :: CurrencySymbol
  , inTokenTokenName :: TokenName
  , inTokenMode :: TokenType
  , outTokenSymbol :: CurrencySymbol
  , outTokenTokenName :: TokenName
  , outTokenMode :: TokenType
  , businessSpecific :: BuiltinData
  }

PlutusTx.makeLift ''RouterDatum
PlutusTx.makeIsDataIndexed ''RouterDatum [('RouterDatum, 0)]

data PRouterDatum (s :: S)
  = PRouterDatum
      ( Term
          s
          ( PDataRecord
              '[ "uniqueId" ':= PByteString
               , "inPairId" ':= PByteString
               , "outPairId" ':= PByteString
               , "evmReceiver" ':= PByteString
               , "routerOutRouterFee" ':= PInteger
               , "inTokenSymbol" ':= PCurrencySymbol
               , "inTokenTokenName" ':= PTokenName
               , "inTokenMode" ':= PTokenType
               , "outTokenSymbol" ':= PCurrencySymbol
               , "outTokenTokenName" ':= PTokenName
               , "outTokenMode" ':= PTokenType
               , "businessSpecific" ':= PData
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PRouterDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PRouterDatum

instance PUnsafeLiftDecl PRouterDatum where type PLifted PRouterDatum = RouterDatum
deriving via (DerivePConstantViaData RouterDatum PRouterDatum) instance PConstantDecl RouterDatum

type PBusinessValidator =
  ( PDatum -- datum expected at the business address
      :--> PScriptHash -- withdrawal canceller script hash
      :--> PAddress -- swap target address (router out)
      :--> PSmartHandleDatum -- datum to be attached after swap
      :--> PValue 'Sorted 'Positive -- value of the input utxo
      :--> PInteger -- router fee
      :--> PRouterDatum -- router datum extracted from `outputDatum` (first parameter of this function)
      :--> PBool
  )
