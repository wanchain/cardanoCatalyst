module Business.MinswapV2.Constants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PScriptCredential), PCurrencySymbol (PCurrencySymbol), PMaybeData (PDNothing))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

pminswapV2LPSymbol :: ClosedTerm PCurrencySymbol
pminswapV2LPSymbol = pcon $ PCurrencySymbol (phexByteStr "d6aae2059baee188f74917493cf7637e679cd219bdfbbf4dcbeb1d0b")

pminswapAddress :: ClosedTerm PAddress
pminswapAddress =
  pcon $
    PAddress $
      pdcons @"credential"
        # pdata
          ( pcon $
              PScriptCredential $
                pdcons
                  # pdata
                    ( pcon $
                        PScriptHash
                          ( phexByteStr
                              "da9525463841173ad1230b1d5a1b5d0a3116bbdeb4412327148a1b7a"
                          )
                    )
                  #$ pdnil
          )
        #$ pdcons @"stakingCredential"
        # pdata (pcon $ PDNothing pdnil)
        #$ pdnil

minswapBech32Address :: String
minswapBech32Address = "addr_test1wrdf2f2x8pq3wwk3yv936ksmt59rz94mm66yzge8zj9pk7s0kjph3"

pbatcherFee :: ClosedTerm PInteger
pbatcherFee = 2_000_000

pdeposit :: ClosedTerm PInteger
pdeposit = 2_000_000
