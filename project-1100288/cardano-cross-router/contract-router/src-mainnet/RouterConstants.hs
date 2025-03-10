module RouterConstants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PScriptCredential), PMaybeData (..), PStakingCredential (PStakingHash))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

ptreasuryScriptHash :: ClosedTerm PScriptHash
ptreasuryScriptHash =
  pcon $
    PScriptHash $
      phexByteStr "00000000000000000000000000000000000000000000000000000000"

ptreasuryStakingHash :: ClosedTerm PStakingCredential
ptreasuryStakingHash =
  pcon $
    PStakingHash $
      pdcons
        # pdata
          ( pcon $
              PScriptCredential $
                pdcons
                  # pdata
                    (pcon $ PScriptHash $ phexByteStr "00000000000000000000000000000000000000000000000000000000")
                  #$ pdnil
          )
        #$ pdnil

ptreasuryAddress :: ClosedTerm PAddress
ptreasuryAddress =
  pcon $
    PAddress $
      pdcons @"credential"
        # pdata
          ( pcon $
              PScriptCredential $
                pdcons # pdata ptreasuryScriptHash #$ pdnil
          )
        #$ pdcons @"stakingCredential"
        # pdata (pcon $ PDJust $ pdcons # pdata ptreasuryStakingHash #$ pdnil)
        #$ pdnil
