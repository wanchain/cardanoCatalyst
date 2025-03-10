module RouterConstants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PScriptCredential), PMaybeData (..), PStakingCredential (PStakingHash))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

ptreasuryScriptHash :: ClosedTerm PScriptHash
ptreasuryScriptHash =
  pcon $
    PScriptHash $
      phexByteStr "1d92619dfd0e638c96e1e275c8040a2424c22ba8252785d2e682346e"

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
                    (pcon $ PScriptHash $ phexByteStr "4e83a9652d76167d9289b190def35276bec262fb0cd18caa4a0835d5")
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
