import {
    CertificateValidator,
    applyDoubleCborEncoding,
    validatorToRewardAddress,
    validatorToScriptHash,
} from "@anastasia-labs/smart-handles-offchain";
import { unsafeGenerateConstantsFromJSONFiles } from "../../utils/config.js";

// import mainnetRouterInSingleSpend from "./config/contract-router/compiled-mainnet/routerInSingleSpend.json";
// import mainnetRouterInBatchSpend from "./config/contract-router/compiled-mainnet/routerInBatchSpend.json";
// import mainnetRouterInBatchStaking from "./config/contract-router/compiled-mainnet/routerInStaking.json";

// import mainnetCancellerStaking from "./config/contract-router/compiled-mainnet/cancellerStaking.json";

import preprodRouterInSingleSpend from "../../../config/contract-router/compiled-testnet/routerInSingleSpend.json";
import preprodRouterInBatchSpend from "../../../config/contract-router/compiled-testnet/routerInBatchSpend.json";
import preprodRouterInBatchStaking from "../../../config/contract-router/compiled-testnet/routerInStaking.json";

import preprodCancellerStaking from "../../../config/contract-router/compiled-testnet/cancellerStaking.json";

export const MAINNET_MIN_SYMBOL =
    "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6";
export const PREPROD_MIN_SYMBOL =
    "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72";

export const MIN_TOKEN_NAME = "4d494e";

export const PREPROD_LP_SYMBOL =
    "d6aae2059baee188f74917493cf7637e679cd219bdfbbf4dcbeb1d0b";
export const MAINNET_LP_SYMBOL =
    "f5808c2c990d86da54bfc97d89cee6efa20cd8461616359478d96b4c";

export const PREPROD_MINSWAP_ADDRESS =
    "addr_test1wrdf2f2x8pq3wwk3yv936ksmt59rz94mm66yzge8zj9pk7s0kjph3";
export const MAINNET_MINSWAP_ADDRESS =
    "addr1w8p79rpkcdz8x9d6tft0x0dx5mwuzac2sa4gm8cvkw5hcnqst2ctf";

export const BATCHER_FEE = BigInt(2_000_000);
export const DEPOSIT = BigInt(2_000_000);

// export const [
//     MAINNET_SINGLE_SCRIPT_CBOR,
//     MAINNET_BATCH_SPEND_SCRIPT_CBOR,
//     MAINNET_BATCH_STAKE_SCRIPT_CBOR,
//     PREPROD_SINGLE_SCRIPT_CBOR,
//     PREPROD_BATCH_SPEND_SCRIPT_CBOR,
//     PREPROD_BATCH_STAKE_SCRIPT_CBOR,
//     getSingleScriptAddress,
//     getBatchSpendScriptAddress,
//     getBatchStakeScriptAddress,
// ] = unsafeGenerateConstantsFromJSONFiles(
//     mainnetRouterInSingleSpend,
//     mainnetRouterInBatchSpend,
//     mainnetRouterInBatchStaking,
//     preprodRouterInSingleSpend,
//     preprodRouterInBatchSpend,
//     preprodRouterInBatchStaking
// );

export const [
    MAINNET_SINGLE_SCRIPT_CBOR,
    MAINNET_BATCH_SPEND_SCRIPT_CBOR,
    MAINNET_BATCH_STAKE_SCRIPT_CBOR,
    PREPROD_SINGLE_SCRIPT_CBOR,
    PREPROD_BATCH_SPEND_SCRIPT_CBOR,
    PREPROD_BATCH_STAKE_SCRIPT_CBOR,
    getSingleScriptAddress,
    getBatchSpendScriptAddress,
    getBatchStakeScriptAddress,
] = unsafeGenerateConstantsFromJSONFiles(
    "",
    "",
    "",
    preprodRouterInSingleSpend,
    preprodRouterInBatchSpend,
    preprodRouterInBatchStaking
);

export const PREPROD_CANCELLER_SCRIPT: CertificateValidator = {
    type: "PlutusV2",
    script: applyDoubleCborEncoding(preprodCancellerStaking.cborHex),
};

export const PREPROD_CANCELLER_REWARD_ADDRESS = validatorToRewardAddress(
    "Preprod",
    PREPROD_CANCELLER_SCRIPT
);

export const PREPROD_CANCELLER_SCRIPT_HASH = validatorToScriptHash(
    PREPROD_CANCELLER_SCRIPT
);

// export const MAINNET_CANCELLER_SCRIPT: CertificateValidator = {
//     type: "PlutusV2",
//     script: applyDoubleCborEncoding(mainnetCancellerStaking.cborHex),
// };

// export const MAINNET_CANCELLER_REWARD_ADDRESS = validatorToRewardAddress(
//     "Mainnet",
//     MAINNET_CANCELLER_SCRIPT
// );

// export const MAINNET_CANCELLER_SCRIPT_HASH = validatorToScriptHash(
//     MAINNET_CANCELLER_SCRIPT
// );
