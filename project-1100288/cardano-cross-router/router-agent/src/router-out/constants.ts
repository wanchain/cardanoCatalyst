import { unsafeGenerateConstantsFromJSONFiles } from "../utils/config.js";

// import mainnetRouterOutSingleSpend from "../../../contract-router/compiled-mainnet/routerOutSingleSpend.json";
// import mainnetRouterOutBatchSpend from "../../../contract-router/compiled-mainnet/routerOutBatchSpend.json";
// import mainnetRouterOutBatchStaking from "../../../contract-router/compiled-mainnet/routerOutStaking.json";

import preprodRouterOutSingleSpend from "../../config/contract-router/compiled-testnet/routerOutSingleSpend.json";
import preprodRouterOutBatchSpend from "../../config/contract-router/compiled-testnet/routerOutBatchSpend.json";
import preprodRouterOutBatchStaking from "../../config/contract-router/compiled-testnet/routerOutStaking.json";

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
//     mainnetRouterOutSingleSpend,
//     mainnetRouterOutBatchSpend,
//     mainnetRouterOutBatchStaking,
//     preprodRouterOutSingleSpend,
//     preprodRouterOutBatchSpend,
//     preprodRouterOutBatchStaking
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
    preprodRouterOutSingleSpend,
    preprodRouterOutBatchSpend,
    preprodRouterOutBatchStaking
);
