import {
  Address,
  AdvancedDatumFields,
  Assets,
  Blockfrost,
  CBORHex,
  Data,
  Network,
  Provider,
  Result,
  RewardAddress,
  genericCatch,
  ok,
  parseAdvancedDatum,
  parseSafeDatum,
  toUnit,
  validatorToAddress,
  validatorToRewardAddress,
} from "@anastasia-labs/smart-handles-offchain";
import * as B from "@blockfrost/blockfrost-js";
import * as M from "@minswap/sdk";
import { RouterDatum } from "../types.js";

export const getBFApiKey = (): Result<string> => {
  const bfApiKey = process.env.BLOCKFROST_KEY;
  if (!bfApiKey) {
    return {
      type: "error",
      error: new Error(
        "Blockfrost API key missing (expected at $BLOCKFROST_KEY)"
      ),
    };
  } else {
    return ok(bfApiKey);
  }
}

export const getBFAdapter = (network: Network): Result<M.BlockfrostAdapter> => {
  // {{{
  const bfApiKeyRes = getBFApiKey();
  if (bfApiKeyRes.type === "error") return bfApiKeyRes;
  const bf = new B.BlockFrostAPI({
    network: network === "Mainnet" ? "mainnet" : "preprod",
    projectId: bfApiKeyRes.data,
  });
  const bfAdapter = new M.BlockfrostAdapter({
    networkId:
      network === "Mainnet" ? M.NetworkId.MAINNET : M.NetworkId.TESTNET,
    blockFrost: bf,
  });
  return ok(bfAdapter);
  // }}}
};

export const getBFProvider = (network: Network): Result<Provider> => {
  // {{{
  const bfApiKeyRes = getBFApiKey();
  if (bfApiKeyRes.type === "error") return bfApiKeyRes;
  const bfApi = new B.BlockFrostAPI({
    projectId: bfApiKeyRes.data,
    network: network === "Mainnet" ? "mainnet" : "preprod",
  });
  const bf = new Blockfrost(bfApi.apiUrl, bfApiKeyRes.data);
  return ok(bf);
  // }}}
};

export const getRouterDatumFromData = (
  extraInfo: Data
): Result<RouterDatum> => {
  // {{{
  const routerDatumRes = parseSafeDatum(Data.to(extraInfo), RouterDatum);
  if (routerDatumRes.type == "left") {
    return {
      type: "error",
      error: new Error(routerDatumRes.value),
    };
  } else {
    return ok(routerDatumRes.value);
  }
  // }}}
};

export const getRouterDatumFromAdvancedDatum = (
  network: Network,
  advancedDatumCBOR: string | null | undefined
): Result<RouterDatum> => {
  // {{{
  const datumRes = parseAdvancedDatum(advancedDatumCBOR ?? "", network);
  if (datumRes.type == "error") return datumRes;
  return getRouterDatumFromData(datumRes.data.extraInfo);
  // }}}
};

export const getSingleAssetApartFromAda = (inputAssets: Assets): Result<[string, bigint]> => {
  try {
    const [[unit0, qty0], [unit1, qty1]] = Object.entries(inputAssets);
    const asset: [string, bigint] =
      unit0 === "" || unit0 === "lovelace"
        ? [unit1, qty1]
        : [unit0, qty0];
    return ok(asset);
  } catch (e) {
    return genericCatch(e);
  }
};

export const mkMintQtyFinder = (
  forInToken: boolean
): ((
  inputAssets: Assets,
  inputDatum: AdvancedDatumFields
) => Promise<Result<bigint>>) => {
  // {{{
  return async (
    inputAssets: Assets,
    inputDatum: AdvancedDatumFields
  ): Promise<Result<bigint>> => {
    const routerDatumRes = getRouterDatumFromData(inputDatum.extraInfo);
    if (routerDatumRes.type == "error") {
      return routerDatumRes;
    } else {
      const rD = routerDatumRes.data;
      if (
        (forInToken && rD.inTokenMode == "Mint") ||
        (!forInToken && rD.outTokenMode == "Mint")
      ) {
        try {
          const assetRes = getSingleAssetApartFromAda(inputAssets);
          if (assetRes.type === "error") return assetRes;
          const [mintUnit, mintQty] =
              [assetRes.data[0], -assetRes.data[1]];
          const specifiedMintUnit = forInToken
            ? toUnit(rD.inTokenSymbol, rD.inTokenTokenName)
            : toUnit(rD.outTokenSymbol, rD.outTokenTokenName);
          if (mintQty != 0n && specifiedMintUnit === mintUnit) {
            return ok(mintQty);
          } else {
            return {
              type: "error",
              error: new Error(
                "Mint quantity resolved to 0, or there was a mismatch between the specified mint unit and the stored tokens"
              ),
            };
          }
        } catch (e) {
          return {
            type: "error",
            error: new Error(
              'TokenMode was set to "Mint" but input token count was not exactly 2'
            ),
          };
        }
      } else {
        return {
          type: "error",
          error: new Error(
            'Invalid config: specified direction (TokenInt/TokenOut) doesn\'t have a corresponding "Mint" mode in the datum'
          ),
        };
      }
    }
  };
  // }}}
};

export const unsafeGenerateConstantsFromJSONFiles = (
  mainnetSingleSpend: any,
  mainnetBatchSpend: any,
  mainnetBatchStaking: any,
  preprodSingleSpend: any,
  preprodBatchSpend: any,
  preprodBatchStaking: any
): [
  CBORHex, // mainnet single spend script
  CBORHex, // mainnet batch spend script
  CBORHex, // mainnet staking script
  CBORHex, // preprod single spend script
  CBORHex, // preprod batch spend script
  CBORHex, // preprod staking script
  (network: Network) => Address,
  (network: Network) => Address,
  (network: Network) => RewardAddress
] => {
  const MAINNET_SINGLE_SCRIPT_CBOR = mainnetSingleSpend.cborHex;
  const MAINNET_BATCH_SPEND_SCRIPT_CBOR = mainnetBatchSpend.cborHex;
  const MAINNET_BATCH_STAKE_SCRIPT_CBOR = mainnetBatchStaking.cborHex;

  const PREPROD_SINGLE_SCRIPT_CBOR = preprodSingleSpend.cborHex;
  const PREPROD_BATCH_SPEND_SCRIPT_CBOR = preprodBatchSpend.cborHex;
  const PREPROD_BATCH_STAKE_SCRIPT_CBOR = preprodBatchStaking.cborHex;

  const getSingleScriptAddress = (network: Network): Address =>
    validatorToAddress(network, {
      type: "PlutusV2",
      script:
        network === "Mainnet"
          ? MAINNET_SINGLE_SCRIPT_CBOR
          : PREPROD_SINGLE_SCRIPT_CBOR,
    });
  const getBatchSpendScriptAddress = (network: Network): Address =>
    validatorToAddress(network, {
      type: "PlutusV2",
      script:
        network === "Mainnet"
          ? MAINNET_BATCH_SPEND_SCRIPT_CBOR
          : PREPROD_BATCH_SPEND_SCRIPT_CBOR,
    });
  const getBatchStakeScriptAddress = (network: Network): RewardAddress =>
    validatorToRewardAddress(network, {
      type: "PlutusV2",
      script:
        network === "Mainnet"
          ? MAINNET_BATCH_STAKE_SCRIPT_CBOR
          : PREPROD_BATCH_STAKE_SCRIPT_CBOR,
    });
  return [
    MAINNET_SINGLE_SCRIPT_CBOR,
    MAINNET_BATCH_SPEND_SCRIPT_CBOR,
    MAINNET_BATCH_STAKE_SCRIPT_CBOR,
    PREPROD_SINGLE_SCRIPT_CBOR,
    PREPROD_BATCH_SPEND_SCRIPT_CBOR,
    PREPROD_BATCH_STAKE_SCRIPT_CBOR,
    getSingleScriptAddress,
    getBatchSpendScriptAddress,
    getBatchStakeScriptAddress,
  ];
};
