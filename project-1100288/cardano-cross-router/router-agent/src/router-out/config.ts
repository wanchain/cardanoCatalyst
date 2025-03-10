import { Config, Target } from "@anastasia-labs/smart-handles-agent";
import {
  OutputDatum,
  Result,
  TxBuilder,
  UTxO,
  ok,
  AdvancedDatumFields,
  Assets,
  Network,
} from "@anastasia-labs/smart-handles-offchain";
import {
  PREPROD_TREASURY_ADDRESS,
  MAINNET_TREASURY_ADDRESS,
  TREASURY_DATUM_CBOR,
  MAINNET_MINTING_SCRIPT,
  PREPROD_MINTING_SCRIPT,
  MINT_SCRIPT_REDEEMER_DATA,
} from "../constants.js";
import { mkMintQtyFinder } from "../utils/config.js";
import * as ROUT from "./constants.js";

// export const config: Config = {
export const mkConfig = (network: Network, target: Target): Config => {
  const isMainnet = network === "Mainnet";
  const isSingle = target === "Single";
  const treasuryAddress = isMainnet
    ? MAINNET_TREASURY_ADDRESS
    : PREPROD_TREASURY_ADDRESS;
  const mintScript = isMainnet
    ? MAINNET_MINTING_SCRIPT
    : PREPROD_MINTING_SCRIPT;
  return {
    label: "RouterOut",
    network,
    provider: "Kupmios",
    pollingInterval: 10_000,
    scriptCBOR: isMainnet
      ? isSingle
        ? ROUT.MAINNET_SINGLE_SCRIPT_CBOR
        : ROUT.MAINNET_BATCH_STAKE_SCRIPT_CBOR
      : isSingle
      ? ROUT.PREPROD_SINGLE_SCRIPT_CBOR
      : ROUT.PREPROD_BATCH_STAKE_SCRIPT_CBOR,
    scriptTarget: target,
    routeDestination: treasuryAddress,
    advancedRouteConfig: {
      additionalAction: async (tx: TxBuilder, _utxo: UTxO) => ok(tx),
      outputDatumMaker: async (
        _inAssets: Assets,
        _inDatum: AdvancedDatumFields
      ): Promise<Result<OutputDatum>> =>
        ok({ kind: "inline", value: TREASURY_DATUM_CBOR }),
      requiredMintConfig: {
        mintQuantityFinder: mkMintQtyFinder(false),
        mintRedeemer: MINT_SCRIPT_REDEEMER_DATA,
        mintScript,
      },
    },
  };
};
