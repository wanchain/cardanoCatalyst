import {
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
} from "@anastasia-labs/smart-handles-offchain";
import {PREPROD_MINTING_POLICY} from "../src/constants";

export const ALWAYS_SUCCEEDS_SCRIPT: SpendingValidator = {
  type: "PlutusV2",
  script:
    "583b010000323232322253330033370e900018021baa001153330034a229309b2b09912999802a5114984d958c018c014dd5000ab9a5573aaae795d081",
};
export const ALWAYS_SUCCEEDS_SCRIPT_HASH = validatorToScriptHash(
  ALWAYS_SUCCEEDS_SCRIPT
);
export const ALWAYS_SUCCEEDS_ADDRESS = validatorToAddress(
  "Preprod",
  ALWAYS_SUCCEEDS_SCRIPT
);

export const reclaimExtraConfig = {
  uniqueId: "0000000000000000000000000000000000000000000000000000000000000000",
  inPairId: "0000000000000000000000000000000000000000000000000000000000000000",
  outPairId: "0000000000000000000000000000000000000000000000000000000000000000",
  evmReceiver:
    "0000000000000000000000000000000000000000000000000000000000000000",
  routerOutRouterFee: 1000000,
  inTokenMode: "Mint",
  outTokenSymbol: PREPROD_MINTING_POLICY,
  outTokenTokenName: "55534443",
  outTokenMode: "Mint",
  aToBDirection: true,
  slippageTolerance: 20,
  killable: true,
};
