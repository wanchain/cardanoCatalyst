#!/usr/bin/env node
import * as RIN_MIN from "./minswap/index.js";
import { outerArgsHandler } from "../utils/cli.js";
import {
  Config,
  logAbort,
  logDim,
  logInfo,
  logSuccess,
} from "@anastasia-labs/smart-handles-agent";
import {
  cancelOneOrder,
  registerCancellerRewardAddress,
} from "./minswap/canceller.js";
import { errorToString } from "@anastasia-labs/smart-handles-offchain";

outerArgsHandler("RouterIn Minswap V2", RIN_MIN.mkConfig, process.argv, {
  arguments: ["cancel-one-order", "register-canceller-validator"],
  action: async (config: Config, arg: string, _args: string[]) => {
    const network = config.network ?? "Mainnet";
    if (arg === "cancel-one-order") {
      const txHashRes = await cancelOneOrder(network, config.provider);
      if (txHashRes.type === "error") {
        logAbort(errorToString(txHashRes.error));
        process.exit(1);
      } else {
        logSuccess(`Order cancelled! Tx hash:
${txHashRes.data}`);
        process.exit(0);
      }
    } else if (arg === "register-canceller-validator") {
      try {
        logDim("Building, signing, and submitting the tx...");
        const infoRes = await registerCancellerRewardAddress(network, config.provider);
        if (infoRes.type === "error") {
          logAbort(errorToString(infoRes.error));
          process.exit(1);
        } else {
          const [cancellerHash, cancellerAddress, txHash] = infoRes.data;
          logSuccess(`
Canceller script, with a hash of:
${cancellerHash}
and a reward address of:
${cancellerAddress}
successfully registered! Tx hash:
${txHash}`
          );
          process.exit(0);
        }
      } catch (e) {
        logAbort(errorToString(e));
        process.exit(1);
      }
    } else {
      // Won't happen.
      logInfo(
        "Valid arguments are: cancel-one-order | register-canceller-validator"
      );
      process.exit(1);
    }
  },
});
