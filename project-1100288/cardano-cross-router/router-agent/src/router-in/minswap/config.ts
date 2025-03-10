// IMPORTS
// {{{
import {
  Config,
  RequestInfo,
  Target,
  logAbort,
} from "@anastasia-labs/smart-handles-agent";
import * as CML from "@anastasia-labs/cardano-multiplatform-lib-nodejs";
import {
  Data,
  OutputDatum,
  Result,
  TxBuilder,
  UTxO,
  genericCatch,
  ok,
  parseSafeDatum,
  AdvancedOutputDatumMaker,
  AdvancedDatumFields,
  Assets,
  Unit,
  fromUnit,
  advancedDatumFieldsToCBOR,
  DatumJson,
  Network,
  parseAdvancedDatum,
  LOVELACE_MARGIN,
  walletFromSeed,
} from "@anastasia-labs/smart-handles-offchain";
import * as L from "lucid-cardano";
import * as M from "@minswap/sdk";
import BigNumber from "bignumber.js";
import * as ROUT from "../../router-out";
import { RouterDatum, tokenModeToDatumJson } from "../../types";
import { MinswapRequest } from "./types";
import {
  MAINNET_MINTING_SCRIPT,
  MAINNET_TREASURY_ADDRESS,
  MINT_SCRIPT_REDEEMER_DATA,
  PREPROD_MINTING_SCRIPT,
  PREPROD_TREASURY_ADDRESS,
  TREASURY_DATUM_CBOR,
  mkTreasuryDatum,
} from "../../constants";
import * as RIN from "./constants";
import { getBFAdapter, mkMintQtyFinder } from "../../utils/config.js";
// }}}

// UTILS
// {{{
/**
 * Copied from on-chain logic. KEEP IN SYNC!
 */
const plovelacesAfterFees = (
  inputLovelaces: bigint,
  routerFee: bigint,
  routerOutRouterFee: bigint
): bigint => {
  return (
    inputLovelaces -
    RIN.BATCHER_FEE -
    RIN.DEPOSIT -
    routerFee -
    routerOutRouterFee
  );
};

const getInputAssetAndQtyFromSmartHandlesInputAssets = (
  routerFee: bigint,
  routerOutRouterFee: bigint,
  inputAssets: Assets
): Result<[Unit, bigint]> => {
  // {{{
  const flattenedAssets = Object.entries(inputAssets);
  if (
    flattenedAssets.length === 1 &&
    (flattenedAssets[0][0] === "" || flattenedAssets[0][0] === "lovelace")
  ) {
    return ok([
      "",
      plovelacesAfterFees(flattenedAssets[0][1], routerFee, routerOutRouterFee),
    ]);
  } else if (flattenedAssets.length === 2) {
    const [inAsset, inAssetQty, inLovelaces] =
      flattenedAssets[0][0] === "" || flattenedAssets[0][0] === "lovelace"
        ? [flattenedAssets[1][0], flattenedAssets[1][1], flattenedAssets[0][1]]
        : [flattenedAssets[0][0], flattenedAssets[0][1], flattenedAssets[1][1]];
    const lovelacesAfterFees = plovelacesAfterFees(
      inLovelaces,
      routerFee,
      routerOutRouterFee
    );
    if (lovelacesAfterFees === BigInt(0)) {
      return ok([inAsset, inAssetQty]);
    } else {
      return {
        type: "error",
        error: new Error("Invalid input Lovelace count"),
      };
    }
  } else {
    return {
      type: "error",
      error: new Error("Input UTxO has too many assets"),
    };
  }
  // }}}
};

const processAdvancedFields = (
  aF: AdvancedDatumFields
): Result<{
  routerDatum: RouterDatum;
  minswapRequest: MinswapRequest;
  routerOutDatumCBOR: string;
}> => {
  // {{{
  const routerDatumRes = parseSafeDatum(Data.to(aF.extraInfo), RouterDatum);
  if (routerDatumRes.type == "left") {
    return {
      type: "error",
      error: new Error(routerDatumRes.value),
    };
  }
  const routerDatum = routerDatumRes.value;
  const minswapRequestRes = parseSafeDatum(
    Data.to(routerDatum.businessSpecific),
    MinswapRequest
  );
  if (minswapRequestRes.type == "left")
    return {
      type: "error",
      error: new Error(minswapRequestRes.value),
    };
  const routerOutDatumCBORRes = advancedDatumFieldsToCBOR(
    {
      ...aF,
      mOwner: null,
      routerFee: routerDatum.routerOutRouterFee,
      reclaimRouterFee: BigInt(0),
      routeRequiredMint:
        routerDatum.outTokenMode === "Mint"
          ? {
              policyId: routerDatum.outTokenSymbol,
              tokenName: routerDatum.outTokenTokenName,
            }
          : null,
      reclaimRequiredMint: null,
    },
    true
  );
  if (routerOutDatumCBORRes.type == "error") return routerOutDatumCBORRes;
  return ok({
    routerDatum,
    minswapRequest: minswapRequestRes.value,
    routerOutDatumCBOR: routerOutDatumCBORRes.data,
  });
  // }}}
};
// }}}

export const mkConfig = (
  network: Network,
  target: Target,
  noBlockfrost?: boolean
): Config => {
  // {{{
  const isMainnet = network === "Mainnet";
  const isSingle = target === "Single";
  const treasuryAddress = isMainnet
    ? MAINNET_TREASURY_ADDRESS
    : PREPROD_TREASURY_ADDRESS;
  const cancellerScriptHash = isMainnet
    ? RIN.MAINNET_CANCELLER_SCRIPT_HASH
    : RIN.PREPROD_CANCELLER_SCRIPT_HASH;
  const mintScript = isMainnet
    ? MAINNET_MINTING_SCRIPT
    : PREPROD_MINTING_SCRIPT;
  return {
    label: "RouterIn Minswap V2",
    network: network,
    provider: "Kupmios",
    pollingInterval: 10_000,
    scriptCBOR: isMainnet
      ? isSingle
        ? RIN.MAINNET_SINGLE_SCRIPT_CBOR
        : RIN.MAINNET_BATCH_STAKE_SCRIPT_CBOR
      : isSingle
      ? RIN.PREPROD_SINGLE_SCRIPT_CBOR
      : RIN.PREPROD_BATCH_STAKE_SCRIPT_CBOR,
    scriptTarget: target,
    routeDestination: isMainnet
      ? RIN.MAINNET_MINSWAP_ADDRESS
      : RIN.PREPROD_MINSWAP_ADDRESS,
    advancedReclaimConfig: {
      // {{{
      outputDatumMaker: (async (_inputAssets, _inputDatum) =>
        ok({
          kind: "inline",
          value: TREASURY_DATUM_CBOR,
        })) as AdvancedOutputDatumMaker,
      additionalAction: async (
        tx: TxBuilder,
        _utxo: UTxO
      ): Promise<Result<TxBuilder>> => ok(tx),
      requiredMintConfig: {
        mintQuantityFinder: mkMintQtyFinder(true),
        mintRedeemer: MINT_SCRIPT_REDEEMER_DATA,
        mintScript,
      },
      // }}}
    },
    advancedRouteConfig: {
      // {{{
      additionalAction: async (tx: TxBuilder, utxo: UTxO) => {
        // Producing a UTxO at wallet's address with an inline datum identical
        // to what Minswap must produce at `successReceiver`. This gives
        // batchers the ability to resolve the hash with dbsync.
        //
        // We are not doing a similar thing for the `refundReceiver` because it
        // is already registered on-chain (both preprod and mainnet).
        // const txConfig = tx.rawConfig();
        // const lucidConfig = txConfig.lucidConfig;
        const wallet = process.env.SEED_PHRASE ? walletFromSeed(process.env.SEED_PHRASE, {network, accountIndex: 0}) : undefined;
        if (utxo.datum) {
          const aFRes = parseAdvancedDatum(utxo.datum, network);
          if (aFRes.type == "error") return aFRes;
          const processRes = processAdvancedFields(aFRes.data);
          if (processRes.type == "error") return processRes;
          const { routerOutDatumCBOR } = processRes.data;
          const walletAddress = wallet?.address;
          if (walletAddress) {
            const newTx = tx.pay.ToContract(
              walletAddress,
              { kind: "inline", value: routerOutDatumCBOR },
              { lovelace: LOVELACE_MARGIN }
            );
            return ok(newTx);
          } else {
            return {
              type: "error",
              error: new Error("No wallet was found"),
            };
          }
        } else {
          return {
            type: "error",
            error: new Error("UTxO to be routed has no datum"),
          };
        }
      },
      outputDatumMaker: async (
        inputAssets: Assets,
        inputDatum: AdvancedDatumFields
      ): Promise<Result<OutputDatum>> => {
        const processRes = processAdvancedFields(inputDatum);
        if (processRes.type == "error") return processRes;
        const { routerDatum, minswapRequest, routerOutDatumCBOR } =
          processRes.data;
        const cmlData = CML.PlutusData.from_cbor_hex(routerOutDatumCBOR);
        const routerOutDatumHash = CML.hash_plutus_data(cmlData);
        const routerOutDatumHashHex = routerOutDatumHash.to_hex();
        cmlData.free();
        routerOutDatumHash.free();
        const inAssetQtyRes = getInputAssetAndQtyFromSmartHandlesInputAssets(
          inputDatum.routerFee,
          routerDatum.routerOutRouterFee,
          inputAssets
        );
        if (inAssetQtyRes.type == "error") return inAssetQtyRes;
        const [inAsset, inQty] = inAssetQtyRes.data;
        const splitInAsset = fromUnit(inAsset);
        const step: M.OrderV2.SwapExactIn = {
          type: M.OrderV2.StepType.SWAP_EXACT_IN,
          direction: minswapRequest.aToBDirection
            ? M.OrderV2.Direction.A_TO_B
            : M.OrderV2.Direction.B_TO_A,
          killable: M.OrderV2.Killable.PENDING_ON_FAILED,
          minimumReceived: minswapRequest.minimumReceive,
          swapAmount: {
            type: M.OrderV2.AmountType.SPECIFIC_AMOUNT,
            swapAmount: inQty,
          },
        };
        try {
          const canceller: M.OrderV2.AuthorizationMethod = {
            type: M.OrderV2.AuthorizationMethodType.WITHDRAW_SCRIPT,
            hash: cancellerScriptHash,
          };
          const inIsA =
            inAsset.toLowerCase() <
            `${routerDatum.outTokenSymbol}${routerDatum.outTokenTokenName}`.toLowerCase();
          const inMinswapAsset = {
            policyId: splitInAsset.policyId,
            tokenName: splitInAsset.assetName ?? "",
          };
          const outMinswapAsset = {
            policyId: routerDatum.outTokenSymbol,
            tokenName: routerDatum.outTokenTokenName,
          };
          const lpAssetName = inIsA
            ? M.PoolV2.computeLPAssetName(inMinswapAsset, outMinswapAsset)
            : M.PoolV2.computeLPAssetName(outMinswapAsset, inMinswapAsset);
          const successReceiverDatum: M.OrderV2.ExtraDatum = {
            type: M.OrderV2.ExtraDatumType.INLINE_DATUM,
            hash: routerOutDatumHashHex,
          };
          const orderDatum: M.OrderV2.Datum = {
            canceller,
            refundReceiver: treasuryAddress, // not used since orders are not killable
            refundReceiverDatum: mkTreasuryDatum(), // not used since orders are not killable
            successReceiver:
              target === "Single"
                ? ROUT.getSingleScriptAddress(network)
                : ROUT.getBatchSpendScriptAddress(network),
            successReceiverDatum,
            lpAsset: {
              policyId: isMainnet
                ? RIN.MAINNET_LP_SYMBOL
                : RIN.PREPROD_LP_SYMBOL,
              tokenName: lpAssetName,
            },
            step,
            maxBatcherFee: RIN.BATCHER_FEE,
            expiredOptions: undefined,
          };
          const datumCBOR = L.Data.to(M.OrderV2.Datum.toPlutusData(orderDatum));
          return ok({
            kind: "inline",
            value: datumCBOR,
          });
        } catch (e) {
          return genericCatch(e);
        }
      },
      // }}}
    },
    advancedRouteRequestMaker: async (requestInfo: RequestInfo) => {
      // {{{
      const extraArgs = requestInfo.extraConfig;
      if (
        requestInfo.owner &&
        extraArgs &&
        extraArgs["uniqueId"] &&
        extraArgs["inPairId"] &&
        extraArgs["outPairId"] &&
        extraArgs["evmReceiver"] &&
        extraArgs["routerOutRouterFee"] &&
        typeof extraArgs["routerOutRouterFee"] === "number" &&
        extraArgs["inTokenMode"] &&
        (extraArgs["inTokenMode"] === "Mint" ||
          extraArgs["inTokenMode"] === "Spend") &&
        extraArgs["outTokenSymbol"] &&
        extraArgs["outTokenTokenName"] &&
        extraArgs["outTokenMode"] &&
        (extraArgs["outTokenMode"] === "Mint" ||
          extraArgs["outTokenMode"] === "Spend") &&
        extraArgs["slippageTolerance"] &&
        typeof extraArgs["slippageTolerance"] === "number"
      ) {
        const routerOutRouterFee = BigInt(extraArgs["routerOutRouterFee"]);
        const outUnit =
          `${extraArgs["outTokenSymbol"]}${extraArgs["outTokenTokenName"]}`.toLowerCase();
        const flattenedAssets: [string, bigint][] = Object.entries(
          requestInfo.asset
        );
        if (
          extraArgs["inTokenMode"] === "Spend" &&
          flattenedAssets.length > 0
        ) {
          return {
            type: "error",
            error: new Error(
              "One or more assets are provided, while `inTokenMode` was set to `Spend`"
            ),
          };
        } else if (
          extraArgs["inTokenMode"] === "Mint" &&
          flattenedAssets.length !== 1
        ) {
          return {
            type: "error",
            error: new Error(
              "Expected exactly 1 token for an `inTokenMode` of `Mint`"
            ),
          };
        }
        const minRequiredLovelaces =
          BigInt(
            Math.max(
              Number(requestInfo.routerFee),
              Number(requestInfo.reclaimRouterFee)
            )
          ) +
          routerOutRouterFee +
          RIN.BATCHER_FEE +
          RIN.DEPOSIT;
        const [inAsset, inQty] =
          flattenedAssets.length === 0
            ? ["", requestInfo.lovelace]
            : flattenedAssets[0];
        if (inAsset === "" && requestInfo.lovelace < minRequiredLovelaces) {
          logAbort(
            `At least ${minRequiredLovelaces} Lovelaces must be locked.`
          );
          process.exit(1);
        } else if (
          inAsset !== "" &&
          requestInfo.lovelace !== minRequiredLovelaces
        ) {
          logAbort(`Locked Lovelaces must be exactly ${minRequiredLovelaces}`);
          process.exit(1);
        }
        const inIsA = inAsset.toLowerCase() < outUnit;
        const splitInAsset = fromUnit(inAsset);

        // Set to string because of the way Minswap has implemented slippage
        // application. It is being "unsafely" parsed to a `number` farther
        // down.
        let slippageAdjustedAmount: string;

        if (noBlockfrost) {
          slippageAdjustedAmount = `${inQty}`;
        } else {
          const bfAdapterRes = getBFAdapter(network);
          if (bfAdapterRes.type == "error") return bfAdapterRes;
          const bfAdapter = bfAdapterRes.data;
          try {
            const slippageTolerance = new BigNumber(
              extraArgs["slippageTolerance"]
            ).div(100);
            const pool = await bfAdapter.getV2PoolByPair(
              {
                policyId: splitInAsset.policyId,
                tokenName: splitInAsset.assetName ?? "",
              },
              {
                policyId: extraArgs["outTokenSymbol"],
                tokenName: extraArgs["outTokenTokenName"],
              }
            );
            if (!pool)
              return {
                type: "error",
                error: new Error("Failed to fetch pool"),
              };

            const amountOut = M.DexV2Calculation.calculateAmountOut({
              reserveIn: inIsA ? pool.reserveA : pool.reserveB,
              reserveOut: inIsA ? pool.reserveB : pool.reserveA,
              amountIn: inQty,
              tradingFeeNumerator: inIsA ? pool.feeA[0] : pool.feeB[0],
            });

            slippageAdjustedAmount = new BigNumber(1)
              .div(new BigNumber(1).plus(slippageTolerance))
              .multipliedBy(amountOut.toString())
              .toFixed(0, BigNumber.ROUND_DOWN);
          } catch (e) {
            return genericCatch(e);
          }
        }
        // const minswapRequest: MinswapRequest = {
        //   aToBDirection: requestInfo["aToBDirection"] === true,
        //   minimumReceive: BigInt(parseInt(slippageAdjustedAmount, 10)),
        // };
        const minswapRequest: DatumJson = {
          constructor: 0,
          fields: [
            {
              constructor: inIsA ? 1 : 0,
              fields: [],
            },
            { int: parseInt(slippageAdjustedAmount, 10) },
          ],
        };

        // const routerDatum: RouterDatum = {
        //   uniqueId: extraArgs["uniqueId"],
        //   inPairId: extraArgs["inPairId"],
        //   outPairId: extraArgs["outPairId"],
        //   evmReceiver: extraArgs["evmReceiver"],
        //   routerOutRouterFee,
        //   inTokenSymbol: splitInAsset.policyId,
        //   inTokenTokenName: splitInAsset.assetName ?? "",
        //   inTokenMode: extraArgs["inTokenMode"],
        //   outTokenSymbol: extraArgs["outTokenSymbol"],
        //   outTokenTokenName: extraArgs["outTokenTokenName"],
        //   outTokenMode: extraArgs["outTokenMode"],
        //   businessSpecific: Data.from(Data.to(minswapRequest, MinswapRequest)),
        // };
        const routerDatum: DatumJson = {
          constructor: 0,
          fields: [
            { bytes: extraArgs["uniqueId"] },
            { bytes: extraArgs["inPairId"] },
            { bytes: extraArgs["outPairId"] },
            { bytes: extraArgs["evmReceiver"] },
            { int: Number(routerOutRouterFee) },
            { bytes: splitInAsset.policyId },
            { bytes: splitInAsset.assetName ?? "" },
            tokenModeToDatumJson(extraArgs["inTokenMode"]),
            { bytes: extraArgs["outTokenSymbol"] },
            { bytes: extraArgs["outTokenTokenName"] },
            tokenModeToDatumJson(extraArgs["outTokenMode"]),
            minswapRequest,
          ],
        };
        return ok({
          valueToLock: {
            ...requestInfo.asset,
            lovelace: requestInfo.lovelace,
          },
          owner: requestInfo.owner.address.bech32,
          routerFee: requestInfo.routerFee,
          reclaimRouterFee: requestInfo.reclaimRouterFee,
          routeRequiredMint: null,
          reclaimRequiredMint:
            extraArgs["inTokenMode"] === "Mint"
              ? {
                  policyId: splitInAsset.policyId,
                  tokenName: splitInAsset.assetName ?? "",
                }
              : null,
          extraInfoDataBuilder: () => routerDatum,
        });
      } else {
        return {
          type: "error",
          error: new Error(
            "Some arguments are missing from the config JSON file"
          ),
        };
      }
      // }}}
    },
  };
  // }}}
};
