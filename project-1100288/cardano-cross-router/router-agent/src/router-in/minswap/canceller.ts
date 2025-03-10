import {
  ProviderName,
  logAbort,
  logDim,
  setupLucid,
  showOutRef,
  showShortOutRef,
} from "@anastasia-labs/smart-handles-agent";
import {
  Address,
  Lucid,
  LucidEvolution,
  Network,
  OutputDatum,
  Result,
  Script,
  UTxO,
  applyDoubleCborEncoding,
  datumJsonToCbor,
  errorToString,
  genericCatch,
  ok,
  validatorToScriptHash,
} from "@anastasia-labs/smart-handles-offchain";
import * as L from "lucid-cardano";
import * as M from "@minswap/sdk";
import {
  getBFProvider,
  getRouterDatumFromAdvancedDatum,
  getSingleAssetApartFromAda,
} from "../../utils/config";
import * as RIN from "./constants.js";
import {
  MAINNET_MINTING_SCRIPT,
  MAINNET_TREASURY_ADDRESS,
  MINT_SCRIPT_REDEEMER_DATA,
  PREPROD_MINTING_SCRIPT,
  PREPROD_TREASURY_ADDRESS,
} from "../../constants";

const getLucid = async (network: Network): Promise<Result<LucidEvolution>> => {
  try {
    const bfRes = getBFProvider(network);
    if (bfRes.type === "error") return bfRes;
    const bf = bfRes.data;
    const lucid = await Lucid(bf, network);
    // Note that this environment variable is the same as the one expected in
    // `smart-handles-agent`
    const seed = process.env.SEED_PHRASE;
    if (!seed)
      return {
        type: "error",
        error: new Error(
          "Wallet seed phrase not found (expected at $SEED_PHRASE)"
        ),
      };
    lucid.selectWallet.fromSeed(seed);
    return ok(lucid);
  } catch (e) {
    return genericCatch(e);
  }
};

const renderUTxOs = (utxos: UTxO[]): string => {
  if (utxos.length < 1) {
    return "";
  } else if (utxos.length === 1) {
    return showOutRef({ ...utxos[0] });
  } else {
    const outRefsRendered: string[] = utxos.map((u) =>
      showShortOutRef({ ...u })
    );
    return outRefsRendered.join(", ");
  }
};

const TX_BUILT_MSG = "Transaction successfully built.";
const SIGNING_TX_MSG = "Signing the transaction...";
const SUBMITTING_TX_MSG = "Submitting the transaction...";
const AWAITING_TX_MSG = "Tx submitted. Awaiting on-chain registration...";

export const registerCancellerRewardAddress = async (
  network: Network,
  provider: ProviderName,
): Promise<Result<[string, Address, string]>> => {
  try {
    const lucid = await setupLucid(network, provider);
    const rewardAddress =
      network === "Mainnet"
        ? RIN.MAINNET_CANCELLER_REWARD_ADDRESS
        : RIN.PREPROD_CANCELLER_REWARD_ADDRESS;
    const cancellerHash =
      network === "Mainnet"
        ? RIN.MAINNET_CANCELLER_SCRIPT_HASH
        : RIN.PREPROD_CANCELLER_SCRIPT_HASH;
    logDim(`Building the transaction to register:
${rewardAddress}`)
    const tx = await lucid.newTx().registerStake(rewardAddress).complete();
    logDim(TX_BUILT_MSG);
    logDim(SIGNING_TX_MSG);
    const signedTx = await tx.sign.withWallet().complete();
    logDim(SUBMITTING_TX_MSG);
    const txHash = await signedTx.submit();
    await lucid.awaitTx(txHash);
    return ok([cancellerHash, rewardAddress, txHash]);
  } catch (e) {
    return genericCatch(e);
  }
};

export const cancelOneOrder = async (
  network: Network,
  providerName: ProviderName
): Promise<Result<string>> => {
  const isMainnet = network === "Mainnet";
  const cancellerRewardAddress = isMainnet
    ? RIN.MAINNET_CANCELLER_REWARD_ADDRESS
    : RIN.PREPROD_CANCELLER_REWARD_ADDRESS;
  const cancellerHash = isMainnet
    ? RIN.MAINNET_CANCELLER_SCRIPT_HASH
    : RIN.PREPROD_CANCELLER_SCRIPT_HASH;
  const minswapAddress = isMainnet
    ? RIN.MAINNET_MINSWAP_ADDRESS
    : RIN.PREPROD_MINSWAP_ADDRESS;
  try {
    const lucid = await setupLucid(network, providerName);
    const provider = lucid.config().provider;
    logDim(`Fetching orders at:
${minswapAddress}`);
    if (!provider) return {type: "error", error: new Error("Provider missing")}
    const orderUTxOs = await provider.getUtxos(minswapAddress);
    const networkId = isMainnet ? M.NetworkId.MAINNET : M.NetworkId.TESTNET;
    const orderDatums: M.OrderV2.Datum[] = [];
    const filteredOrderUTxOs = orderUTxOs.filter((u: UTxO) => {
      if (u.datum) {
        try {
          const constr = L.Data.from(u.datum, L.Constr<L.Data>);
          if (constr instanceof L.Constr) {
            const orderDatum = M.OrderV2.Datum.fromPlutusData(
              networkId,
              constr
            );
            if (
              orderDatum.canceller.type ===
                M.OrderV2.AuthorizationMethodType.WITHDRAW_SCRIPT &&
              orderDatum.canceller.hash === cancellerHash
            ) {
              orderDatums.push(orderDatum);
              return true;
            } else {
              return false;
            }
          } else {
            return false;
          }
        } catch (e) {
          return false;
        }
      } else {
        return false;
      }
    });
    if (filteredOrderUTxOs.length <= 0 || orderDatums.length <= 0)
      return {
        type: "error",
        error: new Error(
          `No order UTxOs were found with a canceller of:
${cancellerHash}`
        ),
      };
    logDim(`Found ${filteredOrderUTxOs.length} UTxO(s):
${renderUTxOs(filteredOrderUTxOs)}`);
    const orderUTxO = filteredOrderUTxOs[0];
    const orderDatum = orderDatums[0];
    logDim(`Building the transaction to cancel:
${showOutRef({ ...orderUTxO })}`);
    const deployedScript = M.DexV2Constant.DEPLOYED_SCRIPTS[networkId];
    const treasuryDatumHash =
      orderDatum.successReceiverDatum.type ===
      M.OrderV2.ExtraDatumType.INLINE_DATUM
        ? orderDatum.successReceiverDatum.hash
        : undefined;
    if (treasuryDatumHash === undefined)
      return {
        type: "error",
        error: new Error("Success receiver datum not found"),
      };
    // We'll provide the resolved router datum via the redeemer.
    const redeemerData = await provider.getDatum(treasuryDatumHash);
    const routerDatumRes = getRouterDatumFromAdvancedDatum(
      network,
      redeemerData
    );
    if (routerDatumRes.type == "error") return routerDatumRes;
    const treasuryOutputDatum: OutputDatum = {
      kind: "inline",
      value: "d87980", // void
    };
    const orderScriptUTxOs = await provider.getUtxosByOutRef([deployedScript.order]);
    const tx = lucid
      .newTx()
      .collectFrom(
        [orderUTxO],
        datumJsonToCbor({
          constructor: M.OrderV2.Redeemer.CANCEL_ORDER_BY_OWNER,
          fields: [],
        })
      )
      .readFrom(orderScriptUTxOs)
      .withdraw(
        cancellerRewardAddress,
        BigInt(0),
        redeemerData
      )
      .attach.WithdrawalValidator(
        isMainnet ? RIN.MAINNET_CANCELLER_SCRIPT : RIN.PREPROD_CANCELLER_SCRIPT
      );
    if (routerDatumRes.data.inTokenMode == "Mint") {
      const tokenRes = getSingleAssetApartFromAda(orderUTxO.assets);
      if (tokenRes.type === "error") return tokenRes;
      tx.pay
        .ToContract(
          isMainnet ? MAINNET_TREASURY_ADDRESS : PREPROD_TREASURY_ADDRESS,
          treasuryOutputDatum,
          { lovelace: orderUTxO.assets["lovelace"] }
        )
        .mintAssets(
          { [tokenRes.data[0]]: -tokenRes.data[1] },
          MINT_SCRIPT_REDEEMER_DATA
        )
        .attach.MintingPolicy(
          isMainnet ? MAINNET_MINTING_SCRIPT : PREPROD_MINTING_SCRIPT
        );
    } else {
      tx.pay.ToContract(
        isMainnet ? MAINNET_TREASURY_ADDRESS : PREPROD_TREASURY_ADDRESS,
        treasuryOutputDatum,
        orderUTxO.assets
      );
    }
    const completedTx = await tx.complete({localUPLCEval: false});
    logDim(TX_BUILT_MSG);
    logDim(SIGNING_TX_MSG);
    const signedTx = await completedTx.sign.withWallet().complete();
    logDim(SUBMITTING_TX_MSG);
    const txHash = await signedTx.submit();
    logDim(AWAITING_TX_MSG);
    await lucid.awaitTx(txHash);
    return ok(txHash);
  } catch (e) {
    logAbort(errorToString(e));
    process.exit(1);
  }
};
