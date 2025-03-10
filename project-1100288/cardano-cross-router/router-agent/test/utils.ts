import {
  Assets,
  Emulator,
  LucidEvolution,
  EmulatorAccount,
  generateEmulatorAccount,
  Result,
  singleRequest,
  SingleRequestConfig,
  RouteRequest,
} from "@anastasia-labs/smart-handles-offchain";
import { Config, RequestInfo } from "@anastasia-labs/smart-handles-agent";

export type LucidContext = {
  lucid: LucidEvolution;
  users: { [key: string]: EmulatorAccount };
  emulator: Emulator;
};

export const createUser = (assets: Assets = {}) => {
  return generateEmulatorAccount({...assets, lovelace: BigInt(200_000_000) });
};

export const getWalletUTxOs = async (lucid: LucidEvolution) => {
  const walletAddr = await lucid.wallet().address();
  const utxos = await lucid.utxosAt(walletAddr);
  return utxos;
};

export const logWalletUTxOs = async (lucid: LucidEvolution, msg: string) => {
  const utxos = await getWalletUTxOs(lucid);
  console.log(`------------------------- ${msg} -------------------------`);
  console.log(utxos);
};

export function unsafeFromOk<T>(res: Result<T>): T {
  if (res.type == "ok") {
    return res.data;
  } else {
    throw res.error;
  }
}

export const submitRequestInfo = async (
  emulator: Emulator,
  lucid: LucidEvolution,
  userSeedPhrase: string,
  config: Config,
  requestInfo: RequestInfo
) => {
  lucid.selectWallet.fromSeed(userSeedPhrase);
  if (!config.advancedRouteRequestMaker)
    throw new Error("Swap request maker missing");
  const advancedRouteRequest = unsafeFromOk(
    await config.advancedRouteRequestMaker(requestInfo)
  );
  const requestConfig: RouteRequest = {
    kind: "advanced",
    data: advancedRouteRequest,
  };

  const singleConf: SingleRequestConfig = {
    scriptCBOR: config.scriptCBOR,
    routeRequest: requestConfig,
    additionalRequiredLovelaces: BigInt(0),
  };

  const requestUnsigned = unsafeFromOk(await singleRequest(lucid, singleConf));
  // console.log(requestUnsigned.data.txComplete.to_json());
  const requestSigned = await requestUnsigned.sign.withWallet().complete();
  console.log("SINGLE REQUEST TX:", requestSigned.toCBOR());
  const requestTxHash = await requestSigned.submit();
  console.log("SINGLE REQUEST TX HASH:", requestTxHash);

  emulator.awaitBlock(100);
};
