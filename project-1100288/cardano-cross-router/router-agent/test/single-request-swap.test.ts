import {
  Emulator,
  Lucid,
  singleReclaim,
  fetchSingleRequestUTxOs,
  SingleReclaimConfig,
  getAddressDetails,
  SingleRouteConfig,
  singleRoute,
} from "@anastasia-labs/smart-handles-offchain";
import { beforeEach, test } from "vitest";
import { mkConfig } from "../src/router-in/minswap/config.js";
import {
  LucidContext,
  createUser,
  submitRequestInfo,
  unsafeFromOk,
} from "./utils.js";
import { reclaimExtraConfig } from "./constants.js";
import { RequestInfo } from "@anastasia-labs/smart-handles-agent";
import { PREPROD_MINTING_POLICY, PREPROD_TREASURY_ADDRESS } from "../src/constants.js";
import {MIN_TOKEN_NAME} from "../src/router-in/minswap/constants.js";

const config = mkConfig("Custom", "Single", true);

const assetToSwap = {
  [PREPROD_MINTING_POLICY + MIN_TOKEN_NAME]: BigInt(100_000_000),
};

// INITIALIZE EMULATOR + ACCOUNTS
beforeEach<LucidContext>(async (context) => {
  context.users = {
    user: createUser(assetToSwap),
    agent: createUser(),
  };
  context.emulator = new Emulator([context.users.agent, context.users.user]);

  context.lucid = await Lucid(context.emulator, "Custom");
});

test<LucidContext>("Test - Single Request, Swap", async ({
  lucid,
  users,
  emulator,
}) => {
  const treasuryAddressDetails = getAddressDetails(PREPROD_TREASURY_ADDRESS);
  const requestInfo: RequestInfo = {
    lovelace: BigInt(7_000_000),
    asset: assetToSwap,
    owner: treasuryAddressDetails,
    routerFee: BigInt(2_000_000),
    reclaimRouterFee: BigInt(1_000_000),
    extraConfig: reclaimExtraConfig,
  };
  await submitRequestInfo(
    emulator,
    lucid,
    users.user.seedPhrase,
    config,
    requestInfo
  );
  // ---------------------------------------------------------------------------

  lucid.selectWallet.fromSeed(users.agent.seedPhrase);
  const allRequests = await fetchSingleRequestUTxOs(
    lucid,
    config.scriptCBOR,
    "Custom"
  );
  console.log("ALL REQUESTS", allRequests);
  if (allRequests.length < 1) throw new Error("No swap requests found");

  // Route
  const routeConfig: SingleRouteConfig = {
    scriptCBOR: config.scriptCBOR,
    requestOutRef: allRequests[0],
    routeAddress: config.routeDestination,
    advancedRouteConfig: config.advancedRouteConfig,
  };
  const validRoute = unsafeFromOk(await singleRoute(lucid, routeConfig));
  const validRouteSigned = await validRoute.sign.withWallet().complete();
  try {
    const routeSignedHash1 = await validRouteSigned.submit();
  } catch (e) {
    console.log(JSON.stringify(e));
    throw e;
  }
}, 60_000);
