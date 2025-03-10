import {
  batchRequest,
  BatchRequestConfig,
  Blockfrost,
  Lucid,
  toUnit,
  batchSwap,
  SwapConfig,
  fetchUsersBatchRequestUTxOs,
  BatchSwapConfig,
  Result,
  TxComplete,
  ReadableUTxO,
  SmartHandleDatum,
} from "@anastasia-labs/smart-handles-offchain";

const USDC_SYMBOL_PREPROD =
  "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72";
const HOSKY_SYMBOL_PREPROD =
  "f6f49b186751e61f1fb8c64e7504e771f968cea9f4d11f5222b169e3";
const USDC_TOKEN_NAME = "7455534443";
const HOSKY_TOKEN_NAME = "74484f534b59";
const USDC_HOSKY_LP_TOKEN_NAME_PREPROD =
  "51055a4ca596860d71faf34c8eefa8a05ea42456c6a16c1090c939fe1c379d92";

const blockfrostKey = process.env.BLOCKFROST_KEY;

// Seed phrases are space separated words

const seedPhrase = process.env.SEED_PHRASE;

const routingAgentsSeedPhrase = process.env.ROUTING_SEED_PHRASE;

if (!blockfrostKey)
  throw new Error("No Blockfrost API key was found (BLOCKFROST_KEY)");
if (!seedPhrase) throw new Error("No wallet seed phrase found (SEED_PHRASE)");
if (!routingAgentsSeedPhrase)
  throw new Error(
    "Routing agent's wallet seed phrase not found (ROUTING_SEED_PHRASE)"
  );

// Commented out as the Minswap version of the smart handles contract is already
// registered on preprod.
//
// const registerRewardAddress = async (
//   lucid: Lucid,
//   rewardAddress: string
// ): Promise<void> => {
//   const tx = await lucid.newTx().registerStake(rewardAddress).complete();
// 
//   const signedTx = await tx.sign().complete();
// 
//   const txHash = await signedTx.submit();
// 
//   await lucid.awaitTx(txHash);
// };

const signAndSubmitTxRes = async (
  lucid: Lucid,
  txRes: Result<TxComplete>
): Promise<string> => {
  if (txRes.type == "error") throw txRes.error;

  const txSigned = await txRes.data.sign().complete();

  const txHash = await txSigned.submit();

  await lucid.awaitTx(txHash);

  return txHash;
};

const run = async () => {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      blockfrostKey
    ),
    "Preprod"
  );

  lucid.selectWalletFromSeed(seedPhrase);

  const requestConfig: BatchRequestConfig = {
    swapRequests: [50, 100, 150, 200, 250].map((l) => ({
      fromAsset: toUnit(USDC_SYMBOL_PREPROD, USDC_TOKEN_NAME),
      quantity: BigInt(l),
      toAsset: toUnit(HOSKY_SYMBOL_PREPROD, HOSKY_TOKEN_NAME),
    })),
    testnet: true,
  };

  const requestTxUnsignedRes = await batchRequest(lucid, requestConfig);

  console.log("Submitting the swap requests...");
  const requestTxHash = await signAndSubmitTxRes(lucid, requestTxUnsignedRes);
  console.log(`Request Successfully Submitted: ${requestTxHash}`);

  // Commented out as the Minswap version of the smart handles contract is
  // already registered on preprod.

  // const batchVAsRes = getBatchVAs(lucid, true);

  // if (batchVAsRes.type == "error") return batchVAsRes;

  // const batchVAs: BatchVAs = batchVAsRes.data;

  // const rewardAddress = batchVAs.stakeVA.address;

  // console.log("Registering the staking validator...");
  // await registerRewardAddress(lucid, rewardAddress);
  // console.log(`Staking validator successfully registered: ${rewardAddress}`);

  const userAddress = await lucid.wallet.address();

  lucid.selectWalletFromSeed(routingAgentsSeedPhrase);
  console.log("(switched to the routing agent's wallet)");

  console.log("Fetching user's batch requests...");
  const usersRequests = await fetchUsersBatchRequestUTxOs(
    lucid,
    userAddress,
    true
  );
  console.log("Fetch completed:");
  console.log(usersRequests);

  const swapConfig: SwapConfig = {
    blockfrostKey,
    poolId: USDC_HOSKY_LP_TOKEN_NAME_PREPROD,
    slippageTolerance: BigInt(10),
  };

  const batchSwapConfig: BatchSwapConfig = {
    swapConfig,
    requestOutRefs: usersRequests.map((u: ReadableUTxO<SmartHandleDatum>) => u.outRef),
    testnet: true,
  };

  const swapTxUnsigned = await batchSwap(lucid, batchSwapConfig);

  console.log("Submitting the swap transaction...");
  const swapTxHash = await signAndSubmitTxRes(lucid, swapTxUnsigned);
  console.log(`Swap successfully performed: ${swapTxHash}`);
};

run();
