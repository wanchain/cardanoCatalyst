# Example Swap Scenario Using Smart Handles

This is a sample script for
utilizing [Anastasia Labs](https://anastasialabs.com)' [Smart Handles contract](https://github.com/Anastasia-Labs/smart-handles) and [SDK](https://github.com/Anastasia-Labs/smart-handles-offchain) for
swapping a few [tUSDC](https://preprod.cexplorer.io/asset/asset1wjnjqe7lkceg7xemx8j06rkw6ez8jnxqeme8zn) tokens
for [tHOSKY](https://preprod.cexplorer.io/asset/asset15qks69wv4vk7clnhp4lq7x0rpk6vs0s6exw0ry) tokens
on Cardano's preprod testnet.

## How to Use

1. Compile the script:
```sh
npm run build
```

2. Set these three environment variables:
```sh
export BLOCKFROST_KEY="your blockfrost api key"
export SEED_PHRASE="user's wallet seed phrase"
export ROUTING_SEED_PHRASE="wallet seed phrase of the routing agent"
```

3. Make sure user's wallet has at least 750 tUSDC available, since the scenario
   submits 5 swap requests with varying input amounts (50, 100, 150, 200, and
   250)

4. Run the compiled script (produced in `dist` directory next to `src` here):
```sh
node dist/index.js
```

## On-Chain Results of a Run

Multiple swap requests, submitted by the user in a single transaction:
[`357ed9fb96a4751a247236f5550ca159098d2d019ec9989f4a471f36cb7d0027`](https://preprod.cexplorer.io/tx/357ed9fb96a4751a247236f5550ca159098d2d019ec9989f4a471f36cb7d0027)

Multiple routing performed by the agent in a single transaction:
[`8c7dc90f95486d66caa216d2e29ec68dc4f5d50cff3428ba469ee3877736b633`](https://preprod.cexplorer.io/tx/8c7dc90f95486d66caa216d2e29ec68dc4f5d50cff3428ba469ee3877736b633)
