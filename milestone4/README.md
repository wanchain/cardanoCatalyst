# Milestone 4

For Milestone 4, the acceptance criteria required the evidence to:

1.	demonstrate that a smart beacon script on Cardano can trigger a cross-chain transaction to bridge assets from Cardano to an EVM chain.

<ins>Evidence 1a: https://preprod.cexplorer.io/tx/9770aa081d17094004445f9fcb0efe769bd52d9c0183adaa83cd2ba0486ed0c9</ins>

-	This is an on-chain transaction record showing that WAN has been swapped for ADA on Minswap. It is the same transaction record as in Milestone 3.

-	After swapping WAN to ADA (demonstrated in Milestone 3), the ADA is sent to addr_test1wqvsdtcc8fdvwm8frpa3juk70dpfzz4p9j3nn7f3dekj82gfq0qx7, a Smart Beacon Script address.

<ins>Evidence 1b: https://preprod.cexplorer.io/tx/56bbe9e9fe0d8195365b03e6a15b8c2c1068dbcacd283ff676e9296dac975cba</ins> 

-	This is an on-chain transaction record showing that the ADA obtained from swapping WAN to ADA on Minswap has been transferred to the Wanchain L1 Blockchain (an EVM-compatible blockchain) via a cross-chain transaction.

<ins>Evidence 1c: https://testnet.wanscan.org/tx/0xa0620a26b201fa184a31d3a8115551689421ac16c5cf6703106c3f5e615039c6</ins>

-	This is the corresponding transaction record for the cross-chain transaction. The initiating transaction hash (56bbe9e9fe0d8195365b03e6a15b8c2c1068dbcacd283ff676e9296dac975cba) matches the transaction record in Evidence 1b.
-	This transaction record also proves that the wallet (0x07b2e0a113fD1E3ca765C217F5c4efA28d22eeDF) that initiated the original WAN to ADA cross-chain swap (the sending address in Milestone 2) is the same wallet that received the ADA.
