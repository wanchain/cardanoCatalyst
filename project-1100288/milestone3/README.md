# Milestone 3

For Milestone 3, the acceptance criteria required the evidence to:

1.	demonstrate that a transaction on an EVM chain can pass data/datum to a Smart Beacon Script on Cardano, which then triggers an asset swap on a Cardano DEX.

<ins>Evidence 1a: https://testnet.wanscan.org/tx/0xb0fb9ff5a31122d0de385a799cf7dccab35a5a3bfbdc524df762727de2e99fc9</ins>

-	This is the on-chain transaction record for a WAN to ADA cross-chain swap. It is the same transaction record as in Milestone 2
-	The receiving address is on Cardano is addr_test1wqpdn49yjqj2ukqe8y7ljdf4z2vg2g7efl9aft3mvy6wrtghj6r64, a Smart Beacon Script address
	
![Milestone 3a](https://github.com/user-attachments/assets/8daf0771-e7db-4d5d-ac5f-dfac2887f2f6)

<ins>Evidence 1b: https://preprod.cardanoscan.io/transaction/37dd8eca74bd572ec744ab38753b34c8ebb539197530057080e0ce9b5159eac2</ins>

-	This is the corresponding on-chain transaction record on Cardano. It is the same transaction record as in Milestone 2. 
-	Upon receiving data/datum (demonstrated in Milestone 2), WAN is minted and sent to addr_test1wqpdn49yjqj2ukqe8y7ljdf4z2vg2g7efl9aft3mvy6wrtghj6r64, the Smart Beacon Script address identified as the receiving address on Cardano.

<ins>Evidence 1c: https://preprod.cexplorer.io/tx/00d5df1d964fbca3a773d49953d5809d4d558cda9924ff4aee743fc04f91b2be</ins>

-	This is an on-chain transaction record showing the WAN on Cardano moving to addr_test1wrdf2f2x8pq3wwk3yv936ksmt59rz94mm66yzge8zj9pk7s0kjph3, a Minswap address. Minswap is a Cardano DEX.

<ins>Evidence 1d: https://preprod.cexplorer.io/tx/9770aa081d17094004445f9fcb0efe769bd52d9c0183adaa83cd2ba0486ed0c9</ins>
-	This is an on-chain transaction record showing that WAN has been swapped for ADA on Minswap.
