# Milestone 5

For the Final Miletone, the acceptance criteria required that:

1. a project closeout report;
2. a final closeout video be published.

## Project Closeout Report

### Name of the project and Project URL on IdeaScale/Fund

* Name: Wanchain's Cardano Bridge Audit

* URL: https://cardano.ideascale.com/c/cardano/idea/120062

### Your Project Number

* 1200245

### Name of the project manager

* Temujin Louie

### Date project started

* November 19, 2024

### Date project completed

* February 14, 2025 

### Deliverables

<i> Note: Specific KPIs are not relevant to this project. In lieu, the specific deliverables are outlined in this section.  </i>

_Summary Report_
     
  - The Wanchain Bridge was thoroughly audited by TxPipe, with specific focus on the smart contracts responsible for managing assets being transferred from and to the Cardano Blockchain. The summary audit report provided by TxPipe can be found here: https://github.com/wanchain/cardanoCatalyst/blob/main/project-1200245/milestone4/Audit%20Report%20Summary%20-%20TxPipe.pdf

_Audited Files_

  - Below is a list of all audited files in this report. Any files not listed here were not audited. The final state of the files for the purposes of this report is considered to be commit 71f4424b8e87201cb137355cc16fc9e55a1d110a.

| Filename |
| ------------- |
| ./CrossChain/NFTMappingToken.hs |
| ./CrossChain/GroupNFT.hs |
| ./CrossChain/Types2.hs |
| ./CrossChain/Treasury.hs |
| ./CrossChain/GroupNFTHolder.hs |
| ./CrossChain/NFTTreasuryCheck.hs |
| ./CrossChain/Types.hs |
| ./CrossChain/NFTRefHolder.hs |
| ./CrossChain/TreasuryCheck.hs |
| ./CrossChain/MappingToken.hs |
| ./CrossChain/NFTMintCheck.hs |
| ./CrossChain/NFTTreasury.hs |
| ./CrossChain/StakeCheck.hs |
| ./CrossChain/CheckToken.hs |
| ./CrossChain/MintCheck.hs |
| ./CrossChain/AdminNFTHolder.hs |
| ./CrossChain/StoremanStake.hs |

_Findings_

| ID | Title | Severity | Status |
| ------------- | ------------- | ------------- | ------------- |
| WAN-001 | Multiple check tokens can be locked in the same UTxO  | Critical  | Resolved |
| WAN-101 | Treasury UTxOs can contain more than one token  | Major  | Resolved |
| WAN-201 | Possible protocol lockdown by providing an invalid list of signatories   | Minor  | Acknowledged |
| WAN-301 | Prevent inclusion of reference scripts  | Info  | Acknowledged |
| WAN-302 | Mapping tokens could be minted during the mintCheckToken burn operations  | Info  | Acknowledged |
| WAN-303 | Multiple compilation warnings present  | Info  | Acknowledged |
| WAN-304 | Hlint style suggestions  | Info  | Acknowledged |
| WAN-305 | Commented code in all modules  | Info  | Acknowledged |

_Severity Guide_

| Severity | Description |
| ------------- | ------------- |
| Critical | Critical issues highlight exploits, bugs, loss of funds, or other vulnerabilities that prevent the dApp from working as intended. These issues have no workaround. |
| Major | Major issues highlight exploits, bugs, or other vulnerabilities that cause unexpected transaction failures or may be used to trick general users of the dApp. dApps with Major issues may still be functional. |
| Minor | Minor issues highlight edge cases where a user can purposefully use the dApp in a non-incentivized way and often lead to a disadvantage for the user.  |
| Info | Info are not issues. These are just pieces of information that are beneficial to the dApp creator. These are not necessarily acted on or have a resolution, they are logged for the completeness of the audit. |

_Status Guide_

| Status | Description |
| ------------- | ------------- |
| Resolved | Issues that have been fixed by the project team. |
| Acknowledged | Issues that have been acknowledged or partially fixed by the project team. Projects can decide to not fix issues for whatever reason. |
| Identified | Issues that have been identified by the audit team. These are waiting for a response from the project team. |

_Additional Evidence_

  - A formal agreement with the external auditing team, published on Twitter: https://x.com/wanchain_org/status/1869759693507141641
  - A summary post, published on Github, listing the total number of found issues and their designation can be found here: https://github.com/wanchain/cardanoCatalyst/tree/main/project-1200245/milestone2
  - A summary post, published on Github, listing all findings, their designation, and their status can be found here: https://github.com/wanchain/cardanoCatalyst/tree/main/project-1200245/milestone3
  - The open-sourced smart contracts can be found here: https://github.com/wanchain/cardano-crosschain-evo
	
_Number of identified issues and their designation_

  - Critical: 1
  - Major: 1
  - Minor: 1
  - Info: 5

_Number of unresolved issues and their designation_

  - Minor: 1
  - Info: 5

## Final thoughts/comments:

  - The audit was successfully completed in collaboration with TxPipe, who demonstrated professionalism and expertise throughout the process. Their thorough review provided valuable insights, reinforcing the security of the Cardano Bridge. Thank you to the Cardano community for your continued support. The Wanchain team looks forward to continuing to build secure, interoperable blockchain solutions that add value to the Cardano ecosystem and the blockchain industry at large.

## Links to other relevant project sources or documents

  - The summary audit report provided by TxPipe can be found here: https://github.com/wanchain/cardanoCatalyst/blob/main/project-1200245/milestone4/Audit%20Report%20Summary%20-%20TxPipe.pdf

## Link to Close-out video:
  - https://www.youtube.com/watch?v=er8EBISDTV0

