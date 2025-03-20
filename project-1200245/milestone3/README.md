# Milestone 3

For Milestone 3, the acceptance criteria required that:

1. the external auditing team conduct a line-by-line review of Wanchain’s Cardano Bridge’s Haskell smart contracts on-chain code until no unresolved issues designated as "Critical" remain.

<ins>Summary</ins>

The Wanchain Bridge was thoroughly audited by TxPipe, with specific focus on the smart contracts responsible for managing assets being transferred from and to the Cardano Blockchain. There are no unresolved "Critical" issues.

<ins>Audited Files</ins>

Below is a list of all audited files in this report. Any files not listed here were not audited.
The final state of the files for the purposes of this report is considered to be commit
71f4424b8e87201cb137355cc16fc9e55a1d110a.

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

<ins>Findings</ins>

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

<ins>Severity Guide</ins>

| Severity | Description |
| ------------- | ------------- |
| Critical | Critical issues highlight exploits, bugs, loss of funds, or other vulnerabilities that prevent the dApp from working as intended. These issues have no workaround. |
| Major | Major issues highlight exploits, bugs, or other vulnerabilities that cause unexpected transaction failures or may be used to trick general users of the dApp. dApps with Major issues may still be functional. |
| Minor | Minor issues highlight edge cases where a user can purposefully use the dApp in a non-incentivized way and often lead to a disadvantage for the user.  |
| Info | Info are not issues. These are just pieces of information that are beneficial to the dApp creator. These are not necessarily acted on or have a resolution, they are logged for the completeness of the audit. |

<ins>Status Guide</ins>

| Status | Description |
| ------------- | ------------- |
| Resolved | Issues that have been fixed by the project team. |
| Acknowledged | Issues that have been acknowledged or partially fixed by the project team. Projects can decide to not fix issues for whatever reason. |
| Identified | Issues that have been identified by the audit team. These are waiting for a response from the project team. |
