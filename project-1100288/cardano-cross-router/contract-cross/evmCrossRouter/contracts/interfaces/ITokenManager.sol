// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;


interface ITokenManager {
    function getTokenPairInfo(uint id) external view returns (uint fromChainID, bytes memory fromAccount, uint toChainID, bytes memory toAccount);

}
