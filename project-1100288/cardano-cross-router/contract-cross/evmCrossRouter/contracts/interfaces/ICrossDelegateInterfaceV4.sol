// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;

interface ICrossDelegateInterfaceV4 {
    function userLock(bytes32 smgID, uint tokenPairID, uint value, bytes calldata userAccount) external payable;
    function userBurn(bytes32 smgID, uint tokenPairID, uint value, uint fee, address tokenAccount, bytes calldata userAccount) external payable;
}

