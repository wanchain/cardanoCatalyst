// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;


interface IBusinessInterface {
    function checkBusinessData(bytes calldata businessData) external view returns (bool);
}
