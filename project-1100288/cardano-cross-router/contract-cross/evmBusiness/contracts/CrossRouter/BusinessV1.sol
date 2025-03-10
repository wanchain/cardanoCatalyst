// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;


//import "../solidity-cbor/CBORDecoding.sol";


import "../interfaces/IBusinessInterface.sol";
import "../lib/BusinessDataLib.sol";

contract BusinessV1 is IBusinessInterface {
  uint256 public constant VERSION = 1;
  function checkBusinessData(bytes calldata /*businessData*/) override external pure returns (bool) {

    return true;
  }
}

