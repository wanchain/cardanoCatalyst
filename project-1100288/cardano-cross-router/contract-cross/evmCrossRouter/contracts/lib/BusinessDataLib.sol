// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;


library BusinessDataLib {
    struct RouteInfo {
      address     businessAddr;
      uint256     feeAda;
      uint256     feeNative;
    }

    struct RouteDataParam {
      address     outReceiveAddr;
      uint256     outTokenPairID;
      bytes       constraintCBOR;

      RouteInfo   routeInfo;
    }
}

