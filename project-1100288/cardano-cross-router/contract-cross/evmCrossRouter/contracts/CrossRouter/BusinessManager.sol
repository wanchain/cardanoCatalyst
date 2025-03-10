// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;

import '@openzeppelin/contracts/access/AccessControl.sol';
import "../lib/BusinessDataLib.sol";

contract BusinessManager is AccessControl{
    bytes32 public constant ROUTE_ADMIN_ROLE = keccak256("ROUTE_ADMIN_ROLE");
    bytes32 public constant FEE_ADMIN_ROLE = keccak256("FEE_ADMIN_ROLE");

    mapping(bytes => BusinessDataLib.RouteInfo) public mapRouteInfo;

    event SetRouteInfo(bytes routeIn, address addrBusiness);
    event SetRouteFees(bytes routeIn, uint256 feeAda, uint256 feeNative);

    constructor(address _admin, address _businessAdmin, address _feeAdmin) {
        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(ROUTE_ADMIN_ROLE, _businessAdmin);
        _grantRole(FEE_ADMIN_ROLE, _feeAdmin);
    }

    function setRouteInfo(bytes memory routeIn, address businessAddr) external onlyRole(ROUTE_ADMIN_ROLE) {
      require(routeIn.length != 0, "setRouteInfo routeIn is zero");
      require(businessAddr != address(0), "setRouteInfo businessAddr is zero");

      mapRouteInfo[routeIn].businessAddr = businessAddr;
      emit SetRouteInfo(routeIn, businessAddr);
    }

    function setRouteFees(bytes calldata routeIn, uint256 feeAda, uint256 feeNative) external onlyRole(FEE_ADMIN_ROLE) {
      require(routeIn.length != 0, "setRouteFees routeIn is zero");
      require(mapRouteInfo[routeIn].businessAddr != address(0), "setRouteFees businessAddr is zero");

      mapRouteInfo[routeIn].feeAda = feeAda;
      mapRouteInfo[routeIn].feeNative = feeNative;

      emit SetRouteFees(routeIn, feeAda, feeNative);
    }
}

