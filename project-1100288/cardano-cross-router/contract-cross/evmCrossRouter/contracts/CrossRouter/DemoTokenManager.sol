// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;
import "../interfaces/ITokenManager.sol";

contract DemoTokenManager is ITokenManager {
  struct TokenPairInfo {
    uint      fromChainID;            // index in coinType.txt; e.g. eth=60, etc=61, wan=5718350
    bytes     fromAccount;            // from address
    uint      toChainID;              //
    bytes     toAccount;              // to token address
  }

  // a map from a sequence ID to token pair
  mapping(uint => TokenPairInfo) public mapTokenPairInfo;

  event AddTokenPair(uint id, uint fromChainID,  bytes fromAccount, uint toChainID, bytes toAccount);
  // 辅助测试
  function addTokenPair(uint id, uint fromChainID, bytes calldata fromAccount, uint toChainID, bytes calldata toAccount)
    public
  {
      // create a new record
      mapTokenPairInfo[id].fromChainID = fromChainID;
      mapTokenPairInfo[id].fromAccount = fromAccount;
      mapTokenPairInfo[id].toChainID = toChainID;
      mapTokenPairInfo[id].toAccount = toAccount;

      // fire event
      emit AddTokenPair(id, fromChainID, fromAccount, toChainID, toAccount);
  }
  ////*******************************************************************************************************************

  function getTokenPairInfo(uint id)
    override
    external
    view
    returns (uint fromChainID, bytes memory fromAccount, uint toChainID, bytes memory toAccount)
  {
    fromChainID = mapTokenPairInfo[id].fromChainID;
    fromAccount = mapTokenPairInfo[id].fromAccount;
    toChainID = mapTokenPairInfo[id].toChainID;
    toAccount = mapTokenPairInfo[id].toAccount;
  }
}
