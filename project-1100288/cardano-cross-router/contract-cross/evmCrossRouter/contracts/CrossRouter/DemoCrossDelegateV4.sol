// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;

import '../interfaces/ICrossDelegateInterfaceV4.sol';

contract DemoCrossDelegateV4 is ICrossDelegateInterfaceV4 {

  //******************************************
  // 辅助测试
  address test_tokenAccount;
  function setTokenAccount(address _tokenAccount) public {
    test_tokenAccount = _tokenAccount;
  }
  //

  event UserLockLogger(bytes32 indexed smgID, uint indexed tokenPairID, address indexed tokenAccount, uint value, bytes userAccount);
  event UserBurnLogger(bytes32 indexed smgID, uint indexed tokenPairID, uint value, address tokenAccount, uint fee, bytes userAccount);

  function userLock(bytes32 smgID, uint tokenPairID, uint value, bytes calldata userAccount) 
    override
    external
    payable
  {
    emit UserLockLogger(smgID, tokenPairID, test_tokenAccount, value, userAccount);
  }

  function userBurn(bytes32 smgID, uint tokenPairID, uint value, uint fee, address tokenAccount, bytes calldata userAccount) 
    override
    external
    payable
  {
    emit UserBurnLogger(smgID, tokenPairID, value, tokenAccount, fee, userAccount);
  }
}

