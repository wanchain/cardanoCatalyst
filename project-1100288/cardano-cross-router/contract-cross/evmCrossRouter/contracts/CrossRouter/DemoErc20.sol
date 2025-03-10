// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/access/Ownable.sol";


contract DemoErc20 is ERC20("DemoErc20", "DemoErc20") {
    function mint(address _to, uint value) public {
        _mint(_to, value);
    }
}


