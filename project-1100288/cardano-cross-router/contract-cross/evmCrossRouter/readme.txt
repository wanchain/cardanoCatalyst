


test:
1 linux：export NODE_OPTIONS=--max_old_space_size=4096
  windows: set NODE_OPTIONS=--max_old_space_size=4096

2 ganache-cli -m 'skill level pulse dune pattern rival used syrup inner first balance sad' --host 0.0.0.0 --chainId 50

3 cd evmCrossRouter

4 exec: truffle test --compile-none --network localTest ./test/test_Contract.js
