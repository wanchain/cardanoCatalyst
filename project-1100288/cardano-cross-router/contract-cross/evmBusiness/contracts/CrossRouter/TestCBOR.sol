// SPDX-License-Identifier: MIT
// Wanchain

pragma solidity ^0.8.0;

// 只有CBOR编码,没有解码
import "solidity-cborutils/contracts/CBOR.sol";
// https://www.npmjs.com/package/solidity-cborutils
// https://github.com/smartcontractkit/solidity-cborutils/blob/master/test/TestCBOR.sol


import "../interfaces/IBusinessInterface.sol";
import "../lib/BusinessDataLib.sol";

// 出错
// import "../solidity-cbor/CBORDecoding.sol";
// import "../solidity-cbor/ByteParser.sol";
// https://owlprotocol.github.io/solidity-cbor/docs/quickstart
// https://github.com/owlprotocol/react-snake-game/blob/develop/solidity/contracts/SnakeGameRewards.sol
// https://owlprotocol.github.io/solidity-cbor/docs/


// 该项目对CBOR的使用，主要是字段以固定顺序组成数组并解析，
// 并不支持普通Object对象的智能解析
// 对比 CborDecode.sol和CBOR.sol，可以看出CborDecode.sol不支持MajMap=5(CBOR.sol中MAJOR_TYPE_MAP=5)
import "../lib/CborDecode.sol";
// https://github.com/filecoin-project/filecoin-solidity
// https://docs.filecoin.io/smart-contracts/developing-contracts/filecoin.sol
// TestCBOR_1


import "../lib/WitnetCBOR.sol";
// https://scrollscan.com/address/0x0dAc06Ca814b6238524FcB2044108fa112aDf199#code
// https://github.com/witnet/witnet-solidity-bridge/blob/master/contracts/libs/WitnetDecoderLib.sol
// TestCBOR_2 
// 参看witnet-solidity-bridge项目中的TestWitnetCBOR.sol,不是期望的解码方式


contract TestCBOR is IBusinessInterface {
  using CBOR for CBOR.CBORBuffer;
  using CBORDecoder for bytes;

  using WitnetCBOR for WitnetCBOR.CBOR;

  uint256 public constant VERSION = 1;
  function checkBusinessData(bytes calldata businessData) override external pure returns (bool) {
    abi.decode(businessData, (BusinessDataLib.BUSINESS_CBOR_V1));
    return true;
  }

  function getBusinessDataInCBOR(bytes calldata businessData) external pure returns (bytes memory) {
    BusinessDataLib.BUSINESS_CBOR_V1 memory cbor_v1 = abi.decode(businessData, (BusinessDataLib.BUSINESS_CBOR_V1));

    CBOR.CBORBuffer memory buf = CBOR.create(64);
    buf.startMap();

    buf.writeKVUInt256("amountOutMin", cbor_v1.amountOutMin);

    buf.endSequence();
    return buf.data();
  }

  function testCBOR_1() external pure returns (bytes memory, bytes memory) {
    // encode
    CBOR.CBORBuffer memory buf = CBOR.create(64);
    buf.startFixedArray(2);
    buf.writeString("key_1");
    buf.writeString("value_1");
    bytes memory cobrData = buf.data();// 不需要调用buf.endSequence();

    // decode
    uint byteIdx = 0;
    uint len;
    bytes memory tmp;
    
    // CBORDecoder 测试
    (len, byteIdx) = cobrData.readFixedArray(byteIdx);
    assert(len == 2);

    string memory strKey;
    (strKey, byteIdx) = cobrData.readString(byteIdx);

    string memory strVal;
    (strVal, byteIdx) = cobrData.readString(byteIdx);

    return (bytes(strKey), bytes(strVal));
  }

  function testCBOR_2() external pure returns (string memory, string memory) {
    // encode
    CBOR.CBORBuffer memory buf = CBOR.create(64);
    buf.writeString("key_1");
    buf.writeString("value_1");
    bytes memory cborData = buf.data();

    // decode 
    WitnetCBOR.CBOR memory witnet_cbor = WitnetCBOR.fromBytes(cborData);
    string memory key = witnet_cbor.readString();
    string memory val = witnet_cbor.readString();// 错误，读出来值为：call_result: Result { '0': 'key_1', '1': 'gvalu' }

    // return buf.data();
    return (key, val);
  }

  function testCBOR() external pure returns (bytes memory) {
    CBOR.CBORBuffer memory buf = CBOR.create(64);
    // Maps
    buf.startMap();

    // Short strings
    buf.writeKVString("key1", "value1");

    // Longer strings
    buf.writeKVString("long", "This string is longer than 24 characters.");

    // Bytes
    buf.writeKVBytes("bytes", bytes("Test"));

    // Bools, null, undefined
    buf.writeKVBool("true", true);
    buf.writeKVBool("false", false);
    buf.writeKVNull("null");
    buf.writeKVUndefined("undefined");

    // Arrays
    {
      buf.writeKVArray("array");
        buf.writeUInt64(0);
        buf.writeUInt64(1);
        buf.writeUInt64(23);
        buf.writeUInt64(24);

        // 2, 4, and 8 byte numbers.
        buf.writeUInt64(0x100);
        buf.writeUInt64(0x10000);
        buf.writeUInt64(0x100000000);

        // Negative numbers
        buf.writeInt64(-42);

      buf.endSequence();
    }
    buf.endSequence();

    return buf.data();
  }
}

