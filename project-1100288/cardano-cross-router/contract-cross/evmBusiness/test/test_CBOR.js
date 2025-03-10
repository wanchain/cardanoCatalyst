

const TestCBOR = artifacts.require("TestCBOR");

const Utils = require("./utils");
let BigNumber = require("bignumber.js");
// > const cbor = require('cbor');
// > const myData = {a: '1', b: '2', c: [3,4,5]};
// > cbor.encode(myData).toString('hex');
// // Your CBOR-encoded data:
const cbor_data = 'a36161613161626132616383030405';

const CBOR = require("cbor");


before("init... -> success", () => {
    let network = global["network"];
    console.log("network:", network);
});

it('test CBOR ==> success', async () => {
  const myData = {
    a: '1',
    b: '2',
    c: [3,4,5]
  };
  console.log("myData:", myData);
  let cborStr = CBOR.encode(myData).toString("hex");
  console.log("cborStr:", cborStr);
  assert.equal(cborStr, cbor_data, "test encode_CBOR Error");

  let decodeObj = CBOR.decode(Buffer.from(cborStr,"hex"));
  console.log("decodeObj:", decodeObj);

  assert.equal(JSON.stringify(myData), JSON.stringify(decodeObj), "test decode_CBOR Error");

  //****************************************************
  
  // deploy
  let TestCBORJson = await TestCBOR._json;
  let testCBORSc = new web3.eth.Contract(TestCBORJson.abi);
  let deployRet = await testCBORSc.deploy( { data: TestCBORJson.bytecode, arguments: [] })
    .send({
         from: network.deployer,
         gasPrice: '20000000000',// ganacle-cli default value
         gas: 6721975            // ganache-cli default value
     });
  console.log("deployRet._address:", deployRet._address);
  let scAddr = deployRet._address;

  // instance
  let testCBORInst = new web3.eth.Contract(TestCBORJson.abi, scAddr);
  
  // check VERSION
  {
    console.log("\n\n");
    let version = await testCBORInst.methods.VERSION().call();
    console.log("version:", version);
    let bn_ver = new web3.utils.BN(version);
    let cn_val = new web3.utils.BN(1);
    assert.equal(bn_ver.eq(cn_val), true, "CborTest.VERSION Error");
  }

  // checkBusinessData
  let constraintCBOR;
  {
    console.log("\n\n");
    constraintCBOR = web3.eth.abi.encodeParameters(["uint256"], ["0x12345f"]);
    let bRet = await testCBORInst.methods.checkBusinessData(constraintCBOR).call();
    console.log("bRet:", bRet);
  }

  // getBusinessDataInCBOR
  {
    console.log("\n\n");
    console.log("constraintCBOR:", constraintCBOR);
    let cborBytesHex = await testCBORInst.methods.getBusinessDataInCBOR(constraintCBOR).call();
    console.log("cborBytesHex:", cborBytesHex);

    let cobrBytes = Buffer.from(cborBytesHex.slice(2), "hex");
    console.log("cobrBytes:", cobrBytes);
    var decoded = await CBOR.decodeFirst(cobrBytes);
    console.log("decoded:", decoded);
    let bn_amountOutMin = new BigNumber(decoded.amountOutMin);
    console.log("bn_amountOutMin:", bn_amountOutMin);
    console.log("bn_amountOutMin:", bn_amountOutMin.toString());
  }

  // testCBOR
  {
    console.log("\n\n");
    cborBytesHex = await testCBORInst.methods.testCBOR().call();
    console.log("cborBytesHex:", cborBytesHex);

    cobrBytes = Buffer.from(cborBytesHex.slice(2), "hex");
    console.log("cobrBytes:", cobrBytes);
    var decoded = await CBOR.decodeFirst(cobrBytes);
    console.log("decoded:", decoded);
    assert.deepEqual(decoded, {
        'key1': 'value1',
        'long': 'This string is longer than 24 characters.',
        'bytes': Buffer.from('Test'),
        'true': true,
        'false': false,
        'null': null,
        'undefined': undefined,
        'array': [0, 1, 23, 24, 0x100, 0x10000, 0x100000000, -42]
    });
  }

  // testCBOR_1
  {
    console.log("\n\n");
    console.log("testCBOR_1");
    let call_result = await testCBORInst.methods.testCBOR_1().call();
    console.log("call_result:", call_result);
    console.log("call_result['0']:", Buffer.from(call_result['0'].slice(2), "hex").toString());
    console.log("call_result['1']:", Buffer.from(call_result['1'].slice(2), "hex").toString());
  }

  // testCBOR_2
  {
    console.log("\n\n");
    console.log("testCBOR_2");
    let call_result = await testCBORInst.methods.testCBOR_2().call();
    console.log("call_result:", call_result);
    // console.log("call_result['0']:", Buffer.from(call_result['0'].slice(2), "hex").toString());
    // console.log("call_result['1']:", Buffer.from(call_result['1'].slice(2), "hex").toString());
  }
});
