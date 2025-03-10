

const BusinessV1 = artifacts.require("BusinessV1");


const Utils = require("./utils");


before("init... -> success", () => {
    let network = global["network"];
    console.log("network:", network);
});

it('BusinessV1 ==> checkData success', async () => {
  let network = global["network"];

  // deploy
  let BusinessV1Json = await BusinessV1._json;
  let businessV1Sc = new web3.eth.Contract(BusinessV1Json.abi);
  let deployRet = await businessV1Sc.deploy( { data: BusinessV1Json.bytecode, arguments: [] })
    .send({
         from: network.deployer,
         gasPrice: '20000000000',// ganacle-cli default value
         gas: 6721975            // ganache-cli default value
     });
  //console.log("deployRet._address:", deployRet._address);
  let scAddr = deployRet._address;

  // instance
  let businessV1Inst = new web3.eth.Contract(BusinessV1Json.abi, scAddr);
  Utils.setGlobal("businessV1Addr", scAddr);
  
  // check VERSION
  let version = await businessV1Inst.methods.VERSION().call();
  console.log("version:", version);
  let bn_ver = new web3.utils.BN(version);
  let cn_val = new web3.utils.BN(1);
  assert.equal(bn_ver.eq(cn_val), true, "BusinessV1.VERSION Error");

  // checkData
  let constraintCBOR = web3.eth.abi.encodeParameters(["uint256"], ["12345"]);
  let bRet = await businessV1Inst.methods.checkBusinessData(constraintCBOR).call();
  console.log("bRet:", bRet);
  // let params = ["uint256", "address", "uint256"];
  // let decode_params_val = web3.eth.abi.decodeParameters(params, res_checkData);
  // console.log("decode_params_val:", decode_params_val);
});


