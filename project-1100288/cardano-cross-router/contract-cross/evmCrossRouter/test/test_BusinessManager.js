
const BusinessManager = artifacts.require("BusinessManager");

const Utils = require("./utils");


before("init... -> success", () => {
    let network = global["network"];
    console.log("network:", network);
});


it('BusinessManager ==>  success', async () => {
  let network = global["network"];

  // deploy
  let BusinessManagerJson = BusinessManager._json;
  let businessManager = new web3.eth.Contract(BusinessManagerJson.abi);
  let deployRet = await businessManager.deploy( { data: BusinessManagerJson.bytecode, arguments: [network.admin, network.admin, network.admin] })
                                    .send({
                                           from: network.deployer,
                                           gasPrice: '20000000000',// ganacle-cli default value
                                           gas: 6721975            // ganache-cli default value
                                     });
  let scAddr = deployRet._address;
  console.log("deployRet._address:", deployRet._address);
  Utils.setGlobal("BusinessManagerAddr", scAddr);

  // instance
  let businessManagerInst = new web3.eth.Contract(BusinessManagerJson.abi, scAddr);
  //console.log("businessManagerInst:", businessManagerInst);

  // test 
  let businessV1Addr = global["businessV1Addr"];
  let receipt = await businessManagerInst.methods.setRouteInfo(network.routeAddress, businessV1Addr)
           .send({
              from: network.admin,
              gasPrice: '20000000000',// ganacle-cli default value
              gas: 6721975            // ganache-cli default value
           });
  assert.equal(receipt.status, true, " RouterHelperManager SetRouteHelper failure ");

  // check Event
  console.log("receipt:", receipt);
  let evLog = receipt.events.SetRouteInfo;
  console.log("evLog:", evLog);
  assert.equal(evLog.returnValues.routeIn, network.routeAddress, "check SetRoute routeAddress fail");
  console.log("evLog.returnValues:", evLog.returnValues);
  console.log("businessV1Addr:", businessV1Addr);
  assert.equal(evLog.returnValues.addrBusiness, businessV1Addr, "check SetRoute addrBusiness fail");

  {
    // 另一种方式
    let eventString = "SetRouteInfo(bytes,address)";
    let SetRouteInfoSignature = web3.utils.keccak256(eventString);
    console.log("SetRouteInfoSignature:", SetRouteInfoSignature);

    receipt = await web3.eth.getTransactionReceipt(receipt.transactionHash);
    console.log("receipt.logs:", receipt.logs);
    console.log("receipt.logs[0].topics:", receipt.logs[0].topics);
    assert.equal(SetRouteInfoSignature, receipt.logs[0].topics, "check SetRouteInfoSignature fail");

    let eventLogData = web3.eth.abi.decodeParameters(["bytes","address"], receipt.logs[0].data);
    console.log("eventLogData:", eventLogData);
  }

  // test 
  // expect fail
  try {
    receipt = await routeHelperManagerInst.methods.SetRouteHelper(network.routeAddress, routeHelperV1Addr)
            .send({
                from: network.deployer,
                gasPrice: '20000000000',// ganacle-cli default value
                gas: 6721975            // ganache-cli default value
            });
  }
  catch(err) {
    console.log("revert");
  }
});


