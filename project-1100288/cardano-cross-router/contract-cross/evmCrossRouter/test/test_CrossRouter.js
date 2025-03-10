
let BigNumber = require("bignumber.js");

const CrossRouter = artifacts.require("CrossRouter");
const DemoCrossDelegateV4 = artifacts.require("DemoCrossDelegateV4")
const DemoTokenManager = artifacts.require("DemoTokenManager");
const DemoErc20 = artifacts.require("DemoErc20");

const Utils = require("./utils");

async function encode_for_test() {
  console.log("\n\n\n");
  console.log("encode_for_test");
  
  let constraintCBOR = web3.eth.abi.encodeParameters(["uint256"], ["12345"]);
  console.log("constraintCBOR:", constraintCBOR);

  // 跨回来的参数：outReceive/outTokenPairID/constraintCBOR
  routeData = web3.eth.abi.encodeParameters(["address", "uint256", "bytes"], [network.admin, 222, constraintCBOR]);
  console.log("routeData:", routeData);

  console.log("encode_for_test");
  console.log("\n\n\n");
}

before("init... -> success", () => {
    let network = global["network"];
    console.log("network:", network);
});


it('CrossRouter ==>  success', async () => {
  {
    //await encode_for_test();
  }
  let network = global["network"];
  // test Parameter
  let tokenPairID = 1;
  global["tokenPairID"] = tokenPairID;

  let smgID = "0x2";
  global["smgID"] = smgID;

  let fromChainID = 30000;
  global["fromChainID"] = fromChainID;

  let toChainID = 40000;
  global["toChainID"] = toChainID;

  let demoErc20_addr;

  let userAccount = "0x06";// 即 在cardano链上的 Route合约地址
  let routeAddress = userAccount;

  let routeData = "0x07";
  let cross_value = 8;

  let fee = "0x09";
  let feeNative = 456;

  // 1 deploy
  // 1.1 deploy DemoCrossDelegateV4
  let demoCrossDelegateV4_Inst = await DemoCrossDelegateV4.new();
  let demoCrossDelegateV4_addr = demoCrossDelegateV4_Inst.address;
  console.log("demoCrossDelegateV4_addr:", demoCrossDelegateV4_addr);

  // 1.2 deploy TokenManager
  let DemoTokenManager_Inst = await DemoTokenManager.new();
  //console.log("DemoTokenManager_Inst:", DemoTokenManager_Inst);
  let DemoTokenManager_addr = DemoTokenManager_Inst.address;
  console.log("DemoTokenManager_addr:", DemoTokenManager_addr);

  // deploy DemoErc20
  // *********************************************
  let demoErc20_Inst = await DemoErc20.new();
  demoErc20_addr = demoErc20_Inst.address;
  console.log("demoErc20_addr:", demoErc20_addr);
  global["demoErc20_addr"] = demoErc20_addr;
  
  // call DemoTokenManager.AddTokenPair
  // adTokenPaird(uint id, uint fromChainID, bytes calldata fromAccount, uint toChainID, bytes calldata toAccount)
  let addTokenPair_args = [tokenPairID, fromChainID, demoErc20_addr, toChainID, "0x5"];
  let tx_receipt = await DemoTokenManager_Inst.addTokenPair(...addTokenPair_args);
  // console.log("tx_receipt:", tx_receipt);
  assert.equal(tx_receipt.receipt.status, true, " DemoTokenManager addTokenPair failure ");
  let tokenPairInfo = await DemoTokenManager_Inst.getTokenPairInfo(1);
  console.log("tokenPairInfo:", tokenPairInfo);

  // 1.3 deploy CrossRouter
  let CrossRouterScJson = CrossRouter._json;
  let CrossRouterSc = new web3.eth.Contract(CrossRouterScJson.abi);
  let args = [demoCrossDelegateV4_addr, DemoTokenManager_addr, network.admin, network.admin, network.admin, network.admin];
  let deployRet = await CrossRouterSc.deploy( { data: CrossRouterScJson.bytecode, arguments: args })
                                    .send({
                                           from: network.deployer,
                                           gasPrice: '20000000000',// ganacle-cli default value
                                           gas: 6721975            // ganache-cli default value
                                     });
  let crossRouter_addr = deployRet._address;
  console.log("crossRouter_addr:", crossRouter_addr);
  Utils.setGlobal("crossRouter_addr", crossRouter_addr);
  // instance
  let crossRouterInst = new web3.eth.Contract(CrossRouterScJson.abi, crossRouter_addr);

  {
    // SetRouteHelper
    // deploy RouteHelperV1
    let businessV1_addr = global["businessV1Addr"];
    console.log("businessV1_addr", businessV1_addr);
    let receipt = await crossRouterInst.methods.setRouteInfo(routeAddress, businessV1_addr)
           .send({
              from: network.admin,
              gasPrice: '20000000000',// ganacle-cli default value
              gas: 6721975            // ganache-cli default value
           });
    assert.equal(receipt.status, true, " setRouteInfo failure ");
    // console.log("receipt:", receipt);
    let SetRouteInfo_EvetData = web3.eth.abi.decodeParameters(["bytes","address"], receipt.events.SetRouteInfo.raw.data);
    console.log("SetRouteInfo_EvetData:", SetRouteInfo_EvetData);

    // 
    feeNative = 456;
    args = [routeAddress, 123, feeNative];
    // setRouteFees(bytes calldata routeIn, uint256 feeAda, uint256 feeNative) external onlyRole(FEE_ADMIN_ROLE) {
    receipt = await crossRouterInst.methods.setRouteFees(...args)
           .send({
              from: network.admin,
              gasPrice: '20000000000',// ganacle-cli default value
              gas: 6721975            // ganache-cli default value
           });
    assert.equal(receipt.status, true, " SetRouteHelper failure ");
    let SetRouteFees_EvetData = web3.eth.abi.decodeParameters(["bytes","uint256", "uint256"], receipt.events.SetRouteFees.raw.data);
    console.log("SetRouteFees_EvetData:", SetRouteFees_EvetData);


  }

  // 2 test 
  // 2.1
  // 2.1.1 call crossUserLock
  // prepare erc20 token
  tx_receipt = await demoErc20_Inst.mint(network.admin, cross_value);
  // console.log("DemoErc20 mint tx_receipt:", tx_receipt);
  let admin_balance = await demoErc20_Inst.balanceOf(network.admin);
  console.log("ERC20 admin_balance.toString:", admin_balance.toString());

  let demoErc20_Inst_2 = new web3.eth.Contract(DemoErc20._json.abi, demoErc20_addr);
  // console.log("demoErc20_Inst_2:", demoErc20_Inst_2);
  console.log("crossRouter_addr:", crossRouter_addr);
  console.log("cross_value:", cross_value);
  tx_receipt = await demoErc20_Inst_2.methods.approve(crossRouter_addr, cross_value).send({
            from: network.admin,
            gasPrice: '20000000000',// ganacle-cli default value
            gas: 6721975            // ganache-cli default value
        });
  //console.log("DemoErc20 approve tx_receipt:", tx_receipt);
  let admin_to_crossRouter_allowance = await demoErc20_Inst.allowance(network.admin, crossRouter_addr);
  console.log("admin_to_crossRouter_allowance_1:", admin_to_crossRouter_allowance.toString());

  let msg_value = web3.utils.toWei('1', 'ether'); // 1 ether
  //console.log("msg_value:", msg_value);
  let balance_admin = await web3.eth.getBalance(network.admin);
  //console.log("before crossUserLock balance_admin :", balance_admin);
  let constraintCBOR = web3.eth.abi.encodeParameters(["uint256"], ["12345"]);
  console.log("constraintCBOR:", constraintCBOR);
  // 跨回来的参数：outReceive/outTokenPairID/constraintCBOR
  routeData = web3.eth.abi.encodeParameters(["address", "uint256", "bytes"], [network.admin, 222, constraintCBOR]);
  console.log("routeData:", routeData);
  let args_crossUserLock = [smgID, tokenPairID, cross_value, userAccount, routeData];
  let receipt = await crossRouterInst.methods.crossUserLock(...args_crossUserLock)
           .send({
              from: network.admin,
              gasPrice: '20000000000',// ganacle-cli default value
              gas: 6721975,           // ganache-cli default value
              value: msg_value
           });
  //console.log("crossUserLock receipt:", receipt);
  assert.equal(receipt.status, true, " CrossRouter crossUserLock failure ");
  console.log("receipt:", receipt);

  {
    // check Balance
    console.log("msg_value:", msg_value);
    balance_admin = await web3.eth.getBalance(network.admin);
    console.log("ether after before crossUserLock balance_admin :", balance_admin); 

    // expect 0
    let balance_crossRouter = await web3.eth.getBalance(crossRouter_addr);
    console.log("ether balance_crossRouter:", balance_crossRouter.toString());
    assert.equal(balance_crossRouter.toString(), feeNative.toString(), "check after crossUserLock ether balance_crossRouter fail");

    // expece 1 ether
    let balance_crossDelegate = await web3.eth.getBalance(demoCrossDelegateV4_addr);
    console.log("ether balance_crossDelegate:", balance_crossDelegate.toString());
    let bn_msgvalue = new BigNumber(msg_value);
    let bn_feeNative = new BigNumber(feeNative);
    let bn_balance = bn_msgvalue.minus(bn_feeNative);
    console.log("bn_balance:", bn_balance.toString());
    assert.equal(balance_crossDelegate.toString(), bn_balance.toString(), "check after crossUserLock ether  balance_crossDelegate fail");
  }

  // 2.1.2 check Event
  console.log("crossUserLock receipt.events:", receipt.events);
  {
    // CrossRouter Event CrossUserLock
    let event_crossUserLock = receipt.events.CrossUserLock;
    //console.log("event_crossUserLock:", event_crossUserLock);
    // (bytes indexed userAccount, address outReceiveAddr, uint256 outTokenPairID, uint256 feeAda, uint256 feeNative, bytes constraintCBOR);
    // let eventLogData = web3.eth.abi.decodeParameters(["bytes", "address", "uint256", "uint256", "uint256", "bytes"], event_crossUserLock.raw.data);
    // console.log("event_crossUserLock Data:", eventLogData);
    //assert.equal(eventLogData["0"], routeAddress, "check crossUserLock Event routeAddress fail");

    // 查看 CrossRouter 合约 balance，当前CrossDelegate为空函数，不会把token转走
    let crossRouter_balance = await demoErc20_Inst.balanceOf(crossRouter_addr);
    console.log("ERC20 crossRouter_balance:", crossRouter_balance.toString());
    // 查看allowance
    let admin_to_crossRouter_allowance = await demoErc20_Inst.allowance(network.admin, crossRouter_addr);
    console.log("ERC20 admin_to_crossRouter_allowance_2:", admin_to_crossRouter_allowance.toString());

    let crossRouter_to_delegate_allowance = await demoErc20_Inst.allowance(crossRouter_addr, demoCrossDelegateV4_addr);
    console.log("ERC20 crossRouter_to_delegate_allowance:", crossRouter_to_delegate_allowance.toString());

  }

  //**********************************************************************************************
  //**********************************************************************************************
  // 2.2 call crossUserBurn
  {
    tx_receipt = await demoErc20_Inst.mint(network.admin, cross_value);
    let admin_balance = await demoErc20_Inst.balanceOf(network.admin);
    console.log("admin_balance.toString:", admin_balance.toString());
    tx_receipt = await demoErc20_Inst_2.methods.approve(crossRouter_addr, cross_value).send({
              from: network.admin,
              gasPrice: '20000000000',// ganacle-cli default value
              gas: 6721975            // ganache-cli default value
          });
    let admin_to_crossRouter_allowance = await demoErc20_Inst.allowance(network.admin, crossRouter_addr);
    console.log("admin_to_crossRouter_allowance_3:", admin_to_crossRouter_allowance.toString());
  }

  constraintCBOR = web3.eth.abi.encodeParameters(["uint256"], ["12345"]);
  console.log("constraintCBOR:", constraintCBOR);
  // 跨回来的参数：outReceive/outTokenPairID/constraintCBOR
  routeData = web3.eth.abi.encodeParameters(["address", "uint256", "bytes"], [network.admin, 222, constraintCBOR]);
  console.log("routeData:", routeData);
  let args_crossUserBurn = [smgID, tokenPairID, cross_value, fee, demoErc20_addr, userAccount, routeData];
  console.log("args_crossUserBurn:", args_crossUserBurn);
  tx_receipt = await crossRouterInst.methods.crossUserBurn(...args_crossUserBurn)
                    .send({
                      from: network.admin,
                      gasPrice: '20000000000',// ganacle-cli default value
                      gas: 6721975,           // ganache-cli default value
                      value: msg_value
                    });
  console.log("crossUserBurn tx_receipt:", tx_receipt);
  assert.equal(receipt.status, true, " CrossRouter crossUserBurn failure ");

 {
    // check Event
    let event_crossUserBurn = tx_receipt.events.CrossUserBurn;
    console.log("event_crossUserBurn:", event_crossUserBurn);
    let ev_vals = event_crossUserBurn.returnValues;
    console.log("ev_vals:", ev_vals);
    //assert.equal(ev_vals.routeIn, userAccount, "check crossUserBurn Event fail");
  }

  {
    // check Balance
    balance_admin = await web3.eth.getBalance(network.admin);
    console.log("ether after before crossUserBurn balance_admin :", balance_admin); 

    // expect 
    let balance_crossRouter = await web3.eth.getBalance(crossRouter_addr);
    console.log("ether balance_crossRouter:", balance_crossRouter.toString());
    let bn_feeNative = new BigNumber(feeNative);
    bn_feeNative = bn_feeNative.multipliedBy(2);
    assert.equal(balance_crossRouter.toString(), bn_feeNative.toString(), "check after crossUserBurn balance_crossRouter fail");

    // expece 
    let balance_crossDelegate = await web3.eth.getBalance(demoCrossDelegateV4_addr);
    console.log("ether balance_crossDelegate:", balance_crossDelegate.toString());
    let bn_msg_val = new BigNumber(msg_value);
    bn_msg_val = bn_msg_val.multipliedBy(2);

    let bn_msgvalue = new BigNumber(msg_value);
    bn_msgvalue = bn_msgvalue.multipliedBy(2);
    console.log("bn_msg_val.toString:", bn_msg_val.toString());

    bn_balance = bn_msgvalue.minus(bn_feeNative);
    console.log("bn_balance:", bn_balance.toString());

    assert.equal(balance_crossDelegate.toString(), bn_balance.toString(), "check after crossUserBurn ether  balance_crossDelegate fail");
  }

  {
    // check erc20 balance
    // 查看 CrossRouter 合约 balance，当前CrossDelegate为空函数，不会把token转走
    let crossRouter_balance = await demoErc20_Inst.balanceOf(crossRouter_addr);
    console.log("ERC20 crossRouter_balance:", crossRouter_balance.toString());

    // 查看allowance
    let admin_to_crossRouter_allowance = await demoErc20_Inst.allowance(network.admin, crossRouter_addr);
    console.log("ERC20 admin_to_crossRouter_allowance_2:", admin_to_crossRouter_allowance.toString());

    let crossRouter_to_delegate_allowance = await demoErc20_Inst.allowance(crossRouter_addr, demoCrossDelegateV4_addr);
    console.log("ERC20  crossRouter_to_delegate_allowance:", crossRouter_to_delegate_allowance.toString());
  }

  // test transferTo
  {
    console.log("\n\n");
    console.log("test transferTo");
    let balance_admin = await web3.eth.getBalance(network.admin);
    console.log("balance_admin:", balance_admin);

    let balance_crossRouter = await web3.eth.getBalance(crossRouter_addr);
    console.log("balance_crossRouter:", balance_crossRouter);

    tx_receipt = await crossRouterInst.methods.transferTo(network.admin, balance_crossRouter.toString())
      .send({
        from: network.admin,
        gasPrice: '20000000000',// ganacle-cli default value
        gas: 6721975            // ganache-cli default value
      });
    console.log("crossUserBurn tx_receipt:", tx_receipt);
    assert.equal(receipt.status, true, " CrossRouter transferTo failure ");
    balance_crossRouter = await web3.eth.getBalance(crossRouter_addr);
    console.log("balance_crossRouter2:", balance_crossRouter);
    assert.equal(balance_crossRouter, balance_crossRouter.toString(), " after transferTo balance failure ");
  }

  {
    console.log("\n\n\n\n");
    console.log("***************************************************************");
    console.log("***************************************************************");
    console.log("\n\n\n\n");
    // 换一种方式查询分析event
    console.log("crossUserBurn tx_receipt:", tx_receipt);
    let txReceipt = await web3.eth.getTransactionReceipt(tx_receipt.transactionHash);
    console.log("txReceipt:", txReceipt);

    let eventSignatures = getEventSignature(CrossRouterScJson.abi);
    console.log("eventSignatures:", eventSignatures);
    let logs = [];
    txReceipt.logs.filter((log) => {
      if (!eventSignatures[log.topics[0]]) {
        return;
      }
      let inputs = eventSignatures[log.topics[0]].abiEntry.inputs;
      logs.push({
        event: eventSignatures[log.topics[0]].abiEntry.name,
        args: web3.eth.abi.decodeLog(inputs, log.data, log.topics.slice(1))
      });
    });
    console.log("logs:", logs);
  }
  return ;

  {
    console.log("\n\n\n\n");
    console.log("***************************************************************");
    console.log("***************************************************************");
    console.log("\n\n\n\n");
    // 换一种方式查询分析event
    //console.log("crossUserBurn tx_receipt:", tx_receipt);
    let txReceipt = await web3.eth.getTransactionReceipt(tx_receipt.transactionHash);
    console.log("txReceipt:", txReceipt);
    console.log("txReceipt.logs:", txReceipt.logs);

    //console.log("DemoCrossDelegateV4._abi:", DemoCrossDelegateV4._json.abi);
    let eventSignatures_DemoCrossDelegate = getEventSignature(DemoCrossDelegateV4._json.abi);
    console.log("eventSignatures_DemoCrossDelegate:", eventSignatures_DemoCrossDelegate);
    let logs = [];
    txReceipt.logs.filter((log) => {
      if (!eventSignatures_DemoCrossDelegate[log.topics[0]]) {
        return;
      }
      let inputs = eventSignatures_DemoCrossDelegate[log.topics[0]].abiEntry.inputs;
      console.log("inputs:", inputs);
      console.log("log.data:", log.data);
      console.log("log.topics.slice(1):", log.topics.slice(1));

      logs.push({
        event: eventSignatures_DemoCrossDelegate[log.topics[0]].abiEntry.name,
        args: web3.eth.abi.decodeLog(inputs, log.data, log.topics.slice(1))
      });
    });
    console.log("logs:", logs);
  }

});

function getEventSignature(abi) {
  let knownEvents = {};
  abi.forEach(item => {
    if (item.type === "event") {
      //console.log("item:", item);
      let eventString = `${item.name}()`;
      //console.log("eventString 1:", eventString);
      let argvType = item.inputs.map(one => one.type);
      if (argvType.length) {
          //console.log("argvType:", argvType);
          eventString = `${item.name}(${argvType.join(",")})`
      }
      //console.log("eventString 2:", eventString);
      let eventSignature = web3.utils.keccak256(eventString);
      knownEvents[eventSignature] = {
        eventString: eventString,
        abiEntry: item
      };
    }
  });
  return knownEvents;
}
