
const optimist = require("optimist");

const Utils = require("./utils");
const truffleConfig = require("../truffle-config");




contract('Test Cross Router', (accounts) => {
  // console.log("Test Cross Router accounts:", accounts);
  before("test Contract init...   -> success", async () => {
      console.log("before init");
      let currNetwork = optimist.argv.network;
      let networkConfig = truffleConfig.networks[currNetwork];
      //console.log("networkConfig:", networkConfig);
      Utils.setGlobal("network", networkConfig);

  });

  Utils.importMochaTest("Test CBOR V1", './test_CBOR');
  Utils.importMochaTest("Test BusinessV1 V1", './test_BusinessV1');

  after("finish...   -> success", function () {
    console.log("After all tests");
  });
});
