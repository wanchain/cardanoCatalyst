
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

  Utils.importMochaTest("Test BusinessV1 V1", './test_BusinessV1');
  Utils.importMochaTest("Test BusinessManager", './test_BusinessManager');
  Utils.importMochaTest("Test CrossRouter V1", './test_CrossRouter');

  after("finish...   -> success", function () {
    console.log("After all tests");
  });
});
