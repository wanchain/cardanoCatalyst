#!/usr/bin/env node
import * as ROUT from "./router-out/index.js";
import { outerArgsHandler } from "./utils/cli.js";

outerArgsHandler("RouterOut", ROUT.mkConfig, process.argv);
