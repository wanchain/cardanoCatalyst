import {
  Config,
  Target,
  chalk,
  logAbort,
  main,
} from "@anastasia-labs/smart-handles-agent";
import { Network, errorToString } from "@anastasia-labs/smart-handles-offchain";
import { Command } from "@commander-js/extra-typings";

/**
 * `argHandler` is an optional overrider triggers custom behavior instead of
 * smiply instanciating a `smart-handles-agent` CLI.
 * `arguments` is the list of keywords that can trigger its `action`. And
 * `action` is provided with the keyword that triggered it, along with any other
 * arguments that had followed.
 */
export const outerArgsHandler = (
  label: string,
  configMaker: (network: Network, target: Target) => Config,
  args: string[],
  argHandler?: {
    arguments: string[];
    action: (config: Config, arg: string, args: string[]) => Promise<void>;
  },
) => {
  if (args[4] === "submit-simple") {
    logAbort(`${label} does not support simple requests.`);
    process.exit(1);
  } else if (
    (args[2] === "Mainnet" || args[2] === "Preprod") &&
    (args[3] === "Single" || args[3] === "Batch")
  ) {
    const config = configMaker(args[2], args[3]);
    if (argHandler && argHandler.arguments.some(a => a === args[4])) {
      argHandler.action(config, args[4], args.slice(5));
    } else {
      const program: Command = main(config);
      program
        .parseAsync([...args.slice(0, 2), ...args.slice(4)])
        .catch((e: any) => console.log(errorToString(e)));
    }
  } else {
    console.log();
    console.log(chalk.bold(`${label} CLI Tool`));
    const helpText = `To insatntiate the ${label} variant of Anastasia Labs' Smart Handles Agent,
pass these two arguments:
\u0009network  (Mainnet | Preprod) - Choose either mainnet or preprod
\u0009target   (Single  | Batch  ) - If set to \`Batch\` the provided script is
\u0009                               expected to be a staking script
`;
    console.log(helpText);
  }
};
