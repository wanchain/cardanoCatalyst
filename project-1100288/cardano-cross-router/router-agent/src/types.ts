import { Data, DatumJson } from "@anastasia-labs/smart-handles-offchain";

export const TokenModeSchema = Data.Enum([
  Data.Literal("Mint"),
  Data.Literal("Spend"),
]);
export type TokenMode = Data.Static<typeof TokenModeSchema>;
export const TokenMode = TokenModeSchema as unknown as TokenMode;
export const tokenModeToDatumJson = (tokenMode: TokenMode): DatumJson => {
  return {
    constructor: tokenMode === "Mint" ? 0 : 1,
    fields: [],
  };
};

export const RouterDatumSchema = Data.Object({
  uniqueId: Data.Bytes(),
  inPairId: Data.Bytes(),
  outPairId: Data.Bytes(),
  evmReceiver: Data.Bytes(),
  routerOutRouterFee: Data.Integer(),
  inTokenSymbol: Data.Bytes(),
  inTokenTokenName: Data.Bytes(),
  inTokenMode: TokenModeSchema,
  outTokenSymbol: Data.Bytes(),
  outTokenTokenName: Data.Bytes(),
  outTokenMode: TokenModeSchema,
  businessSpecific: Data.Any(),
});
export type RouterDatum = Data.Static<typeof RouterDatumSchema>;
export const RouterDatum = RouterDatumSchema as unknown as RouterDatum;
export type RouterDatumFields = {
  uniqueId: string;
  inPairId: string;
  outPairId: string;
  evmReceiver: string;
  routerOutRouterFee: bigint;
  inTokenSymbol: string;
  inTokenTokenName: string;
  inTokenMode: "Mint" | "Spend";
  outTokenSymbol: string;
  outTokenTokenName: string;
  outTokenMode: "Mint" | "Spend";
  businessSpecific: Data;
}
