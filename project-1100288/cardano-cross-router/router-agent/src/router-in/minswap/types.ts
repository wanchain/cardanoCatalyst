import { Data } from "@anastasia-labs/smart-handles-offchain";

export const MinswapRequestSchema = Data.Object({
  aToBDirection: Data.Boolean(),
  minimumReceive: Data.Integer(),
});
export type MinswapRequest = Data.Static<typeof MinswapRequestSchema>;
export const MinswapRequest = MinswapRequestSchema as unknown as MinswapRequest;
