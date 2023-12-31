export const MODNAME = "e10a-unwrap-types";

type RetPromise1 = Promise<Promise<Promise<{ gold: 1e3 }>>>;
//   ^?
// The type is still wrapped in multiple promise layers

type RetPromise2 = Awaited<Promise<Promise<Promise<{ gold: 1e3 }>>>>;
//   ^?
// The type is unwrapped.


type UnwrapArray<T> =
  T extends Array<infer Inner>
    ? UnwrapArray<Inner>
    : T;

type U = Array<Array<number>>;
//   ^?
// Type is number[][].


// But we can unwrap the type.
type V = UnwrapArray<Array<Array<number>>>;
//   ^?