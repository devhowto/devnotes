export const MODNAME = "e09a-infer";

function add(x: number, y: number): number {
  return x + y;
}

//
// NOTE: TypeScript already provides the utility type ReturnType.
//
type RetType<T> = T extends (...args: any[]) => infer R ? R : unknown;

type AddReturn = ReturnType<typeof add>;
//   ^?
