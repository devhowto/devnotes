export const MODNAME = "e11b-tuples";

//
// By definitions, in TypeScript tuples are arrays with
// known types and length.
//

//
// Note we added ‘readonly’ now.
type Rgb = readonly [number, number, number];

const blue: Rgb = [0, 0, 255];

//
// And then TS checker says there is not ‘push’ (or any other related
// method on `blue`). That means the only way to assign values is
// during the initial assignment.
//
blue.push(0);
