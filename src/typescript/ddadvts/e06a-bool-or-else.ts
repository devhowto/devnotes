export const MODNAME = "e06a-bool-or-else";

//
// Conditional types.
//

type BoolOrElse<T> = T extends boolean ? boolean : "string";

type B = BoolOrElse<boolean>;
//   ^?

var x = 1;

type S = BoolOrElse<number>
//   ^?
