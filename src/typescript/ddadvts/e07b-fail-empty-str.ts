export const MODNAME = "e07b-inpfail-empty-str";

//
// We can do T extends "" because TS has type literals!!! <3
//
type NoEmptyStr<T extends string> = T extends "" ? never : T;

function failOnEmptyStr<T extends string>(val: NoEmptyStr<T>) {
  if (val.length < 1)
    throw new Error("Input string is empty.");
}

//
// Now TS type-checking is helping us out. Because we passed an empty
// string, it will cause the conditional type we used for the param
// to result in never, and an nothing can be assigned to never.
//
failOnEmptyStr("");
