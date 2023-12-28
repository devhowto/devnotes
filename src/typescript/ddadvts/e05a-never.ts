export const MODNAME = "e05a-never";

/**
 * Not even doing a type assertion `as any` solves it!
 *
 * Nothing is assigned to `never`. Nothing!
 */
const h1: never = "hello" as any;

/**
 * And please, _never_ (pun intended) do this.
 */
const h2: never = "hello" as never;
