export const MODNAME = "index type 1";
/*

https://github.com/microsoft/TypeScript/issues/15768

*/

const log: Console["log"] = console.log.bind(console);

type I<K extends number> = ['a'][K];
type J<K extends number> = ['bc'][K];

/* x can only possibly be 'a'. */
const x: I<number> = 'a';

/* y can only possibly be 'bc'. */
const y: J<number> = 'bc';

type TVal = { 0: { id: number }, 1: { name: string } };

/* Same as `type TKey = 0 | 1`. */
type TKey = keyof TVal;

/* 7 is not assignable to type TKey, which is 0 | 1. */
const e: TKey = 7;

function getVal<K extends TKey>(k: K, data: TVal): TVal[K] {
  return data[k];
}

declare const data: TVal;

/* Intellisense is smart to complete id or name depending on
 * the 0 or 1 keys passed. */
var id = getVal(0, data).id;
var name = getVal(1, data).name;
//    ^?
