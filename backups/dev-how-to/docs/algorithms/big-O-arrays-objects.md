# Big-O of Arrays and Objects 

Let's take a look at time complexity of array and object methods in JavaScript.

## Time Complexity

How much CPU work do our data structures need to perform their basic operations?
Let's take a look.

### Big O of Object Operations

Objects are *unordered* key/value pairs. Nice to use when order is not needed or required. Most operations on objects are fast. They have no concept of beginning, middle or end.

- Access → O(1) → direct access to each value by key.
- Insertion → O(1).
- Deletion → O(1).
- Search → O(n), linear time.

Same concrete examples:

- `Object.keys`: O(n).
- `Object.values`: O(n).
- `Object.entries`: O(n).
- `obj.hasOwnProperty`: O(1).

### Big O of Array Operations

Arrays are like ordered lists. Use them when order is needed. Some operations are fast, some are not so fast.

- Access → O(1). Direct access to each value by index.
- Insertion → O(?). It depends.
  - Insert at index 0 is O(n) because it has to change the index of all other elements.
  - Insert or remove at end of array is O(1). No indexes needs to change.

`push()` is always faster then `shift()` and `unshift()` since `push()` adds to the end, while `shift()` and `unshift()` requires changing the indexes of all other elements because they operate on the beginning of the array.

`unshift(val)` adds val at position 0 and return val. Moves all other index one index to the right.

Removing at index 0 is O(n) because it has to change the index of all elements.  Removing last is O(1) because it doesn't need to change the index of other elements.

Removing at some other position is O(n) (it depends). Removing elements near the end requires less indexes changes; removing more to the beginning requires more indexes changes.

## Space Complexity

We also have to consider space complexity — how much memory our data structures take.

There is something called *auxiliary space complexity*, which has to do the space an algorithm requires to perform its computation, not including the input.
We will not care about input space, but with the space the algorithm itself needs.
Unless otherwise noted, when we say “space complexity”, we'll be referring to the *auxiliary space*.

### Primitive Types

Most primitive types are constant space:

- booleans;
- undefined;
- null;
- numbers;

All of the above are O(1) (constant space).
It doesn't matter if you have `x = 1` or `x = 1e256`.
The space required does not change (or not in any significant way).

Strings are not constant space, since their space requirement depends on the length.
They are O(n), with `n` being the length of the string.
As the length grows, the space complexity grows.

### Reference Types

For both objects and arrays, it is O(n), `n` being the number of keys in the object or the length for arrays.

## Code Examples

### sum() array

Following the concept of *empty sum*, a sum of an empty list results in 0 (zero).
That is what Haskell `sum` Prelude function does, for example:

```text
$ stack exec -- ghci
GHCi, version 9.0.2: https://www.haskell.org/ghc/ :? for help
ghci> sum []
0
```

Read the [Empty sum article on Wikipedia](https://en.wikipedia.org/wiki/Empty_sum).

#### Unit Tests

```{literalinclude} /../src/algdsts/src/02-bigO-notation/sum.test.ts
:language: typescript
```

#### sum(xs) v1

```{literalinclude} /../src/algdsts/src/02-bigO-notation/sum-v1.ts
:language: typescript
```

#### sum(xs) v2

```{literalinclude} /../src/algdsts/src/02-bigO-notation/sum-v2.ts
:language: typescript
```
#### sum(xs) v3

```{literalinclude} /../src/algdsts/src/02-bigO-notation/sum-v3.ts
:language: typescript
```

### calcSubtotals(xs)

#### Unit Tests

```{literalinclude} /../src/algdsts/src/02-bigO-notation/calcSubtotals.test.ts
:language: typescript
```

#### calcSubtotals(xs) v1

```{literalinclude} /../src/algdsts/src/02-bigO-notation/calcSubtotals-v1.ts
:language: typescript
```
