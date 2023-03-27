import {
  log,
  idty,
} from './lib.js';


function Right(v) {
  return {
    map: f => Right(f(v)),
    fold: (f, g) => g(v),
    inspect: () => `Right(${v})`,
  };
}

function Left(v) {
  return {
    map: f => Left(v),
    fold: (f, g) => f(v),
    inspect: () => `Left(${v})`,
  };
}

log(Right(3).map(n => n + 1).map(n => n / 2).fold(e => 'error', idty));
//=> 2

log(Left(3).map(n => n + 1).map(n => n / 0).fold(e => 'error', idty));
//=> 'error'

/*

Either is a type which is either Left or Right.

In both Left and Right, map() is exactly the same as in Box.

Left is a stubborn little bugger which will refuse to run the function
that handles a value, as it is responsible for handling the case where
we don't have a value.

fold() is different in Right and Left than in Box. Either is a type that
may or may not have a value. It can be either Left (no value) or Right
(we have a value).

If we are to fold() to unwrap a value, but we don't have a value, then
we need two functions functions so we can handle both Left and Right.

The left function (first fn arg to fold()) handles errors (where we
don't have a value), and the right function (second/last fn arg to
fold()) handles success cases (where we do have a value to work with).

The Either type captures the concept of disjunction, which is an “or”
situation.

*/
