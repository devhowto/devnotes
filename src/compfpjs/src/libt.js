//
// libt is more or less short of for “lib types”, as here we expose
// functional, composable types.
//
// By types we mean function wrappers implementing ideas
// like Monoid, Semigroup, etc.
//
// Here we also expose helpers for those types, like functional
// style tryCatch(), fromNullable(), etc.
//

const { Left, Right, Either } = require('./libt/Either');
const { All } = require('./libt/All');
const { Box } = require('./libt/Box');
const { First } = require('./libt/First');
const { Sum } = require('./libt/Sum');
const { Product } = require('./libt/Product');
const { Any } = require('./libt/Any');
const { Min } = require('./libt/Min');
const { Max } = require('./libt/Max');

module.exports = {
  All,
  Any,
  Box,
  First,
  Left,
  Right,
  Either,
  Max,
  Min,
  Product,
  Sum,
};
