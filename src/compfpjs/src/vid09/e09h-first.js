const { List } = require('immutable-ext');
const { log } = require('../lib');
const { Left, Right } = require('../libt');

const First = either => ({
  fold: f => f(either),
  concat: o => (either.isLeft ? o : First(either)),
});

First.empty = () => First(Left());

const find = (xs, f) =>
  List(xs)
    .foldMap(x => First(f(x) ? Right(x) : Left()), First.empty())
    .fold(x => x);

log(
  find([3, 4, 5, 6, 7], x => x > 4)
);
//=> Right(5)
//
// NOTE: Displays an object but not Right(5) as mentioned
// in the video.
////
