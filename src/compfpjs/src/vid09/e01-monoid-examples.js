const { List } = require('immutable-ext');
const { compose } = require('immutable');
const {
  log,
  idty,
  fromNullable,
} = require('../lib');

const Sum = x => ({
  x,
  concat: ({ x: y }) => Sum(x + y),
});
Sum.empty = () => Sum(0);

const Product = x => ({
  x,
  concat: ({ x, y }) => Product(x + y),
});
Product.empty = () => Product(1);

const Any = x => ({
  x,
  concat: ({ x: y }) => Any(x || y),
});
Any.empty = () => Any(false);

const All = x => ({
  x,
  concat: ({ x: y }) => All(x && y),
});
All.empty = () => All(true);

const Min = x => ({
  x,
  concat: ({ x: y }) => Min(x < y ? x : y),
});
Min.empty = () => Min(Infinity);

const Right = x => ({
  fold: (_f, g) => g(x),
  map: f => Right(f(x)),
  concat: o =>
    o.fold(e => Left(e),
           r => Right(x.concat(r))),
});

const Left = x => ({
  fold: (f, _g) => f(x),
  map: _f => Left(x),
  concat: o => Left(x),
});

const stats = List.of(
  { page: 'Home', views: 40 },
  { page: 'About', views: 10 },
  { page: 'Blog', views: 4 },
);

const statsRes1 = stats.foldMap(x =>
  fromNullable(x.views)
  .map(Sum),
  Right(Sum(0)));
// log(statsRes1.toJS()); // not working...
//=> Right(Sum(54))

const First = either => ({
  fold: f => f(either),
  concat: o => either.isLeft ? o : First(either),
});
First.empty = () => First(Left());

const find = (xs, f) =>
  List(xs)
    .foldMap(x =>
      First(f(x) ? Right(x) : Left()),
      First.empty()
    ).fold(idty);

const Fn = f => ({
  fold: f,
  concat: o =>
    Fn(x => f(x).concat(o.fold(x))),
});

const hasVowels = s => /[aeiou]/ig.test(s);
const longWord = s => s.length >= 5;

const both = Fn(compose(All, hasVowels))
               .concat(Fn(compose(All, longWord)));

log(
  ['gym', 'bird', 'lilac'].filter(v => both.fold(v).v)
);
//=> ['lilac']

const Pair = (x, y) => ({
  x,
  y,
  concat: ({ x: x1, y: y1 }) =>
    Pair(x.concat(x1), y.concat(y1)),
});

