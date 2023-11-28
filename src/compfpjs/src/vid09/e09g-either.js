const { List } = require('immutable-ext');
const { log, fromNullable, idty } = require('../lib');
const { Sum } = require('../libt');

const Left = x => ({
  fold: (f, _g) => f(x),
  map: _f => Left(x),
  concat: o => Left(x),
});

const Right = x => ({
  fold: (_f, g) => g(x),
  map: f => Right(f(x)),
  concat: o =>
    o.fold(e => Left(e),
           r => Right(x.concat(r))),
});

const stats = List.of(
  { page: 'Home', views: 40 },
  { page: 'About', views: 10 },
  { page: 'Blog', views: 4 },
);

log(
  stats.foldMap(x =>
    fromNullable(x.views)
    .map(Sum),
    Right(Sum(0)))
  .fold(() => 'error', idty)
  .str()
);
//=> (Sum(54))
