const { List } = require('immutable-ext');
const { Sum } = require('../libt');
const { log } = require('../lib');

const res = List.of(1, 2, 3)
  .foldMap(Sum, Sum.empty());

log(res.str());
//=> Sum(6)