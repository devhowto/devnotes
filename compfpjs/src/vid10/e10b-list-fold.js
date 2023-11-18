const { List } = require('immutable-ext');
const { Sum } = require('../libt');
const { log } = require('../lib');

const res = List.of([Sum(1), Sum(2), Sum(3)])
  fold(Sum.empty());

log(res.str());
//=> Sum(6)