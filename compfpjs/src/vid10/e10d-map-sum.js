const { Map } = require('immutable-ext');
const { Sum } = require('../libt');
const { log } = require('../lib');

const res = Map({ ahsoka: 90, aayla: 80 })
  .map(Sum)
  .fold(Sum.empty());

log(res.str());
//=> Sum(180)