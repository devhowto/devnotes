const { Map } = require('immutable-ext');
const { Sum } = require('../libt');
const { log } = require('../lib');

const res = Map({ ahsoka: Sum(97), aayla: Sum(83) })
  .fold(Sum.empty());

log(res.str());
//=> Sum(180)