const { Sum } = require('../libt');
const { log } = require('../lib');

const res = [Sum(1), Sum(2), Sum(3)]
  .reduce((acc, x) => acc.concat(x), Sum.empty());

log(res.str());
//=> Sum(6)