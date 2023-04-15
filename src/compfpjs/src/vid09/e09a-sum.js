const { log } = require('../lib');
const { Sum } = require('../libt');

const x = Sum(1);
const y = Sum(1);
log(x.concat(y).str());
//=> Sum(2)

log(
  Sum(-1)
  .concat(Sum(1))
  .concat(Sum(-13))
  .str()
);
//=> Sum(-13)