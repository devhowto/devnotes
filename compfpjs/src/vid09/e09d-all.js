const { log } = require('../lib');
const { All } = require('../libt');

log(All.empty().str());
//=> All(true)

log(
  All.empty()
  .concat(All(true))
  .concat(All(true))
  .str()
);
//=> All(true)
