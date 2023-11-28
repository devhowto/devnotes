const { log } = require('../lib');
const { Product } = require('../libt');

log(
  Product(2)
  .concat(Product(7))
  .str()
);
//=> Product(14)

log(
  Product(2)
  .concat(Product(7))
  .concat(Product(-1))
  .str()
);
// Product(-14)