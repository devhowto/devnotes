const Product = x => ({
  x,
  concat: ({ x: y }) => Product(x * y),
  str: () => `Product(${x})`,
});

/** 
 * 1 is neutral/empty/identity of multiplication.
 */
Product.empty = () => Product(1);

module.exports = { Product };