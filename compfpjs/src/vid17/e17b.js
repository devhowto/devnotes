const { log } = require('../lib');

const add = x => y => x + y;
const res1 = add(1)(2);
log(res1);
//=> 3

const inc = add(1);
const res2 = inc(2);
log(res2);
//=> 3