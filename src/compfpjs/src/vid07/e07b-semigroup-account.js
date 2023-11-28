const { Map } = require('immutable-ext');
const { log } = require('../lib');
const { Sum, All, First } = require('../libt');

const acct1 = Map({
  name: First('Ahsoka'),
  isPaid: All(true),
  points: Sum(10),
  friends: ['Aayla'],
});

const acct2 = Map({
  name: First('Ahsoka'),
  isPaid: All(false),
  points: Sum(2),
  friends: ['Leia'],
});

/*
Use First for name, so it knows how to combine the name
on the two accounts.

All on the booleans, and Sum on the points.

friends is an array, so it is already concatable.

But now how do we concat the accounts, since they are
objects (not yet concatable)?

Let's use Map from immutable extension by DrBoolean.
*/

const res = acct1.concat(acct2);
log(res.toJS());
