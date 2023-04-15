const { log } = require('../lib');

const add = x => y => x + y;

const mod = dvr => dvd => dvd % dvr;

/** 
 * @sig Number -> Number
 *
 * The return number is 1 for odd numbers, 0 for even numbers.
 */
const isOdd = mod(2);

log([isOdd(1), isOdd(2)]);
//=> [1, 0]

const filter = pred => xs => xs.filter(pred);
const map = f => xs => xs.map(f);

const onlyOdds = filter(isOdd);
const odds = onlyOdds([1, 2, 3, 4, 5]);
log(odds);
//=> [1, 3, 5]

const replace = regex => repl => str =>
  str.replace(regex, repl);

const censor = replace(/[aeiou]/ig)('*');
const censorAll = map(censor);

log(censor('Ada Lovelace'));
//=> *d* L*v*l*c*

log(
  censorAll([
    'Ada Lovelace',
    'Ahsoka Tano',
    'Master Yoda',
  ])
);
//=> [ '*d* L*v*l*c*', '*hs*k* T*n*', 'M*st*r Y*d*' 