const { log } = require('../lib');
const { All } = require('../libt');

const compose = (...fns) =>
  (...args) =>
    fns.reduceRight((res, fn) =>
      [fn.call(null, ...res)], args)[0];

const Fn = f => ({
  fold: f,
  concat: o => Fn(x => f(x).concat(o.fold(x))),
});

const hasVowels = s => /[aeiou]/i.test(s);
const longWord = s => s.length >= 5;

const both = Fn(compose(All, hasVowels))
               .concat(Fn(compose(All, longWord)));

log(
  ['gym', 'bird', 'lilac']
  .filter(v => both.fold(v).x)
);
//=> ['lilac']
