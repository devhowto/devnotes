import { performance } from 'node:perf_hooks';
const log = console.log.bind(console);

//
// 1e5 doen't even finish Replit free plan.
//
var nums = Array.apply(null, { length: 1e5 })
  .map(Function.call, Math.random);

//
// push
//
var aIni = performance.now();
var strs1 = nums.reduce((acc, n) => {
  acc.push(n);
  return acc;
}, []);
var aEnd = performance.now();
log('PUSH:', aEnd - aIni | 0);

//
// spread
//
var bIni = performance.now();
var strs2 = nums.reduce((acc, n) => {
  return [...acc, n];
}, []);
var bEnd = performance.now();

log('SPREAD:', bEnd - bIni | 0);
