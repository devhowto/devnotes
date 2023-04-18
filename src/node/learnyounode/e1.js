const { performance: perf } = require('node:perf_hooks');
const log = console.log.bind(console);

// Non-blocking operation.
function f() {
  setTimeout(() => {
    log('Timed out!');
  }, 0);
}

f();

log('END');

// Blocking Operation
const ini = perf.now();
var total = 0;
for (var i = 0; i < 99999999; ++i) {
  total += i; 
}
const end = perf.now();
log({ ini, end, ellapsed: end - ini });

f();
