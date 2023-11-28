const { of, rejected } = require('folktale/concurrency/task');
const { log, add1 } = require('../lib');

of(1)
  .map(add1) // runs
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> ok 2

rejected(-1)
  .map(add1) // ignored
  .run()
  .listen({
    onRejected: e => log('err', e),
    onResolved: v => log('ok', v),
  });
//=> err -1
// Because map was ignored, we do NOT add 1 to
// -1 and the result is still the original -1.
////