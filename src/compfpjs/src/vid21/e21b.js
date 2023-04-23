const { log } = require('../lib');
const { task, of } = require('folktale/concurrency/task');

// Simulates some sort of DB find thingy.
const DB = ({
  find: id =>
    task(({ reject, resolve }) =>
      setTimeout(() =>
        resolve({ id, title: `Project #${id}` }), 100))
});

const reportHeader = (p1, p2) =>
  `Report ${p1.title} compared to ${p2.title}`;

of(p1 => p2 => reportHeader(p1, p2))
  .ap(DB.find(1))
  .ap(DB.find(42))
  .run()
  .listen({
    onResolved: d => log(d),
    onError: e => log(e),
  });
//=> Report Project #1 compared to Project #4
