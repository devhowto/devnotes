const { log } = require('../lib');
const { task } = require('folktale/concurrency/task');

// Simulates some sort of DB find thingy.
const DB = ({
  find: id =>
    task(({ reject, resolve }) =>
      setTimeout(() =>
        resolve({ id, title: `Project #${id}` }), 100))
});

const reportHeader = (p1, p2) =>
  `Report ${p1.title} compared to ${p2.title}`;

DB.find(1)
  .chain(p1 =>
    DB.find(42)
      .map(p2 => reportHeader(p1, p2)))
  .run()
  .listen({
    onResolved: d => log(d),
  });
//=> Report Project #1 compared to Project #4
