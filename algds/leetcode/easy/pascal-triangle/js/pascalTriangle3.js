import { performance as perf } from 'node:perf_hooks';

const log = console.log.bind(console);

var generate = function (numRows) {
  if (!numRows) return [];

  let matrix = [[1]];

  for (let i = 1; i < numRows; ++i) {
    let row = [1],
        j = 1;

    debugger;

    while (j < i) {
      row.push(matrix[i - 1][j - 1] + matrix[i - 1][j]);
      ++j;
    }

    row.push(1);
    matrix.push(row);
  }

  return matrix;
};

const ini = perf.now();
generate(1024);
const end = perf.now();

log({ ini, end, diff: end - ini });
