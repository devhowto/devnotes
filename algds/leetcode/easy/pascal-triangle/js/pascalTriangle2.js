import { performance as perf } from 'node:perf_hooks';

const log = console.log.bind(console);

const findMiddleIdx = (num) => Math.floor(num / 2);

var generate = function (numRows) {
  if (numRows === 0) return [];
  if (numRows === 1) return [[1]];
  if (numRows === 2) return [[1], [1, 1]];

  const helper = (targetArr, output) => {
    if (output.length === targetArr) return output;

    const nextArrLength = output.length + 1;
    const tempArr = new Array(nextArrLength)

    tempArr[0] = 1;
    tempArr[tempArr.length - 1] = 1;

    for (let i = 1; i <= findMiddleIdx(nextArrLength); i++) {
      tempArr[i] =
        output[output.length - 1][i] + output[output.length - 1][i - 1];

      tempArr[nextArrLength - 1 - i] = tempArr[i];
    }

    if (nextArrLength >= 3) {
      if (nextArrLength % 2) {
        const middleIdx = findMiddleIdx(nextArrLength);

        tempArr[middleIdx] = output[output.length - 1][middleIdx] * 2;
      }
    }

    output.push(tempArr);

    return helper(targetArr, output);
  };

  return helper(numRows, [[1], [1, 1]]);
};

// console.log(
//   "test generate",
//   generate(10).map((el) => el.join(" "))
// );

// [1],
// [1, 1],
// [1, 2, 1],
// [1, 3, 3, 1],
// [1, 4, 6, 4, 1];
// [1, 5, 10, 10, 5, 1];
// [1, 6, 15, 20, 15, 6, 1];

const ini = perf.now();
generate(1024);
const end = perf.now();

log({ ini, end, diff: end - ini });
