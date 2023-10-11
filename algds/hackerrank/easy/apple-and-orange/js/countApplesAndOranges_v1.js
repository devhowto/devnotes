const log = console.log.bind(console);

/**
 * Count how many apples and oranges fall within the range of the house.
 */
function count(
  houseStart,
  houseEnd,
  appleTree,
  orangeTree,
  apples,
  oranges,
) {
  // log({ houseStart, houseEnd, appleTree, apples });

  for (let i = 0; i < apples.length; ++i) {
    let applePos = appleTree + apples[i];
    log({ applePos });
  }
}

count(
  7,
  10,
  4,
  12,
  [4, 2, 6, -3, 9],
  [],
);
