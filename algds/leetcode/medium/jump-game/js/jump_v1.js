const log = console.log.bind(console);

/**
 * Iterative, procedural solution.
 *
 * @param {Array<number>} nums
 * @returns {boolean}
 */
function canJump(nums) {
  const len = nums.length;
  const lastIdx = len - 1;

  if (len <= 1) return true;

  let maxJmp = nums[0];

  for (let i = 0; i < nums.length; ++i) {
    //
    // Checks if we can still try go continue.
    //
    if (maxJmp <= i && nums[i] === 0)
      return false;

    //
    // Updates maxJmp according the current i. The next jump can only
    // possibly be the jump defined by the next index + the current
    // index.
    //
    if (i + nums[i] > maxJmp)
      maxJmp = i + nums[i];

    //
    // If the maxJmp we can make would match or overshoot the last
    // index, then we can reach it and we have a successful outcome.
    //
    if (maxJmp >= lastIdx)
      return true;
  }
}

log(
  canJump([2, 3, 1, 1, 4]), // true
  canJump([3, 2, 1, 0, 4]), // false
  canJump([2, 0, 0]),       // true
);
