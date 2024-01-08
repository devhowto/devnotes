/**
 * Checks if `n` is 1.
 *
 * @param n The value to check.
 * @return A boolean indicating whether `n` is 1 or not.
 */
function isOne(n: unknown): boolean {
  return Number(n) === 1;
}

/**
 * Converts a positive number to its binary string representation.
 *
 * ASSUME: `n` >= 0.
 *
 * @param n The number to be converted.
 * @returns The converted number.
 */
function toBinaryStr(n: number): string {
  return n.toString(2);
}

/**
 * Count the number of 1 in the binary representation of `n`.
 *
 * ASSUME: `n` is positivive.
 *
 * @param n The number in decimal.
 * @return The number of 1 bits.
 */
function countBits(n: number): number {
  return [...toBinaryStr(n)].filter(isOne).length;
}

export { countBits };

