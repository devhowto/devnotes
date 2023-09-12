const log = console.log.bind(console);

function isVowel(chr) {
  return "aeiou".includes(chr);
}

/**
 * T.C: O(n).
 * S.C: O(1).
 *
 * @param {string} str
 * @returns {number}
 */
function vowelCount(str) {
  let count = 0;

  for (let c of str)
    if (isVowel(c)) ++count;

  return count;
}

// No chars, 0 vowels.
log(vowelCount(""));

// No vowels in this string.
log(vowelCount("xyz"));

// All 5 chars are vowels.
log(vowelCount("aeiou"));

// “y” is not considered a vowel in this challenge.
log(vowelCount("yaeiou"));

// No vowels, only consontant chars
log(vowelCount("bcdfghjklmnpqrstvwxyz"));

