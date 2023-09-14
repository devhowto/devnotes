const log = console.log.bind(console);

function palind(s) {
  const len = s.length;

  if (len <= 1) return true;

  if (s[0] === s[len - 1])
    return palind(s.slice(1, len - 1))

  return false;
}

log(palind(""));
log(palind("b"));
log(palind("aba"));
log(palind("abab"));
log(palind("racecar"));

/*

racecar
 aceca
  cec
   e


*/
