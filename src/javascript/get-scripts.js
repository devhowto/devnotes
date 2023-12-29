function getScripts(b) {
  return [].concat(
    b
      ? ["one.js"]
      : [], "two.js", "three.jsx"
  );
}

// getScripts(0);
//=> [ "two.js", "three.js" ]


// getScripts(1);
//=> [ "one.js", "two.js", "three.js" ]

