//
// Write a program that accepts one or more numbers as command-line arguments
// and prints the sum of those numbers to the console (stdout).
//

const log = console.log.bind(console);

/**
 * Gets node params skipping the first two default params.
 */
const getParams = processArgv => processArgv.slice(2);

const params = getParams(process.argv);

const sum = params.reduce((acc, num) => acc + num, 0);
log(sum);
//=> '0102030'

/*

argc: argument count
argv: argument values (plural, array)

int main(int argc, char *argv[]) {
  argv[0] // main.c
  argv[1] // hello
  argv[2] // world
}

*/
