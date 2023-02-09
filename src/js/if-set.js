var log = console.log.bind(console);
var err = console.error.bind(console);

var { ENV, BRD, MKT, BRK} = process.env;

log(ENV, BRD, MKT, BRK);

function any(f, ...vars) {
  log(vars);
  return vars.some(f);
}

function isEmpty(v) {
  return v === '' || v === undefined || v === null;
}

if (any(isEmpty, ENV, BRD, MKT, BRK)) {
  err('Env vars not defined...')
  process.exit(1);
}

log('EOT');
