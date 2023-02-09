'use strict';

const log = console.log.bind(console);

/**
 * Get env vars according to opts description object.
 *
 * Returns a tuple containing the number of errors and an object
 * with key/value pairs for the env vars extracted.
 *
 * @param {object} opts The config object.
 * @param {NodeJS.processEnv} processEnv process.env object
 * @returns {[number, object]}
*/
function getEnvVars(opts, env) {
  let errorCount = 0;
  const envVars = {};
  const entries = Object.entries(opts);

  entries.forEach(function eachKey([key, cfg]) {
    const val = env[key];

    if (cfg.required) {
      log(key, cfg);
      if (cfg.validate(val)) {
        envVars[key] = val;
      } else if (cfg.default !== undefined) {
        envVars[key] = cfg.default;
      } else {
        envVars[key] = `ERROR: ${key} failed validation.`;
        ++errorCount;
      }
    } else if (cfg.default !== undefined) {
      envVars[key] = cfg.default;
    }
  });

  return [errorCount, envVars];
}

function presence(val) {
  return (
    val !== undefined &&
    val !== null &&
    typeof val === 'string' &&
    val.trim().length >= 1
  );
}

const config = {
  ENV: {
    required: true,
    validate: presence,
    default: 'stage',
  },
  BRAND: {
    required: true,
    validate: presence,
  },
  MARKET: {
    required: true,
    validate: presence,
    default: 'br'
  },
  BREAKPOINT: {
    required: true,
    validate: function validateBreakpoint(val) {
      return ['mobile', 'tablet', 'desktop'].includes(val);
    },
  },
  LOGLEVEL: {
    required: false,
    default: 'INFO',
  }
};

const [errorCount, envVars] = getEnvVars(config, process.env);

log('Errors:', errorCount);
log(envVars);

/**
 * Formats the key/value object pairs as an ASCII list.
 *
 * @param {{[string]: string | number}} envVars
 * @returns {string}
 *
 * @example
 * formatKeyValAsList({ id: 1, FOO=bar });
 * //=> • id='1'
 * //=> • FOO='bar'
 */
function formatKeyValAsList(envVars) {
  return Object.entries(envVars).reduce((acc, [key, val]) => {
    return [...acc, `  • ${key}='${val}'`]
  }, []).join('\n');
}

/**
 * Formats the key/value object pairs as a key/val,
 * comma-separated* string.
 *
 * @param {{[string]: string | number}} keyVal
 * @returns {string}
 *
 * @example
 * formatKeyValAsString({ id: 1, FOO=bar });
 * //=> 'id=1, FOO=bar'
 */
function formatKeyValAsString(keyVals) {
  return Object.entries(keyVals).reduce((acc, [key, val]) => {
    return [...acc, `${key}=${val}`];
  }).join(', ');
}

log(formatKeyValAsList(envVars));
log(formatKeyValAsString(envVars));
