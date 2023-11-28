//
// tags: javascript exception
//

const log = console.log.bind(console);

function toJSON(obj) {
  return JSON.stringify(obj, null, 2);
}

/**
 * Parses an exception for the message and appends a “feature” to the
 * returned string.
 *
 * @param {Error} ex An Error exception.
 * @param {string} feature A feature string to prepend
 * @returns {string}
 */
function parseMsg(ex, feature) {
  const msg = ex.message || 'Default';
  return ['MyPro', feature, msg].filter(e => e).join('::');
}

function createError(e) {
  const error = new Error(parseMsg(e));
  error.origName = e && e.name;
  error.name = 'StandardError';

  return error;
};

function createFeatureXError(e, feature) {
  const error = new Error(parseMsg(e));
  error.origName = e && e.name;
  error.name = 'FeatureXError';

  return error;
};

(function run() {
  const err = createError(new Error('Gave wood!'));
  const featX = createFeatureXError('Oh noes!', 'feature-X');

  log(err);
  log(featX);
}());

