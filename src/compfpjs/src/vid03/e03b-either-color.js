const { log } = require('../lib');

/**
 * @sig String -> String | undefined
 */
function findColor(name) {
  return {
    red: '#ff4444',
    blue: '#3b5998',
    yellow: '#fff68f',
  }[name];
}

log(findColor('red'));
//=> #ff4444'

log(findColor('red').slice(1).toUpperCase());
//=> 'FF4444'

log(findColor('green').slice(1).toUpperCase());
// TypeError: Cannot read properties of undefined (reading 'slice')
