'use strict';

const fs = require('node:fs');

/**
 * List files in `dir` filtered by `ext`.
 *
 * @param {string} dir
 * @param {string} dir
 * @param {(Error, string[]) => undefined} cb
 */
module.exports = function (dir, ext, callback) {
  const re = new RegExp(`\\.${ext}$`);

  fs.readdir(dir, function (err, files) {
    if (err) { return callback(err); }

    const filteredFiles = files.filter(file => re.test(file));

    callback(null, filteredFiles);
  });
};

