const path = require('path');

module.exports = {
  entry: {
    onping: './lib/js/src/App.js'
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
