
var path = require('path');

var BUILD_DIR = path.resolve(__dirname, 'static/build');
var APP_DIR = path.resolve(__dirname, 'client');

var config = {
  entry: APP_DIR + '/index.jsx',
  output: {
    path: BUILD_DIR,
    filename: 'bundle.js'
  },
  resolve: {
    extensions: ['', '.js', '.jsx']
  },
  module : {
    loaders : [
      {
        test : /\.jsx?/,
        loader : 'babel',
        include : APP_DIR,
        exclude: '/node_modules/',
        query: {
          cacheDirectory: true,
          presets: ['react', 'es2015']
        }
      },
    ]
  },
};

module.exports = config;
