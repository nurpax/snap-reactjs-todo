
var path = require('path');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var combineLoaders = require('webpack-combine-loaders');

var BUILD_DIR = path.resolve(__dirname, 'static/build');
var APP_DIR = path.resolve(__dirname, 'client');

var config = {
  entry: APP_DIR + '/index.js',
  output: {
    path: BUILD_DIR,
    filename: 'bundle.js'
  },
  resolve: {
    extensions: ['', '.js', '.jsx']
  },
  plugins: [new ExtractTextPlugin('styles.css')],
  module : {
    loaders : [
      {
        test : /\.jsx?/,
        loader : 'babel',
        include : APP_DIR,
        exclude: '/node_modules/',
        query: {
          cacheDirectory: true,
          presets: ['react', 'es2015', 'stage-0']
        }
      },

      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract(
          'style-loader',
          combineLoaders([{
            loader: 'css-loader',
            query: {
              modules: true,
              localIdentName: '[name]__[local]___[hash:base64:5]'
            }
          }])
        )
      },

      {
        test: /\.scss$/,
        loader: ExtractTextPlugin.extract(
          'style-loader',
          combineLoaders([{
            loader: 'css-loader',
            query: {
              modules: true,
              sourceMap: true,
              localIdentName: '[name]__[local]___[hash:base64:5]'
            },
          },
          {
            loader: 'sass-loader',
            query: {
              sourceMap: true,
            }
          }])
        )
      }
    ]

  },
};

module.exports = config;
