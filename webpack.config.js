
var webpack = require("webpack");
var path = require('path');
var AssetsPlugin = require('assets-webpack-plugin')
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var combineLoaders = require('webpack-combine-loaders');

var BUILD_DIR = path.resolve(__dirname, 'static/build');
var APP_DIR = path.resolve(__dirname, 'client');

var config = {
  entry: APP_DIR + '/index.js',
  output: {
    path: BUILD_DIR,
    filename: "[name]-bundle-[hash].js",
    publicPath: '/static/build'
  },
  resolve: {
    extensions: ['', '.js', '.jsx']
  },
  plugins: [
    new webpack.DefinePlugin({'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV || 'development')}),
    new ExtractTextPlugin('styles-[hash].css'),
    new AssetsPlugin(),
    new HtmlWebpackPlugin({template: 'static/index.template.ejs'})
  ],
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
        test: /globalStyles\.css$/,
        loader: ExtractTextPlugin.extract(
          'style-loader',
          'css-loader?sourceMap'
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
