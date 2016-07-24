'use strict';

var config
  = { entry: './src/entry'
    , debug: true
    , devtool: 'source-map'
    , devServer: { contentBase: '.'
                 , port: 4008
                 , stats: 'errors-only'
                 }
    , output: { path: './assets'
              , pathinfo: true
              , publicPath: '/assets'
              , filename: 'bundle.js'
              }
    , module: { loaders: [ { test: /\.purs$/
                           , loader: 'purs-loader'
                           , query: { src: [ 'lib/purescript-*/src/**/*.purs'
                                           , 'bower_components/purescript-*/src/**/*.purs'
                                           , 'src/**/*.purs' 
                                           ]
                                    , bundle: false
                                    , psc: 'psa'
                                    , pscArgs: { sourceMaps: true }
                                    , pscIde: true
                                    , bundleNamespace: 'PS',
                                    }
                           }
                         , { test: /\.js$/
                           , loader: 'source-map-loader'
                           , exclude: /node_modules|bower_components/
                           }
                         , { test: /\.scss$/
                           , loaders: ["style", "css", "sass"]
                           }  
                         , { test: /\.css$/
                           , loaders: ["style", "css"]
                           }  
                         , { test: /\.(woff|ttf|eot)/
                           , loader: 'url-loader'}
                         ]
              }
    , resolve: { modulesDirectories: [ 'lib', 'node_modules', 'bower_components' ]
               , extensions: [ '', '.purs', '.js', '.css']
               }
    }
    ;

module.exports = config;