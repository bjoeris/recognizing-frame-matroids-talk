# recognizing-frame-matroids-talk

These are the slides from my talk at the 2016 International Workshop on Structure in Graphs and Matroids.

## Prerequisites

``` shell
npm install --global bower
```

## Compile:

``` shell
npm install
bower install
npm run webpack
```

This will create assets/bundle.js. Open index.html in a browser.

## Dev server

You can use the following command to start a server that will automatically recompile the slides when the code is edited.

``` shell
npm run webpack:server
```

Then open localhost:4008 in a browser