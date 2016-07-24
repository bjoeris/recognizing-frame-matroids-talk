"use strict";

//require('./bower_components/KaTeX/src/katex.min.css');
// require('KaTeX/dist/katex.min.css');
// var katex = require('KaTeX/dist/katex.js');
// var renderMathInElement = require('KaTeX/dist/contrib/auto-render.js');
require('KaTeX/dist/katex.min.css');
var katex = require('KaTeX/katex.js');
var renderMathInElement = require('KaTeX/contrib/auto-render/auto-render.js');

exports.renderMathInElement = function (elem) {
    return function (options) {
        return renderMathInElement(elem, options);
    }
}