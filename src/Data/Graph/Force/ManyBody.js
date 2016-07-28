"use strict";

// module Data.Graph.Force.ManyBody

var d3 = require("d3/d3");

exports.manyBody = function (opts) {
    return function(nodes) {
        var f = d3.forceManyBody();
        f.initialize(nodes);
        f.strength(opts.strength);
        f(1);
    }
}