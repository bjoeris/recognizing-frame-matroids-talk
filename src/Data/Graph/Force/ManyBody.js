"use strict";

// module Data.Graph.Force.ManyBody

var d3 = require("d3/d3");

exports.manyBodyNodes = function (opts) {
    return function (numIterations) {
        return function(nodes) {
            var nodesCopy = nodes.map(function(node, i) {
                node = Object.assign({}, node);
                node.index = i;
                return node;
            });
            var f = d3.forceManyBody();
            f.initialize(nodesCopy);
            f.strength(opts.strength);
            for (var i=0; i<numIterations; ++i) {
                f(1);
            }
            return nodesCopy;
        }
    }
}