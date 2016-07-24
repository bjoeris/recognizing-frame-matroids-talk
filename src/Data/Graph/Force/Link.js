"use strict";

// module Data.Graph.Force.Link

var d3 = require("d3/d3");

exports.linkNodes = function (opts) {
    return function (numIterations) {
        return function(nodes) {
            return function(edges) {
                var nodesCopy = nodes.map(function(node, i) {
                    node = Object.assign({}, node);
                    node.index = i;
                    return node;
                });
                var links = edges.map(function(e, i) {
                    var l = {};
                    l.index = i;
                    l.source = e.source;
                    l.target = e.target; 
                    l.original = e; 
                    return l;
                });
                var f = d3.forceLink();
                f.id(function(v) {
                    return v.id;
                });
                f.distance(function(l) {
                    return opts.distance(l.original)(l.source)(l.target);
                });
                f.initialize(nodesCopy);
                f.links(links);
                for (var i=0; i<numIterations; ++i) {
                    f(1);
                }
                return nodesCopy;
            }
        }
    }
}