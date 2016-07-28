"use strict";

// module Data.Graph.Force.Link

var d3 = require("d3/d3");

exports.link = function (opts) {
    return function (edges) {
        var links = edges.map(function(e, i) {
            var l = {};
            l.index = i;
            l.source = e.source;
            l.target = e.target; 
            l.original = e; 
            return l;
        });
        return function(nodes) {
            var f = d3.forceLink();
            f.id(function(v) {
                return v.id;
            });
            f.distance(function(l) {
                return opts.distance(l.original)(l.source)(l.target);
            });
            f.initialize(nodes);
            f.links(links);
            f(1);
        }
    }
}