"use strict";

// module Data.Graph.Force.Link

var d3 = require("d3/d3");

exports.moveNodes = function (nodes) {
    var n;
    for (var i=0; i<nodes.length; ++i) {
        n = nodes[i];
        n.x += n.vx;
        n.y += n.vy;
    }
}

exports.dampVelocity = function (s) {
    return function(nodes) {
        var n;
        for (var i=0; i<nodes.length; ++i) {
            n = nodes[i];
            n.vx *= s;
            n.vy *= s;
        }
    }
}

exports.pull = function(strength) {
    return function(cx) {
        return function(cy) {
            return function(nodes) {
                var n, dx, dy, m2;
                for (var i=0; i<nodes.length; ++i) {
                    n = nodes[i];
                    dx = cx - n.x;
                    dy = cy - n.y;
                    m2 = dx * dx + dy * dy;
                    n.vx += strength * dx / m2;
                    n.vy += strength * dy / m2;
                }
            }
        }
    }
}

exports.fixPositions = function(positions) {
    return function(nodes) {
        var n;
        for (var i=0; i<nodes.length; ++i) {
            n = nodes[i];
            if (positions.hasOwnProperty(n.id)) {
                n.x = positions[n.id].x;
                n.y = positions[n.id].y;
                n.vx = 0;
                n.vy = 0;
            }
        }
    }
}

exports.pull = function(strength) {
    return function(cx) {
        return function(cy) {
            return function(nodes) {
                var n, dx, dy, m2;
                for (var i=0; i<nodes.length; ++i) {
                    n = nodes[i];
                    dx = cx - n.x;
                    dy = cy - n.y;
                    m2 = dx * dx + dy * dy;
                    n.vx += strength * dx / m2;
                    n.vy += strength * dy / m2;
                }
            }
        }
    }
}

exports.iterateForce = function(count) {
    return function (force) {
        return function (nodes) {
            for (var i=0; i<count; ++i) {
                force(nodes);
            }
        }
    }
}

exports.applyForceNodes = function(force) {
    return function(nodes) {
        var nodesCopy = nodes.map(function(node, i) {
            node = Object.assign({}, node);
            node.index = i;
            return node;
        });
        force(nodesCopy);
        return nodesCopy;
    }
}

exports.appendForce = function(f1) {
    return function(f2) {
        return function(nodes) {
            f1(nodes);
            f2(nodes);
        }
    }
}

exports.emptyForce = function(nodes) {
    return;
}