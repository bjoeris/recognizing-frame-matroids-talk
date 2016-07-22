"use strict";

var d3 = require("d3/d3");

exports.geoCircleImpl = function(options) {
    return d3.geoCircle()
      .center(options.center)
      .radius(options.radius)
      .precision(options.precision)();
};

exports.geoDistance = function(point1) {
    return function(point2) {
        return d3.geoDistance(point1, point2);
    }
}

function caching(fn) {
    return {
        newValue : fn,
    }
}

function cachingUpdate(fn) {
    return function(c) {
        return caching(function() {
            return fn(c.newValue());
        });
    };
}

function getValue(c) {
    if (typeof(c.cachedValue) === 'undefined') {
        c.cachedValue = c.newValue();
    }
    return c.cachedValue;
}

exports.geoOrthographic = caching(d3.geoOrthographic);

exports.clipAngleImpl = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.clipAngle( value );
    });
};

exports.clipExtentImpl = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.clipExtent( value );
    });
};

exports.scale = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.scale( value );
    });
};

exports.translate = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.translate( value );
    });
};

exports.center = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.center( value );
    });
};

exports.rotate = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.rotate( value );
    });
};

exports.precision = function(value) {
    return cachingUpdate( function(projection) { 
        return projection.precision( value );
    });
};

exports.geoPath = caching(d3.geoPath);

exports.pointRadius = function(value) {
    return cachingUpdate( function(path) { 
        return path.pointRadius( value );
    });
};

exports.projectionImpl = function(value) {
    return cachingUpdate( function(path) { 
        return path.projection( getValue(value) );
    });
};

exports.pathStringImpl = getValue;