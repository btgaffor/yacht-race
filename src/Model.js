"use strict";

exports.squishSystemArray = function(effects) {
    return function() {
        for (var i = 0; i < effects.length; i++) {
            effects[i]();
        }
    };
};

exports.travelSystemArray = function(effects) {
    return function() {
        var rv = new Array(effects.length);
        for (var i = 0; i < effects.length; i++) {
            rv[i] = effects[i]();
        }
        return rv;
    };
};
