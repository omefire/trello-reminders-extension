"use strict";

exports.alert = function(string) {
    return function() {
        alert(string);
    }
}