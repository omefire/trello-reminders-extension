"use strict";

exports._getXFromUrl = function (url) {
    return function (prefix) {
        return function () {
            if (!url || url.indexOf(prefix) != 0)
                return null;

            var remainUrl = url.slice(prefix.length);
            var iNextSlash = remainUrl.indexOf("/");
            if (iNextSlash >= 0)
                remainUrl = remainUrl.slice(0, iNextSlash);
            return require('../../bower_components/purescript-nullable/src/Data/Nullable').notNull(remainUrl);
        }
    }
}

exports._nextSibling = function(elt) {
    return function() {
	return elt.nextSibling;
    }
}

exports.alert = function(obj) {
    return function() {
	alert(JSON.stringify(obj));
    }
}

exports._getElementById = function(id) {
    return function(document) {
	var val = document.getElementById(id);
	if(!val) {
	    return null;
	} else {
	    //return require('../../bower_components/purescript-nullable/src/Data/Nullable').null;
	    return require('../../bower_components/purescript-nullable/src/Data/Nullable').notNull(val);
	}
	/*return function() {
	    var val = document.getElementById(id);
	    if(!val) {
		return null;
	    } else {
		//return require('../../bower_components/purescript-nullable/src/Data/Nullable').null;
		return require('../../bower_components/purescript-nullable/src/Data/Nullable').notNull(val);
	    }
	}*/
    }
}
