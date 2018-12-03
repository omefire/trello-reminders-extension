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
	    return require('../../bower_components/purescript-nullable/src/Data/Nullable').notNull(val);
	}
    }
}

exports._head = function(document) {
    return document.head;
}

exports.setOnLoad = function(element) {
    return function(callback) {
	return function() {
	    element.onload = callback;
	}
    }
}


exports.showModal = function(dialog) {
    return function() {
	return dialog.showModal();
    }
}

exports.show = function(dialog) {
    return function() {
	return dialog.show();
    }
}

exports._setTimeout = function(fn) {
    return function(msecs) {
        return setTimeout(fn, msecs);
    }
}

exports._setInterval = function(fn) {
    return function(msecs) {
        return setInterval(fn, msecs);
    }
}

exports._flatpickr = function(selector) {
    return function(config) {
        flatpickr(selector, config);
    }
}
