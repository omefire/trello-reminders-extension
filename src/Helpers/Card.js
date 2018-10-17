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

exports._showDialog = function() {
    alert("TTT");
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

//var jQuery = require('../../bower_components/jquery/dist/jquery');
//var jQueryDialog = require('../../bower_components/jquery-ui/ui/widgets/dialog');

//var JSDOM = require("../../node_modules/jsdom").JSDOM;
//var window = new JSDOM();
//var document = (new JSDOM('')).window;
//global.document = document;

//var $ = require('../../node_modules/jquery-ui/ui/jquery-1-7'); //require('jquery')(window);
//var jQuery = $;

//require('../../node_modules/jquery-ui/ui/widgets/dialog');

/*var $ = require('jquery');
var jQuery = $;
window.jQuery = jQuery;
window.$ = jQuery;
//require('jquery-ui');*/
//require('../../node_modules/jquery-ui/ui/jquery-1-7');
//require('../../node_modules/jquery-ui/ui/widgets/dialog');
//require('jquery-ui/ui/widgets/dialog');

exports.jqry = function(selector) {
    return function() {
	return $(selector);
    }
}

exports.dialog = function(elt) {
    return function() {
	return elt.dialog();
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
