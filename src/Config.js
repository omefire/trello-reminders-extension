"use strict";

exports.getChromeRuntime = function() {
    /*return function() {
        alert("Chrome runtime: " + chrome.runtime);
        return chrome.runtime;
        };*/
    //alert("Chrome runtime: " + chrome.runtime);
    return chrome.runtime;
};

exports.getURL = function(chromeRuntime) {
    return function(filePath) {
        return chromeRuntime.getURL(filePath);
        /*return function() {
            alert(chromeRuntime);
            return chromeRuntime.getURL(filePath);
        };*/
    };
};

exports.getURL1 = function(filePath) {
    return function() {
        return chrome.runtime.getURL(filePath);
    };
};

exports.getURL2 = function(filePath) {
    return chrome.runtime.getURL(filePath);
};
