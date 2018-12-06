// Generated by BUCKLESCRIPT VERSION 4.0.7, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Tea_sub = require("./tea_sub.js");

function every($staropt$star, tagger) {
  var key = $staropt$star !== undefined ? $staropt$star : "";
  var enableCall = function (callbacks) {
    var lastTime = /* record */[/* contents */Date.now()];
    var id = /* record */[/* contents */undefined];
    var onFrame = function (_time) {
      var time = Date.now();
      var match = id[0];
      if (match !== undefined) {
        var ret_001 = /* delta */time < lastTime[0] ? 0.0 : time - lastTime[0];
        var ret = /* record */[
          /* time */time,
          ret_001
        ];
        lastTime[0] = time;
        Curry._1(callbacks[/* enqueue */0], Curry._1(tagger, ret));
        var match$1 = id[0];
        if (match$1 !== undefined) {
          id[0] = window.requestAnimationFrame(onFrame);
          return /* () */0;
        } else {
          return /* () */0;
        }
      } else {
        return /* () */0;
      }
    };
    id[0] = window.requestAnimationFrame(onFrame);
    return (function (param) {
        var match = id[0];
        if (match !== undefined) {
          window.cancelAnimationFrame(match);
          id[0] = undefined;
          return /* () */0;
        } else {
          return /* () */0;
        }
      });
  };
  return Tea_sub.registration(key, enableCall);
}

function times($staropt$star, tagger) {
  var key = $staropt$star !== undefined ? $staropt$star : "";
  return every(undefined, (function (ev) {
                return Curry._2(tagger, key, ev[/* time */0]);
              }));
}

function diffs($staropt$star, tagger) {
  var key = $staropt$star !== undefined ? $staropt$star : "";
  return every(undefined, (function (ev) {
                return Curry._2(tagger, key, ev[/* delta */1]);
              }));
}

exports.every = every;
exports.times = times;
exports.diffs = diffs;
/* No side effect */