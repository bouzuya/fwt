"use strict";

exports.getVideoElementByIdImpl = function (id, Just, Nothing) {
  return function () {
    var e = document.getElementById(id);
    if (e !== null && e instanceof HTMLVideoElement) {
      return Just(e);
    } else {
      return Nothing;
    }
  };
};

exports.getVideoHeight = function (e) {
  return function () {
    return e.videoHeight;
  };
};

exports.getVideoWidth = function (e) {
  return function () {
    return e.videoWidth;
  };
};

exports.videoElementToCanvasImageSource = function (e) {
  return e;
};
