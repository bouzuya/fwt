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

exports.getMediaStream = function () {
  return navigator
    .mediaDevices
    .getUserMedia({ audio: false, video: true });
};

exports.getSourceObjectImpl = function (e, Just, Nothing) {
  return function () {
    if (e.srcObject === null) {
      return Nothing;
    } else {
      return Just(e.srcObject);
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

exports.setSourceObject = function (srcObject) {
  return function (e) {
    return function () {
      e.srcObject = srcObject;
    };
  };
};

exports.videoElementToCanvasImageSource = function (e) {
  return e;
};
