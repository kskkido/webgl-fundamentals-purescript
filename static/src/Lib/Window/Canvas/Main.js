'use strict';

// module Lib.Window.Canvas.Main

exports.getWebGLContextImpl = function (canvas) {
  return function () {
    return canvas.getContext('webgl');
  };
};

exports.getClientWidthImpl = function (canvas) {
  return function () {
    return canvas.clientWidth;
  };
};

exports.getClientHeightImpl = function (canvas) {
  return function () {
    return canvas.clientHeight;
  };
};

exports.resizeImpl = function (canvas) {
  return function () {
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
  };
};
