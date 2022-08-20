'use strict';

// module Lib.Window.Image.Main

exports.fromUrlImpl = function (url) {
  return function (onError, onSuccess) {
    try {
      const image = new Image();
      image.src = url;
      image.onload = () => onSuccess(image);
    } catch (e) {
      onError(e);
    }
    return function (error, onCancelError, onCancelSuccess) {
      onCancelSuccess();
    };
  };
};

exports.toTexImageSourceImpl = function (image) {
  return image;
}

exports.getWidthImpl = function (image) {
  return function () {
    return image.width;
  };
};

exports.getHeightImpl = function (image) {
  return function () {
    return image.height;
  };
};
