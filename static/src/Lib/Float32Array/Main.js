'use strict';

// module Lib.Float32Array.Main

exports.fromArrayImpl = function (xs) {
  return function () {
    return new Float32Array(xs);
  };
};

exports.toArrayBufferImpl = function (xs) {
  return xs;
};
