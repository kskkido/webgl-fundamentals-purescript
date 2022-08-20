'use strict';

// module Lib.Uint8Array.Main

exports.fromArrayImpl = function (xs) {
  return function () {
    return new Uint8Array(xs);
  };
};

exports.toArrayBufferImpl = function (xs) {
  return xs;
};
