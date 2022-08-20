'use strict';

// module Graphics.WebGL.Raw.Util

exports.nullAsEmpty = function (x) {
  if (x === undefined || x === null) {
    return [];
  } else {
    return x;
  }
};
