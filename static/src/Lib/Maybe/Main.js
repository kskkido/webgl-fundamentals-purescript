'use strict';

// module Graphics.WebGL.Raw.Util

exports.fromImpl = function (Nothing, Just, x) {
  if (x === undefined || x === null) {
    return Nothing;
  } else {
    return Just(x);
  }
};
