module Shaders.Shape3d.Models.DrawConfig.Main where

import Prelude
import Data.Map as Map
import Data.Maybe as Maybe

type DrawConfig =
  { scale ::
    { x :: Number
    , y :: Number
    , z :: Number
    }
  , position ::
    { x :: Number
    , y :: Number
    , z :: Number
    }
  , rotation ::
    { x :: Number
    , y :: Number
    , z :: Number
    }
  }

fromMap :: Map.Map String Number -> DrawConfig
fromMap mx =
  { scale:
    { x: Maybe.maybe 1.0 identity $ Map.lookup "scale.x" mx
    , y: Maybe.maybe 1.0 identity $ Map.lookup "scale.y" mx
    , z: Maybe.maybe 1.0 identity $ Map.lookup "scale.z" mx
    }
  , position:
    { x: Maybe.maybe 0.0 identity $ Map.lookup "position.x" mx
    , y: Maybe.maybe 0.0 identity $ Map.lookup "position.y" mx
    , z: Maybe.maybe 0.0 identity $ Map.lookup "position.z" mx
    }
  , rotation:
    { x: Maybe.maybe 0.0 identity $ Map.lookup "rotation.x" mx
    , y: Maybe.maybe 0.0 identity $ Map.lookup "rotation.y" mx
    , z: Maybe.maybe 0.0 identity $ Map.lookup "rotation.z" mx
    }
  }

