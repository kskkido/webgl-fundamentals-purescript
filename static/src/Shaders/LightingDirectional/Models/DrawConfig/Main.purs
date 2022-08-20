module Shaders.LightingDirectional.Models.DrawConfig.Main where

import Prelude
import Data.Int as Int
import Data.Map as Map
import Data.Maybe as Maybe

type DrawConfig =
  { angle :: Number
  , radius :: Number
  , count :: Int
  , fieldOfView :: Number
  , scale ::
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
  , light ::
    { x :: Number
    , y :: Number
    , z :: Number
    }
  }

unit :: DrawConfig
unit =
  { angle: 0.0
  , radius: 200.0
  , count: 1
  , fieldOfView: 60.0
  , scale:
    { x: 1.0
    , y: 1.0
    , z: 1.0
    }
  , position:
    { x: 0.0
    , y: 0.0
    , z: (-400.0)
    }
  , rotation:
    { x: 0.0
    , y: 0.0
    , z: 0.0
    }
  , light:
    { x: 0.0
    , y: 0.0
    , z: 0.0
    }
  }

fromMap :: Map.Map String Number -> DrawConfig
fromMap mx =
  { angle: Maybe.maybe unit.angle identity $ Map.lookup "angle" mx
  , radius: Maybe.maybe unit.radius identity $ Map.lookup "radius" mx
  , count: Maybe.maybe unit.count Int.floor $ Map.lookup "count" mx
  , fieldOfView: Maybe.maybe unit.fieldOfView identity $ Map.lookup "fieldOfView" mx
  , scale:
    { x: Maybe.maybe unit.scale.x identity $ Map.lookup "scale.x" mx
    , y: Maybe.maybe unit.scale.y identity $ Map.lookup "scale.y" mx
    , z: Maybe.maybe unit.scale.z identity $ Map.lookup "scale.z" mx
    }
  , position:
    { x: Maybe.maybe unit.position.x identity $ Map.lookup "position.x" mx
    , y: Maybe.maybe unit.position.y identity $ Map.lookup "position.y" mx
    , z: Maybe.maybe unit.position.z identity $ Map.lookup "position.z" mx
    }
  , rotation:
    { x: Maybe.maybe unit.rotation.x identity $ Map.lookup "rotation.x" mx
    , y: Maybe.maybe unit.rotation.y identity $ Map.lookup "rotation.y" mx
    , z: Maybe.maybe unit.rotation.z identity $ Map.lookup "rotation.z" mx
    }
  , light:
    { x: Maybe.maybe unit.light.x identity $ Map.lookup "light.x" mx
    , y: Maybe.maybe unit.light.y identity $ Map.lookup "light.y" mx
    , z: Maybe.maybe unit.light.z identity $ Map.lookup "light.z" mx
    }
  }


