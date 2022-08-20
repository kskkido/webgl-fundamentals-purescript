module Pages.Shaders.Animation.Models.ShaderConfig.Main where

import Prelude
import Data.Int as Int
import Data.Map as Map
import Data.Maybe as Maybe
import Shaders.Animation.Models.DrawConfig.Main as DrawConfig

type ShaderConfig =
  { pause :: Int
  , angle :: Number
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
  , rotationSpeed ::
    { x :: Number
    , y :: Number
    , z :: Number
    }
  }

unit :: ShaderConfig
unit =
  { pause: 0
  , angle: 0.0
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
  , rotationSpeed:
    { x: 0.0
    , y: 0.0
    , z: 0.0
    }
  }

fromMap :: Map.Map String Number -> ShaderConfig
fromMap mx =
  { pause: Maybe.maybe unit.pause Int.floor $ Map.lookup "pause" mx
  , angle: Maybe.maybe unit.angle identity $ Map.lookup "angle" mx
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
  , rotationSpeed:
    { x: Maybe.maybe unit.rotationSpeed.x identity $ Map.lookup "rotationSpeed.x" mx
    , y: Maybe.maybe unit.rotationSpeed.y identity $ Map.lookup "rotationSpeed.y" mx
    , z: Maybe.maybe unit.rotationSpeed.z identity $ Map.lookup "rotationSpeed.z" mx
    }
  }

toDrawConfig :: Number -> ShaderConfig -> DrawConfig.DrawConfig -> DrawConfig.DrawConfig
toDrawConfig delta sc dc =
  { angle: sc.angle
  , radius: sc.radius
  , count: sc.count
  , fieldOfView: sc.fieldOfView
  , scale: sc.scale
  , position: sc.position
  , rotation:
    { x: dc.rotation.x + sc.rotationSpeed.x * delta
    , y: dc.rotation.y + sc.rotationSpeed.y * delta
    , z: dc.rotation.z + sc.rotationSpeed.z * delta
    }
  }

