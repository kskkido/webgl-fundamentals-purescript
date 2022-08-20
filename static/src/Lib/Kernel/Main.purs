module Lib.Kernel.Main where

import Prelude
import Math as Math
import Data.Int as Int
import Data.Foldable as Foldable
import Lib.KernelType.Main as Lib.KernelType

type Kernel = Array Number

fromKernelType :: Lib.KernelType.KernelType -> Kernel
fromKernelType Lib.KernelType.Identity1 = identity1
fromKernelType Lib.KernelType.GaussianBlur1 = gaussianBlur1
fromKernelType Lib.KernelType.Unsharpen1 = unsharpen1
fromKernelType Lib.KernelType.Sharpen1 = sharpen1
fromKernelType Lib.KernelType.Sharpness1 = sharpness1

toWeight :: Kernel -> Number
toWeight = Math.max 1.0 <<< Foldable.sum

identity1 :: Kernel
identity1 = [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0]

gaussianBlur1 :: Kernel
gaussianBlur1 = [0.045, 0.122, 0.045, 0.122, 0.332, 0.122, 0.045, 0.122, 0.045]

unsharpen1 :: Kernel
unsharpen1 = [-1.0, -1.0, -1.0, -1.0, 9.0, -1.0, -1.0, -1.0, -1.0]

sharpen1 :: Kernel
sharpen1 = [-1.0, -1.0, -1.0, -1.0, 16.0, -1.0, -1.0, -1.0, -1.0]

sharpness1 :: Kernel
sharpness1 = [0.0, -1.0, 0.0, -1.0, 5.0, -1.0, 0.0, -1.0, 0.0]

