module Lib.KernelType.Main where

data KernelType =
    Identity1
  | GaussianBlur1
  | Unsharpen1
  | Sharpness1
  | Sharpen1

toString :: KernelType -> String
toString Identity1 = "identity1"
toString GaussianBlur1 = "gaussianBlur1"
toString Unsharpen1 = "unsharpen1"
toString Sharpen1 = "sharpen1"
toString Sharpness1 = "sharpness1"
