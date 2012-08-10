{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.ImageMagick.MagickCore.Types.FFI.Statistic
    where

import           Foreign.C.Types
#include <magick/MagickCore.h>

newtype MagickEvaluateOperator = MagickEvaluateOperator { unMagickEvaluateOperator :: CInt }
          deriving (Eq, Show)

#{enum MagickEvaluateOperator, MagickEvaluateOperator
  , undefinedEvaluateOperator = UndefinedEvaluateOperator
  , addEvaluateOperator = AddEvaluateOperator
  , andEvaluateOperator = AndEvaluateOperator
  , divideEvaluateOperator = DivideEvaluateOperator
  , leftShiftEvaluateOperator = LeftShiftEvaluateOperator
  , maxEvaluateOperator = MaxEvaluateOperator
  , minEvaluateOperator = MinEvaluateOperator
  , multiplyEvaluateOperator = MultiplyEvaluateOperator
  , orEvaluateOperator = OrEvaluateOperator
  , rightShiftEvaluateOperator = RightShiftEvaluateOperator
  , setEvaluateOperator = SetEvaluateOperator
  , subtractEvaluateOperator = SubtractEvaluateOperator
  , xorEvaluateOperator = XorEvaluateOperator
  , powEvaluateOperator = PowEvaluateOperator
  , logEvaluateOperator = LogEvaluateOperator
  , thresholdEvaluateOperator = ThresholdEvaluateOperator
  , thresholdBlackEvaluateOperator = ThresholdBlackEvaluateOperator
  , thresholdWhiteEvaluateOperator = ThresholdWhiteEvaluateOperator
  , gaussianNoiseEvaluateOperator = GaussianNoiseEvaluateOperator
  , impulseNoiseEvaluateOperator = ImpulseNoiseEvaluateOperator
  , laplacianNoiseEvaluateOperator = LaplacianNoiseEvaluateOperator
  , multiplicativeNoiseEvaluateOperator = MultiplicativeNoiseEvaluateOperator
  , poissonNoiseEvaluateOperator = PoissonNoiseEvaluateOperator
  , uniformNoiseEvaluateOperator = UniformNoiseEvaluateOperator
  , cosineEvaluateOperator = CosineEvaluateOperator
  , sineEvaluateOperator = SineEvaluateOperator
  , addModulusEvaluateOperator = AddModulusEvaluateOperator
  , meanEvaluateOperator = MeanEvaluateOperator
  , absEvaluateOperator = AbsEvaluateOperator
  , exponentialEvaluateOperator = ExponentialEvaluateOperator
  , medianEvaluateOperator = MedianEvaluateOperator
  , sumEvaluateOperator = SumEvaluateOperator
}

