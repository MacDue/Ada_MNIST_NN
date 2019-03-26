with MNIST;

package Convolution is

  type Conv_Fitler is array (Natural range <>, Natural range <>) of Float;
  type Conv_Feature is array (Natural range <>, Natural range <>) of Float;
  type Conv_Bias is array (Natural range <>) of Float;

  function convolve(
    Image : Image_Ptr,
    Fitler : Conv_Fitler,
    Bias : Conv_Bias,
    Stride : Natural := 1
  ) return Conv_Feature;

end Convolution;
