package Image is

  type Unsigned_Byte is mod 2**8;

  type ImagePixels is
    array (Natural range <>, Natural range <>) of Unsigned_Byte;

  function ReadPNG (FilePath : String) return ImagePixels;

end Image;
