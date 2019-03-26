with PNG_IO;
use  PNG_IO;

package body Image is

  function ReadPNG (FilePath : String) return ImagePixels is
    PngFile : PNG_File;
  begin

    declare
      ImageWidth : constant Dimension := Width(PngFile);
      ImageHeight : constant Dimension := Height(PngFile);
      ImageDepth : constant Depth := Bit_Depth(PngFile);
      ImageColorType : constant Colour_Type_Code := Colour_Type(PngFile);

      subtype Row_Coordinate is Coordinate range 0 .. ImageHeight - 1;
      subtype Col_Coordinate is Coordinate range 0 .. ImageWidth - 1;

      ReadPixels: ImagePixels (0 .. ImageWidth - 1, 0 .. ImageHeight - 1) :=
        (others => (others => 0));

      -- generic
      --   with function Value(F : PNG_File; R, C : Coordinate) return Natural;
      -- function ReadPixel (R, C: Coordinate) return Unsigned_Byte;
      --
      -- function ReadPixel(R, C: Coordinate) return Unsigned_Byte is
      --   Pixel : constant Unsigned_Byte := Unsigned_Byte(Value(PngFile, R, C));
      -- begin
      --   return Pixel;
      -- end;
      CurrentPixel : Unsigned_Byte := 0;
    begin
      pragma Assert (ImageDepth = Eight);
      pragma Assert (not Colour(ImageColorType));

      for C in Col_Coordinate loop
        for R in Row_Coordinate loop
          ReadPixels(Natural(C), Natural(R)) := Unsigned_Byte(Pixel_Value(PngFile, R, C));
        end loop;
      end loop;
      return ReadPixels;
    end;
  end;

end Image;
