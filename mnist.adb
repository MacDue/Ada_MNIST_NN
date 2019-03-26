with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body MNIST is

  package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
  use Byte_IO;

  function ReadInt (File : Byte_IO.File_Type) return Integer_32 is
    type IntBytes_Array is array (Natural range 0 .. 3) of Unsigned_8;
    IntBytes : IntBytes_Array;
    Result : Unsigned_32;
  begin
    -- Load little-endian ints
    Byte_IO.Read(File => File, Item => IntBytes(0));
    Byte_IO.Read(File => File, Item => IntBytes(1));
    Byte_IO.Read(File => File, Item => IntBytes(2));
    Byte_IO.Read(File => File, Item => IntBytes(3));

    -- Shifting is not a build in operation in Ada?!?
    -- (Shift_Left is translated into simple shift op though, not a function call)
    Result := Unsigned_32(IntBytes(3));
    Result := Result or Shift_Left(Unsigned_32(IntBytes(2)), 8);
    Result := Result or Shift_Left(Unsigned_32(IntBytes(1)), 16);
    Result := Result or Shift_Left(Unsigned_32(IntBytes(0)), 24);
    return Integer_32(Result);
  end;

  function LoadImages (ImagesPath : String) return Image_Set_Ptr is
    ImageFile : Byte_IO.File_Type;

    Magic  : Integer_32;
    Size   : Integer_32;
    Width  : Integer_32;
    Height : Integer_32;

  begin
    Byte_IO.Open (
      File => ImageFile,
      Mode => Byte_IO.In_File,
      Name => ImagesPath
    );

    Magic  := ReadInt(ImageFile);
    Size   := ReadInt(ImageFile);
    Width  := ReadInt(ImageFile);
    Height := ReadInt(ImageFile);

    Assert (Magic = 2051, ImagesPath & " is not a MNIST image set!");

    Put_Line ("Loading: " & ImagesPath);
    Put_Line ("Images magic: " & Integer_32'Image(Magic));
    Put_Line ("Image count: "  & Integer_32'Image(Size));
    Put_Line ("Image width: "  & Integer_32'Image(Width));
    Put_Line ("Image Height: " & Integer_32'Image(Height));

    declare
      CurrentByte : Unsigned_8;
      Chunk : constant Natural := Natural(Width) * Natural(Height);
      Images : Image_Set_Ptr := new Image_Set(0 .. Natural(Size -1));
      CurrentImage : Image_Ptr;
    begin
      for I in 0 .. Size - 1 loop
        -- Very nasty...
        -- Load each image into a 2D array of bytes.
        CurrentImage := new Image(0 .. Natural(Width-1), 0 .. Natural(Height-1));
        for J in 0 .. Chunk - 1 loop
          Byte_IO.Read(File => ImageFile, Item => CurrentByte);
          -- Nothing pains me more than looped heap allocation...
          -- Don't know a better way to do this in Ada yet.
          -- We know everything we need to perform one big alloc as soon as read the header.
          CurrentImage(J mod Integer(Width), J / Integer(Width)) := CurrentByte;
        end loop;
        Images(Natural(I)) := CurrentImage;
      end loop;

      -- Done-zo!
      Byte_IO.Close(ImageFile);
      return Images;
    end;
  end;

  function LoadLabels (LabelsPath : String) return Label_Set_Ptr is
    LabelsFile : Byte_IO.File_Type;

    Magic : Integer_32;
    Size  : Integer_32;
  begin
    Byte_IO.Open (
      File => LabelsFile,
      Mode => Byte_IO.In_File,
      Name => LabelsPath
    );

    Magic := ReadInt(LabelsFile);
    Size  := ReadInt(LabelsFile);

    Assert (Magic = 2049, LabelsPath & " is not a MNIST label set!");

    declare
      CurrentByte : Unsigned_8;
      Labels : Label_Set_Ptr := new Label_Set(0 .. Natural(Size -1));
    begin

      for I in 0 .. Size -1 loop
        Byte_IO.Read(File => LabelsFile, Item => CurrentByte);
        Labels(Natural(I)) := CurrentByte;
      end loop;

      Byte_IO.Close(LabelsFile);
      return Labels;
    end;

  end;

  DataBaseDir : Unbounded_String := To_Unbounded_String("./data/");

  procedure SetBaseDir(BaseDir : String) is
  begin
    DataBaseDir := To_Unbounded_String(BaseDir);

    if BaseDir(BaseDir'Last) /= '/' then
      Append(DataBaseDir, '/');
    end if;
  end;


  TrainLabels : Label_Set_Ptr := null;
  TrainImages : Image_Set_Ptr := null;

  TestLabels  : Label_Set_Ptr := null;
  TestImages  : Image_Set_Ptr := null;


  procedure LoadTrain is
    TrainLabelsFileName : constant String := "train-labels.idx1-ubyte";
    TrainImagesFileName : constant String := "train-images.idx3-ubyte";
    BaseDirStr : constant String := To_String(DataBaseDir);
  begin
    TrainLabels := LoadLabels(BaseDirStr & TrainLabelsFileName);
    TrainImages := LoadImages(BaseDirStr & TrainImagesFileName);
  end;


  procedure LoadTest is
    TestLabelsFileName : constant String := "t10k-labels.idx1-ubyte";
    TestImagesFileName : constant String := "t10k-images.idx3-ubyte";
    BaseDirStr : constant String := To_String(DataBaseDir);
  begin
    TestLabels := LoadLabels(BaseDirStr & TestLabelsFileName);
    TestImages := LoadImages(BaseDirStr & TestImagesFileName);
  end;

  function GetTrainImages return Image_Set_Ptr is
  begin
    Assert (TrainImages /= null, "MNIST training images not loaded!");
    return TrainImages;
  end;

  function GetTrainLabels return Label_Set_Ptr is
  begin
    Assert (TrainLabels /= null, "MNIST training labels not loaded!");
    return TrainLabels;
  end;

  function GetTestImages return Image_Set_Ptr is
  begin
    Assert (TestImages /= null, "MNIST test images not loaded!");
    return TestImages;
  end;

  function GetTestLabels return Label_Set_Ptr is
  begin
    Assert (TestLabels /= null, "MNIST test labels not loaded!");
    return TestLabels;
  end;

  procedure FreeData is
    procedure FreeLabelSet is new Ada.Unchecked_Deallocation (Label_Set, Label_Set_Ptr);
    procedure FreeImageSet (Images : in out Image_Set_Ptr) is
      procedure FreeImage is new Ada.Unchecked_Deallocation (Image, Image_Ptr);
      procedure FreeEmptyImageSet is new Ada.Unchecked_Deallocation (Image_Set, Image_Set_Ptr);
    begin
      for I in Images'Range loop
        FreeImage(Images(I));
      end loop;
      FreeEmptyImageSet(Images);
    end;
  begin
    if TrainLabels /= null then
      FreeLabelSet(TrainLabels);
      Put_Line("Freed training labels!");
    end if;

    if TrainImages /= null then
      FreeImageSet(TrainImages);
      Put_Line("Freed training images!");
    end if;

    if TestLabels /= null then
      FreeLabelSet(TestLabels);
      Put_Line("Freed testing labels!");
    end if;

    if TestImages /= null then
      FreeImageSet(TestImages);
      Put_Line("Freed testing images!");
    end if;
  end;

end MNIST;
