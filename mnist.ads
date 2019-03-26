with Interfaces;  use Interfaces;

package MNIST is
  type Image is array (Natural range <>, Natural range <>) of Unsigned_8;
  subtype Label is Unsigned_8 range 0 .. 9;

  type Image_Ptr is access Image;

  type Image_Set is array (Natural range <>) of Image_Ptr;
  type Label_Set is array (Natural range <>) of Label;

  type Image_Set_Ptr is access Image_Set;
  type Label_Set_Ptr is access Label_Set;

  procedure SetBaseDir (BaseDir : String);

  procedure LoadTrain;
  procedure LoadTest;

  function GetTrainImages return Image_Set_Ptr;
  function GetTrainLabels return Label_Set_Ptr;

  function GetTestImages return Image_Set_Ptr;
  function GetTestLabels return Label_Set_Ptr;

  procedure FreeData;
end MNIST;
