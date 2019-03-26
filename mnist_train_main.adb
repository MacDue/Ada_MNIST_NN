with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with GNAT.OS_Lib;

with MNIST;
with MNIST_Training; use MNIST_Training;
with MNIST_Model; use MNIST_Model;

procedure MNIST_Train_Main is
  TrainImages : MNIST.Image_Set_Ptr;
  TrainLabels : MNIST.Label_Set_Ptr;
  MNIST_Model : Model_Type;
begin

  if Argument_Count /= 1 then
    Put_Line("Provide a file path for the model save");
    GNAT.OS_Lib.OS_Exit(1);
  end if;

  MNIST.LoadTrain;
  TrainImages := MNIST.GetTrainImages;
  TrainLabels := MNIST.GetTrainLabels;

  MNIST_Model := CreateModel(
    InputLayerSize => (TrainImages(0)'Last(1)+1) * (TrainImages(0)'Last(2)+1),
    HiddenLayerSize => 128,
    OutputLayerSize => 10
  );

  for SampleIndex in TrainImages'Range loop
    Put_Line("Training on sample"
      & Natural'Image(SampleIndex +1) & " /" & Natural'Image(TrainImages'Last +1));

    TrainingStep(
      MNIST_Model,
      TrainImages(SampleIndex),
      TrainLabels(SampleIndex)
    );
  end loop;

  declare
    OutputFilePath : constant String := Argument(1);
  begin
    Put_Line("Writing model to: " & OutputFilePath);
    WriteModelFile(Argument(1), MNIST_Model);
  end;

  MNIST.FreeData;
end;
