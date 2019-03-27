with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with MNIST;
with MNIST_Training; use MNIST_Training;
with MNIST_Model; use MNIST_Model;

procedure MNIST_Train_Main is
  TrainImages : MNIST.Image_Set_Ptr;
  TrainLabels : MNIST.Label_Set_Ptr;
  MNIST_Model : Model_Type;
begin
  if Argument_Count /= 1 then
    Put_Line("Provide a file path for the model to be saved");
    Set_Exit_Status(Failure);
    return;
  end if;

  MNIST.LoadTrain;
  TrainImages := MNIST.GetTrainImages;
  TrainLabels := MNIST.GetTrainLabels;

  MNIST_Model := CreateModel(
    InputLayerSize => (TrainImages(0)'Last(1)+1) * (TrainImages(0)'Last(2)+1),
    HiddenLayerSize => 128,
    OutputLayerSize => 10
  );

  -- Run training...
  declare
    SampleTrainingEpoch : Positive;
    OutputFilePath : constant String := Argument(1);
  begin
    for SampleIndex in TrainImages'Range loop
      Put_Line("Training on sample"
        & Natural'Image(SampleIndex +1) & " /" & Natural'Image(TrainImages'Last +1));

      SampleTrainingEpoch := TrainingStep(
        MNIST_Model,
        TrainImages(SampleIndex),
        TrainLabels(SampleIndex)
      );

      Put_Line("Done in" & Positive'Image(SampleTrainingEpoch) & " epochs");
      New_Line;
    end loop;

    -- Save model...
    Put_Line("Writing model to: " & OutputFilePath);
    WriteModelFile(OutputFilePath, MNIST_Model);
  end;

  -- Clean up...
  MNIST.FreeData;
end;
