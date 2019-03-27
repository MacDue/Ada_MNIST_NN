with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces; use Interfaces;

with MNIST;
with MNIST_Model; use MNIST_Model;

procedure MNIST_Test_Main is
  TestImages : MNIST.Image_Set_Ptr;
  TestLabels : MNIST.Label_Set_Ptr;
  MNIST_Model : Model_Type;
begin
  if Argument_Count /= 1 then
    Put_Line("Provide a file path to the model to load");
    Set_Exit_Status(Failure);
    return;
  end if;

  MNIST.LoadTest;
  TestImages := MNIST.GetTestImages;
  TestLabels := MNIST.GetTestLabels;

  declare
    InputFilePath : constant String := Argument(1);
    CorrectClassifications : Natural := 0;

    TestImage : MNIST.Image_Ptr;
    TestLabel : MNIST.Label;

    PredictedClass : MNIST.Label := 0;
  begin
    MNIST_Model := ReadModelFile(
      FilePath => InputFilePath,
      InputLayerSize => (TestImages(0)'Last(1)+1) * (TestImages(0)'Last(2)+1),
      HiddenLayerSize => 128,
      OutputLayerSize => 10
    );

    for TestIndex in TestImages'Range loop
      TestImage := TestImages(TestIndex);
      TestLabel := TestLabels(TestIndex);

      -- (TODO MOVE TO LIB) Set the input layer
      for Y in TestImage'Range(2) loop
        for X in TestImage'Range(1) loop
          -- Put (if TestImage(X, Y) /= 0 then '@' else ' ');
          MNIST_Model.InputLayer.Outputs(
            X + (Y * (TestImage'Last(1) + 1))
          ) := (if TestImage(X, Y) = 0 then 0.0 else 1.0);
        end loop;
        -- New_Line;
      end loop;

      -- Pass input here...
      Perceptron(MNIST_Model);

      PredictedClass := 0;
      for Class in MNIST_Model.OutputLayer.Outputs'First .. MNIST_Model.OutputLayer.Outputs'Last loop
        if MNIST_Model.OutputLayer.Outputs(Natural(PredictedClass))
            < MNIST_Model.OutputLayer.Outputs(Class) then
          PredictedClass := MNIST.Label(Class);
        end if;
        Put_Line("Class" & Natural'Image(Class) &" =>" & Long_Float'Image(MNIST_Model.OutputLayer.Outputs(Class)));
      end loop;

      Put_Line("Predicted" & MNIST.Label'Image(PredictedClass) & " was" & MNIST.Label'Image(TestLabel));

      if PredictedClass = TestLabel then
        CorrectClassifications := CorrectClassifications + 1;
        Put_Line("Correct!");
      else
        Put_Line("Wrong!");
      end if;
    end loop;

    Put_Line("Accuracy: " & Float'Image(Float(CorrectClassifications) / Float(TestLabels'Length)));
  end;

  MNIST.FreeData;

end;
