with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Sequential_IO;

package body MNIST_Model is

  function CreateModel(
    InputLayerSize, HiddenLayerSize, OutputLayerSize : Natural;
    RandomWeights : Boolean := True
  ) return Model_Type is
    Model : Model_Type := (
      InputLayer => CreateInputLayer(InputLayerSize),

      HiddenLayer => CreateLayer(InputLayerSize, HiddenLayerSize),

      OutputLayer => CreateLayer(HiddenLayerSize, OutputLayerSize)
    );

    use Ada.Numerics.Float_Random;
    Gen : Ada.Numerics.Float_Random.Generator;
  begin

    if RandomWeights then

      -- Interesting (re)-discovery:
      -- The range of the initial weights is very important to if the trained model
      -- will will fit at all or even do anyway...

      -- Random hidden layer weights
      for PixelIndex in Model.InputLayer.Outputs'Range loop
        for HiddenNodeIndex in Model.HiddenLayer.Inputs'Range loop
          Model.HiddenLayer.Weights(PixelIndex, HiddenNodeIndex)
            := Long_Float(Random(Gen)) - 0.5; -- -0.5 to 0.5
        end loop;
      end loop;

      -- Random output layer weights
      for HiddenNodeIndex in Model.HiddenLayer.Outputs'Range loop
        for OutputNodeIndex in Model.OutputLayer.Inputs'Range loop
          Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex)
            -- Init with 'good' random crap
            := ((Long_Float(Random(Gen)) - 0.5) * 20.0) / (10.0 * Long_Float(Model.OutputLayer.Outputs'Length));
        end loop;
      end loop;
    end if;

    return Model;
  end;

  procedure Perceptron(MNIST_Model : Model_Type) is
  begin
    -- https://towardsdatascience.com/what-the-hell-is-perceptron-626217814f53

    -- Clear inputs
    for I in MNIST_Model.HiddenLayer.Inputs'Range loop
      MNIST_Model.HiddenLayer.Inputs(I) := 0.0;
    end loop;

    for I in MNIST_Model.OutputLayer.Inputs'Range loop
      MNIST_Model.OutputLayer.Inputs(I) := 0.0;
    end loop;

    -- Calculate hidden layer inputs
    for PixelIndex in MNIST_Model.InputLayer.Outputs'Range loop
      for NodeIndex in MNIST_Model.HiddenLayer.Inputs'Range loop
        MNIST_Model.HiddenLayer.Inputs(NodeIndex)
          := MNIST_Model.HiddenLayer.Inputs(NodeIndex)
             + (MNIST_Model.InputLayer.Outputs(PixelIndex)
                * MNIST_Model.HiddenLayer.Weights(PixelIndex, NodeIndex));
      end loop;
    end loop;

    -- Calculate hidden layer outputs (mix in some tastey nonlinearity)
    for NodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      MNIST_Model.HiddenLayer.Outputs(NodeIndex) := Sigmoid(
        MNIST_Model.HiddenLayer.Inputs(NodeIndex)
      );
    end loop;

    -- Calculate output layer inputs
    for HiddenLayerNodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      for OutputLayerNodeIndex in MNIST_Model.OutputLayer.Inputs'Range loop
        MNIST_Model.OutputLayer.Inputs(OutputLayerNodeIndex)
          := MNIST_Model.OutputLayer.Inputs(OutputLayerNodeIndex)
             + (MNIST_Model.HiddenLayer.Outputs(HiddenLayerNodeIndex)
                * MNIST_Model.OutputLayer.Weights(HiddenLayerNodeIndex, OutputLayerNodeIndex));
      end loop;
    end loop;

    -- Calculate output layer outputs
    for NodeIndex in MNIST_Model.OutputLayer.Outputs'Range loop
      MNIST_Model.OutputLayer.Outputs(NodeIndex) := Sigmoid(
        MNIST_Model.OutputLayer.Inputs(NodeIndex)
      );
    end loop;
    -- end magic
  end;

  package Long_Float_IO is new Ada.Sequential_IO (Long_Float);

  procedure WriteModelFile(FilePath : String; MNIST_Model : Model_Type) is
    use Long_Float_IO;
    OutFile : Long_Float_IO.File_Type;
  begin
    Long_Float_IO.Create(
      File => OutFile,
      Mode => Long_Float_IO.Out_File,
      Name => FilePath
    );

    for PixelIndex in MNIST_Model.InputLayer.Outputs'Range loop
      for HiddenNodeIndex in MNIST_Model.HiddenLayer.Inputs'Range loop
        Write(OutFile, MNIST_Model.HiddenLayer.Weights(PixelIndex, HiddenNodeIndex));
      end loop;
    end loop;

    for HiddenNodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      for OutputNodeIndex in MNIST_Model.OutputLayer.Inputs'Range loop
        Write(OutFile, MNIST_Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex));
      end loop;
    end loop;

    Close(OutFile);
  end;

  function ReadModelFile(
    FilePath : String;
    InputLayerSize, HiddenLayerSize, OutputLayerSize : Natural
  ) return Model_Type is
    use Long_Float_IO;
    InFile : Long_Float_IO.File_Type;
    MNIST_Model : Model_Type := CreateModel(
      InputLayerSize, HiddenLayerSize, OutputLayerSize,
      RandomWeights => False);
  begin
    Long_Float_IO.Open(
      File => InFile,
      Mode => Long_Float_IO.In_File,
      Name => FilePath
    );

    for PixelIndex in MNIST_Model.InputLayer.Outputs'Range loop
      for HiddenNodeIndex in MNIST_Model.HiddenLayer.Inputs'Range loop
        Read(InFile, MNIST_Model.HiddenLayer.Weights(PixelIndex, HiddenNodeIndex));
      end loop;
    end loop;

    for HiddenNodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      for OutputNodeIndex in MNIST_Model.OutputLayer.Inputs'Range loop
        Read(InFile, MNIST_Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex));
      end loop;
    end loop;

    Close(InFile);
    return MNIST_Model;
  end;

  procedure FreeModel(MNIST_Model : in out Model_Type) is
  begin
    FreeInputLayer(MNIST_Model.InputLayer);
    FreeLayer(MNIST_Model.HiddenLayer);
    FreeLayer(MNIST_Model.OutputLayer);
  end;

end MNIST_Model;
