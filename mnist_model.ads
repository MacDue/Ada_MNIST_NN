with NN_Utils; use NN_Utils;

package MNIST_Model is

  type Model_Type is record
    -- Input layer
    InputLayer : Input_Layer_Type;

    -- Hidden layer
    HiddenLayer : Layer_Type;

    -- Output Layer
    OutputLayer : Layer_Type;
  end record;

  function CreateModel(
    InputLayerSize, HiddenLayerSize, OutputLayerSize : Natural;
    RandomWeights : Boolean := True
  ) return Model_Type;

  procedure Perceptron(MNIST_Model : Model_Type);

  procedure WriteModelFile(FilePath : String; MNIST_Model : Model_Type);
  function ReadModelFile(
    FilePath : String;
    InputLayerSize, HiddenLayerSize, OutputLayerSize : Natural
  ) return Model_Type;

  procedure FreeModel(MNIST_Model : in out Model_Type);

end MNIST_Model;
