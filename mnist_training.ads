with MNIST;
with NN_Utils; use NN_Utils;
with MNIST_Model; use MNIST_Model;

package MNIST_Training is

  procedure SetMaxEpochs(NewMaxEpochs : Positive);
  procedure SetLearningRate(NewLearningRate : Long_Float);
  procedure SetMonentum(NewMomentum : Long_Float);

  function TrainingStep(
    MNIST_Model : Model_Type;
    Sample : MNIST.Image_Ptr;
    Label : MNIST.Label
  ) return Positive;

end MNIST_Training;
