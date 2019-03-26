with MNIST;
with NN_Utils; use NN_Utils;
with MNIST_Model; use MNIST_Model;

package MNIST_Training is

  procedure SetEpochs(NewEpochs : Natural);
  procedure SetLearningRate(NewLearningRate : Long_Float);
  procedure SetMonentum(NewMomentum : Long_Float);

  procedure TrainingStep(
    MNIST_Model : Model_Type;
    Sample : MNIST.Image_Ptr;
    Label : MNIST.Label);

end MNIST_Training;
