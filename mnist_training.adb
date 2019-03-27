with Ada.Text_IO; use Ada.Text_IO;

with MNIST;
with NN_Utils; use NN_Utils;
with Interfaces; use Interfaces;

package body MNIST_Training is

  MaxEpochs : Positive := 512;
  LearningRate : Long_Float :=  1.0E-3;
  Momentum : Long_Float :=  0.9;

  procedure SetMaxEpochs(NewMaxEpochs : Positive) is
  begin
    MaxEpochs := NewMaxEpochs;
  end;

  procedure SetLearningRate(NewLearningRate : Long_Float) is
  begin
    LearningRate := NewLearningRate;
  end;

  procedure SetMonentum(NewMomentum : Long_Float) is
  begin
    Momentum := NewMomentum;
  end;

  -- InputLayer => CreateInputLayer(InputLayerSize),
  --
  -- HiddenLayer => CreateLayer(InputLayerSize, HiddenLayerSize),
  --
  -- OutputLayer => CreateLayer(HiddenLayerSize, OutputLayerSize)

  procedure BackPropagation(MNIST_Model : Model_Type; ExpectedOutputs : Array_Of_Doubles) is
  begin
    -- vvv scary
    -- This is back propagation with momentum
    -- References (for my attempt to understand this in any way)
    -- https://github.com/HyTruongSon/Neural-Network-MNIST-CPP/blob/master/training_nn.cpp#L219
    -- https://blogs.msdn.microsoft.com/uk_faculty_connection/2017/07/04/how-to-implement-the-backpropagation-using-python-and-numpy/
    -- https://www.youtube.com/watch?v=Ilg3gGewQ5U
    -- https://www.willamette.edu/~gorr/classes/cs449/momrate.html
    -- https://media.giphy.com/media/xT0xeJpnrWC4XWblEk/giphy.gif

    for NodeIndex in MNIST_Model.OutputLayer.Outputs'Range loop
      declare
        OutputValue : Long_Float := MNIST_Model.OutputLayer.Outputs(NodeIndex);
        ExpectedValue : Long_Float := ExpectedOutputs(NodeIndex);
      begin
        -- Add in input value?
        -- I think (hope) this calculates the gradient with respect to the error
        MNIST_Model.OutputLayer.BackpropCache(NodeIndex)
          := OutputValue * (1.0 - OutputValue) * (ExpectedValue - OutputValue);
      end;
    end loop;

    -- Propagate back into the hidden layer
    for HiddenNodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      declare
        HiddenNodeNudge : Long_Float := 0.0;
        HiddenNodeOutput : Long_Float := MNIST_Model.HiddenLayer.Outputs(HiddenNodeIndex);
      begin
        for OutputNodeIndex in MNIST_Model.OutputLayer.Outputs'Range loop
          -- My understanding:
          -- This is combining all the nudges relevent nudges for this
          -- hidden node from the output layer using the weights
          HiddenNodeNudge
            := HiddenNodeNudge
               + (MNIST_Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex)
                  * MNIST_Model.OutputLayer.BackpropCache(OutputNodeIndex));
        end loop;
        -- Calculate gradient
        MNIST_Model.HiddenLayer.BackpropCache(HiddenNodeIndex)
          := HiddenNodeOutput * (1.0 - HiddenNodeOutput) * HiddenNodeNudge;
      end;
    end loop;

    -- Update output layer weights with momentum
    for HiddenNodeIndex in MNIST_Model.HiddenLayer.Outputs'Range loop
      for OutputNodeIndex in MNIST_Model.OutputLayer.Outputs'Range loop
        declare
          NewOutputLayerWeightsDelta : Long_Float := 0.0;
        begin
          -- Apply the previously (just above this code) calculateded gradient (backprop cache)
          -- And use add in part of the previous (last backprop) gradient with momentum
          NewOutputLayerWeightsDelta
            -- Mathly expressed: https://visualstudiomagazine.com/Articles/2017/06/01/~/media/ECG/visualstudiomagazine/Images/2017/06/0617vsm_McCaffreyFig2s.ashx
            := (LearningRate
                * MNIST_Model.OutputLayer.BackpropCache(OutputNodeIndex) -- (oj - tj) * oj * (1 - oj)
                * MNIST_Model.HiddenLayer.Outputs(HiddenNodeIndex))      -- xi (the input)
               + (Momentum
                  * MNIST_Model.OutputLayer.WeightDeltas(HiddenNodeIndex, OutputNodeIndex));

          MNIST_Model.OutputLayer.WeightDeltas(HiddenNodeIndex, OutputNodeIndex) := NewOutputLayerWeightsDelta;
          -- Weight updated!
          MNIST_Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex)
            := MNIST_Model.OutputLayer.Weights(HiddenNodeIndex, OutputNodeIndex) + NewOutputLayerWeightsDelta;
        end;
      end loop;
    end loop;

    -- Much the same for the hidden later
    for PixelIndex in MNIST_Model.InputLayer.Outputs'Range loop
      for HiddenNodeIndex in MNIST_Model.OutputLayer.Outputs'Range loop
        declare
          NewHiddenLayerWeightsDelta : Long_Float := 0.0;
        begin
          NewHiddenLayerWeightsDelta
            := (LearningRate
                * MNIST_Model.HiddenLayer.BackpropCache(HiddenNodeIndex)
                * MNIST_Model.InputLayer.Outputs(PixelIndex))
               + (Momentum
                  * MNIST_Model.HiddenLayer.WeightDeltas(PixelIndex, HiddenNodeIndex));

          MNIST_Model.HiddenLayer.WeightDeltas(PixelIndex, HiddenNodeIndex) := NewHiddenLayerWeightsDelta;
          MNIST_Model.HiddenLayer.Weights(PixelIndex, HiddenNodeIndex)
            := MNIST_Model.HiddenLayer.Weights(PixelIndex, HiddenNodeIndex) + NewHiddenLayerWeightsDelta;
        end;
      end loop;
    end loop;
    -- Finished!
  end;

  function TrainingStep(
    MNIST_Model : Model_Type;
    Sample : MNIST.Image_Ptr;
    Label : MNIST.Label
  ) return Positive is
    ExpectedOutputs : Array_Of_Doubles (0 .. MNIST_Model.OutputLayer.Outputs'Last);
    -- Can't do := (Natural(Label) => 1.0, others => 0), reason unknown
    Epsilon : constant Long_Float := 1.0E-3;
  begin
    -- Set expected
    ExpectedOutputs(Natural(Label)) := 1.0;

    -- Set the input layer
    for Y in Sample'Range(2) loop
      for X in Sample'Range(1) loop
        MNIST_Model.InputLayer.Outputs(
          X + (Y * Sample'Length(1))
        ) := (if Sample(X, Y) = 0 then 0.0 else 1.0);
      end loop;
    end loop;

    -- Train
    declare
      Error : Long_Float := 0.0;
      EpochsTaken : Positive := MaxEpochs;
    begin
      for Epoch in 1 .. MaxEpochs loop
        Perceptron(MNIST_Model);
        BackPropagation(MNIST_Model, ExpectedOutputs);

        Error := SquareError(MNIST_Model.OutputLayer.Outputs, ExpectedOutputs);

        if Error < Epsilon then
          EpochsTaken := Epoch;
          exit;
        end if;
      end loop;

      Put_Line("Error:" & Long_Float'Image(Error));
      return EpochsTaken;
    end;
  end;

end MNIST_Training;
