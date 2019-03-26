package NN_Utils is

  type Array_Of_Doubles is array (Natural range <>) of Long_Float with
    Default_Component_Value => 0.0;
  type Matrix_Of_Doubles is array (Natural range <>, Natural range <>) of Long_Float
    with Default_Component_Value => 0.0;

  type Layer_Inputs  is new Array_Of_Doubles;
  type Layer_Weights is new Matrix_Of_Doubles;
  type Layer_Outputs is new Array_Of_Doubles;

  type Layer_Weights_Delta  is new Matrix_Of_Doubles;
  type Layer_Backprop_Cache is new Array_Of_Doubles;

  type Layer_Inputs_Ptr is access Layer_Inputs;
  type Layer_Weights_Ptr is access Layer_Weights;
  type Layer_Outputs_Ptr is access Layer_Outputs;

  type Layer_Weights_Delta_Ptr is access Layer_Weights_Delta;
  type Layer_Backprop_Cache_Ptr is access Layer_Backprop_Cache;

  type Layer_Type is record
    Inputs  : Layer_Inputs_Ptr;
    Outputs : Layer_Outputs_Ptr;
    Weights : Layer_Weights_Ptr;
    BackpropCache : Layer_Backprop_Cache_Ptr;
    WeightDeltas  : Layer_Weights_Delta_Ptr;
  end record;

  type Input_Layer_Type is record
    Outputs : Layer_Outputs_Ptr;
  end record;

  function CreateInputLayer(Layer_Size : Natural) return Input_Layer_Type;
  function CreateLayer(Input_Size, Layer_Size: Natural) return Layer_Type;

  function Sigmoid(X : Long_Float) return Long_Float;

  function SquareError(Outputs : Layer_Outputs_Ptr; Expected : Array_Of_Doubles) return Long_Float;

end NN_Utils;
