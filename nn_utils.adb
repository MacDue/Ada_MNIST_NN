with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;

package body NN_Utils is

  function Sigmoid(X : Long_Float) return Long_Float is
    package DoubleMath is new Ada.Numerics.Generic_Elementary_Functions(Long_Float);
    use DoubleMath;
  begin
    return 1.0 / (1.0 + Long_Float(e)**(-X));
  end;

  function SquareError(
    Outputs : Layer_Outputs_Ptr;
    Expected : Array_Of_Doubles
  ) return Long_Float is
    Result : Long_Float := 0.0;
  begin
    for I in Expected'Range loop
      Result := Result + (Outputs(I) - Expected(I))**2;
    end loop;
    return 0.5 * Result;
  end;

  function CreateInputLayer(Layer_Size : Natural) return Input_Layer_Type is
    Outputs : Layer_Outputs_Ptr := new Layer_Outputs(0 .. Layer_Size -1);
    Layer : Input_Layer_Type := (
      Outputs => Outputs
    );
  begin
    return Layer;
  end;

  function CreateLayer(Input_Size, Layer_Size: Natural) return Layer_Type is
    Inputs  : Layer_Inputs_Ptr := new Layer_Inputs(0 .. Layer_Size -1);
    Outputs : Layer_Outputs_Ptr := new Layer_Outputs(0 .. Layer_Size -1);
    Weights : Layer_Weights_Ptr := new Layer_Weights(0 .. Input_Size -1, 0 .. Layer_Size -1);
    BackpropCache : Layer_Backprop_Cache_Ptr := new Layer_Backprop_Cache(0 .. Layer_Size -1);
    WeightDeltas  : Layer_Weights_Delta_Ptr := new Layer_Weights_Delta(0 .. Input_Size -1, 0 .. Layer_Size -1);
    Layer : Layer_Type := (
      Inputs,
      Outputs,
      Weights,
      BackpropCache,
      WeightDeltas
    );
  begin
    return Layer;
  end;

end NN_Utils;
