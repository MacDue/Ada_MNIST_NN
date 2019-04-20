with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Interfaces.C;
with Interfaces; use Interfaces;

with SDL.Hints;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

with SDL.Video.Renderers; use SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

with SDL.Video.Rectangles; use SDL.Video.Rectangles;

with SDL.Video.Palettes; use SDL.Video.Palettes;

with SDL.Events.Events;

with MNIST;
with MNIST_Model; use MNIST_Model;

-- Horrible shitty WIP one file NN renderer for Ada learning

procedure Visualise is
  Width : constant := 1000;
  Height : constant := 900;
  Title : constant String := "Ada NN Visualisation";

  Window : SDL.Video.Windows.Window;
  Renderer : SDL.Video.Renderers.Renderer;

  G_MNIST_Model : Model_Type;

  function LoadMnistModel(InputFilePath : String) return Model_Type is
    MNIST_Model : constant Model_Type := ReadModelFile(
      FilePath => InputFilePath,
      InputLayerSize => 28 * 28, -- 784 pixels
      HiddenLayerSize => 128,
      OutputLayerSize => 10
    );
  begin
    return MNIST_Model;
  end;

  procedure DrawMnistNN(MNIST_Model : Model_Type) is
    -- DrawInput
    -- DrawNode (show what's in the node too (pixels/number))
    -- DrawLayer
    -- DrawWeights
    ClearColour : constant Colour := (0, 0, 0, 0);
    use Interfaces.C;
  begin
    Clear(Renderer);

    Set_Draw_Colour(Renderer, (0, 0, 255, 55));
    for Pixel in MNIST_Model.InputLayer.Outputs'Range loop
      declare
        X : constant int := int(10 + 2 * (Pixel mod 2));
        Y : constant int := int((Height-MNIST_Model.InputLayer.Outputs'Length)/2 + Pixel);
      begin
        for HiddenNode in MNIST_Model.HiddenLayer.Inputs'Range loop
          Draw(Renderer, Line => ((X+3,Y), (int(100 - 10 * (HiddenNode mod 2)), int((Height - MNIST_Model.HiddenLayer.Outputs'Length*7)/2 + HiddenNode * 7)+3)));
        end loop;
      end;
    end loop;

    for HiddenNode in MNIST_Model.HiddenLayer.Inputs'Range loop
      declare
        X : constant int := int(100 - 10 * (HiddenNode mod 2));
        Y : constant int := int((Height - MNIST_Model.HiddenLayer.Outputs'Length*7)/2 + HiddenNode * 7);
      begin
        for OutputNode in MNIST_Model.OutputLayer.Inputs'Range loop
          Draw(Renderer, Line => ((X+7,Y+3), (400, int((Height - MNIST_Model.OutputLayer.Outputs'Length*70)/2 + 70 * OutputNode)+30)));
        end loop;
      end;
    end loop;
    Set_Draw_Colour(Renderer, (255, 0, 0, 255));

    for Pixel in MNIST_Model.InputLayer.Outputs'Range loop
      declare
        X : constant int := int(10 + 2 * (Pixel mod 2));
        Y : constant int := int((Height-MNIST_Model.InputLayer.Outputs'Length)/2 + Pixel);
      begin
        Fill(Renderer, Rectangle => (X, Y, 1, 1));
      end;
    end loop;

    for HiddenNode in MNIST_Model.HiddenLayer.Inputs'Range loop
      declare
        X : constant int := int(100 - 10 * (HiddenNode mod 2));
        Y : constant int := int((Height - MNIST_Model.HiddenLayer.Outputs'Length*7)/2 + HiddenNode * 7);
      begin
        Fill(Renderer, Rectangle => (X, Y, 7, 7));
      end;
    end loop;

    for OutputNode in MNIST_Model.OutputLayer.Inputs'Range loop
      Fill(Renderer, Rectangle => (
        400,  int((Height - MNIST_Model.OutputLayer.Outputs'Length*70)/2 + 70 * OutputNode), 60, 60
      ));
    end loop;

    Present(Renderer);
    Set_Draw_Colour(Renderer, ClearColour);
  end;

  procedure MainLoop is
    Event : SDL.Events.Events.Events;
    ExitLoop : Boolean := False;
    use type SDL.Events.Event_Types;
  begin
    loop
      while SDL.Events.Events.Poll (Event) loop
        case Event.Common.Event_Type is
          when SDL.Events.Quit =>
            ExitLoop := True;
          when others =>
            null;
        end case;
      end loop;
      exit when ExitLoop;
      DrawMnistNN(G_MNIST_Model);
    end loop;
  end;

begin
  if Argument_Count /= 1 then
    Put_Line("Provide a file path to the model to load");
    Set_Exit_Status(Failure);
    return;
  end if;
  G_MNIST_Model := LoadMnistModel(Argument(1));

  -- SDL.Hints.Set(SDL.Hints.Render_Scale_Quality, "1");
  SDL.Video.Windows.Makers.Create(
    Win => Window,
    Title => Title,
    Position => (0, 0),
    Size => (Width, Height)
  );
  SDL.Video.Renderers.Makers.Create(Renderer, Window);

  MainLoop;
  Window.Finalize;
  SDL.Finalise;
end;
