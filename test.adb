package body Test is

  MyMemeMap : constant MemeMap := (Dank => "DaNK", Undank => "Donk");

  function GetMemeMap return MemeMap is
  begin
    return MyMemeMap;
  end;
end Test;
