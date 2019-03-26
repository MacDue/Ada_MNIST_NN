package Test is
  type Memes is (Dank, Undank);

  type MemeMap is array (Memes) of String(1..4);

  function GetMemeMap return MemeMap;
end Test;
