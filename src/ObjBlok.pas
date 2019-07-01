unit ObjBlok;

interface

type

TBlkType = (usek, navestidlo, vyhybka, prejezd, text, pomocny_obj, uvazka, uvazka_spr, zamek, vykol, rozp, TU);

TGraphBlok = class
  typ:TBlkType;

  Blok:Integer;
  OblRizeni:Integer;
  index:Integer;  // index bloku daneho typu v poli vsech bloku (tzn. kazdy typ ma svou indexaci - pocinaje 0)
end;

implementation

end.
