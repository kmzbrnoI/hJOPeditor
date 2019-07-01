unit ObjBlok;

interface

uses IniFiles;

type

TBlkType = (usek, navestidlo, vyhybka, prejezd, text, pomocny_obj, uvazka, uvazka_spr, zamek, vykol, rozp, TU);

TGraphBlok = class
  typ:TBlkType;

  Blok:Integer;
  OblRizeni:Integer;
  index:Integer;  // index bloku daneho typu v poli vsech bloku (tzn. kazdy typ ma svou indexaci - pocinaje 0)

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string); virtual;
  procedure Save(ini:TMemIniFile; key:string); virtual;
end;

implementation

constructor TGraphBlok.Create(index:Integer);
begin
 inherited Create();
 Self.index := index;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TGraphBlok.Load(ini:TMemIniFile; key:string);
begin
 Self.Blok := ini.ReadInteger(key, 'B', -1);
 Self.OblRizeni := ini.ReadInteger(key, 'OR', -1);
end;

procedure TGraphBlok.Save(ini: TMemIniFile; key: string);
begin
 ini.WriteInteger(key, 'B', Self.Blok);
 ini.WriteInteger(key, 'OR', Self.OblRizeni);
end;

////////////////////////////////////////////////////////////////////////////////

end.
