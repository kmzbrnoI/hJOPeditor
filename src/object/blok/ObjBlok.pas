unit ObjBlok;

interface

uses IniFiles, Global, PGraphics;

type

TBlkType = (usek, navestidlo, vyhybka, prejezd, text, pomocny_obj, uvazka,
            uvazka_spr, zamek, vykol, rozp, TU);

TGraphBlok = class
  typ:TBlkType;

  Blok:Integer;
  OblRizeni:Integer;
  index:Integer;  // index bloku daneho typu v poli vsech bloku (tzn. kazdy typ ma svou indexaci - pocinaje 0)

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string); virtual;
  procedure Save(ini:TMemIniFile; key:string); virtual;
  procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); virtual; abstract;

  class function TypeToFileStr(typ:TBlkType):string;
end;

implementation

constructor TGraphBlok.Create(index:Integer);
begin
 inherited Create();
 Self.index := index;
 Self.Blok := -1;
 Self.OblRizeni := 0;
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

class function TGraphBlok.TypeToFileStr(typ:TBlkType):string;
begin
 case (typ) of
  TBlkType.usek: Result := 'U';
  TBlkType.navestidlo: Result := 'N';
  TBlkType.vyhybka: Result := 'V';
  TBlkType.prejezd: Result := 'PRJ';
  TBlkType.text: Result := 'T';
  TBlkType.pomocny_obj: Result := 'P';
  TBlkType.uvazka: Result := 'Uv';
  TBlkType.uvazka_spr: Result := 'UvS';
  TBlkType.zamek: Result := 'Z';
  TBlkType.vykol: Result := 'Vyk';
  TBlkType.rozp: Result := 'R';
 else
  Result := '?';
 end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
