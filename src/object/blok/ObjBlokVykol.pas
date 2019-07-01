unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles;

type

TVykol = class(TGraphBlok)
  Pos:TPoint;
  symbol:Integer;
  obj:integer;               // index useku, na kterem je vykolejka
  vetev:integer;             // cislo vetve, ve kterem je vykolejka

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string); override;
  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TVykol.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.vykol;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TVykol.Load(ini: TMemIniFile; key: string);
begin
 inherited;

 Self.Pos.X := ini.ReadInteger(key, 'X', 0);
 Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
 Self.symbol := ini.ReadInteger(key, 'T', 0);
 Self.obj := ini.ReadInteger(key, 'O', 0);
 Self.vetev := ini.ReadInteger(key, 'V', -1);
end;

procedure TVykol.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Pos.X);
 ini.WriteInteger(key, 'Y', Self.Pos.Y);
 ini.WriteInteger(key, 'T', Self.symbol);
 ini.WriteInteger(key, 'O', Self.obj);
 ini.WriteInteger(key, 'V', Self.vetev);
end;

////////////////////////////////////////////////////////////////////////////////

end.
