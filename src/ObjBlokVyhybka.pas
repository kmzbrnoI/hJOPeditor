unit ObjBlokVyhybka;

interface

uses ObjBlok, Types, IniFiles;

type

TVyhybka = class(TGraphBlok)
 PolohaPlus:Byte;
 Position:TPoint;
 SymbolID:Integer;
 obj:integer;               //index useku, na kterem vyhybka je

 constructor Create(index:Integer);
 procedure Load(ini:TMemIniFile; key:string); override;
 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TVyhybka.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.vyhybka;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TVyhybka.Load(ini: TMemIniFile; key: string);
begin
 inherited;

 Self.SymbolID := ini.ReadInteger(key, 'S', 0);
 Self.PolohaPlus := ini.ReadInteger(key, 'P', 0);
 Self.Position.X := ini.ReadInteger(key, 'X', 0);
 Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
 Self.obj := ini.ReadInteger(key, 'O', -1);
end;

procedure TVyhybka.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'S', Self.SymbolID);
 ini.WriteInteger(key, 'P', Self.PolohaPlus);
 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'O', Self.obj);
end;

////////////////////////////////////////////////////////////////////////////////

end.
