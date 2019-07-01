unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles;

type

TNavestidlo = class(TGraphBlok)
 Position:TPoint;
 SymbolID:Integer;

 constructor Create(index:Integer);
 procedure Load(ini:TMemIniFile; key:string); override;
 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TNavestidlo.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.navestidlo;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNavestidlo.Load(ini:TMemIniFile; key:string);
begin
 inherited;

 Self.Position.X := ini.ReadInteger(key, 'X', 0);
 Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
 Self.SymbolID   := ini.ReadInteger(key, 'S', 0);
end;

procedure TNavestidlo.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'S', Self.SymbolID);
end;

////////////////////////////////////////////////////////////////////////////////

end.
