unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types;

type

TZamek = class(TGraphBlok)
  Pos:TPoint;

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string); override;
  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TZamek.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.zamek;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TZamek.Load(ini:TMemIniFile; key:string);
begin
 inherited;

 Self.Pos.X := ini.ReadInteger(key, 'X', 0);
 Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
end;

procedure TZamek.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Pos.X);
 ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

////////////////////////////////////////////////////////////////////////////////

end.
