unit ObjBlokUvazka;

interface

uses ObjBlok, IniFiles, Types;

type

TUvazka = class(TGraphBlok)
  Pos:TPoint;
  defalt_dir:Integer;

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string); override;
  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TUvazka.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.uvazka;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUvazka.Load(ini: TMemIniFile; key: string);
begin
 inherited;

 Self.Pos.X := ini.ReadInteger(key, 'X', 0);
 Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
 Self.defalt_dir := ini.ReadInteger(key, 'D', 0);
end;

procedure TUvazka.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'D', Self.defalt_dir);
end;

////////////////////////////////////////////////////////////////////////////////

end.
