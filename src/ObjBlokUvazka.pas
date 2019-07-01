unit ObjBlokUvazka;

interface

uses ObjBlok, IniFiles, Types;

type

TUvazka = class(TGraphBlok)
  Pos:TPoint;
  defalt_dir:Integer;

  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TUvazka.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'D', Self.defalt_dir);
end;

end.
