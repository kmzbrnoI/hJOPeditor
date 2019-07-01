unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types;

type

TZamek = class(TGraphBlok)
  Pos:TPoint;

  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TZamek.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

end.
