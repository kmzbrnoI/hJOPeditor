unit ObjBlokRozp;

interface

uses ObjBlok, IniFiles, Types;

type

TRozp = class(TGraphBlok)
  Pos:TPoint;

  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TRozp.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

end.
