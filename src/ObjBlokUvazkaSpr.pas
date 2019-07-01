unit ObjBlokUvazkaSpr;

interface

uses ObjBlok, Types, IniFiles;

type

TUvazkaSprVertDir = (top = 0, bottom = 1);

//uvazka spr
TUvazkaSpr = class(TGraphBlok)
  Pos:TPoint;
  vertical_dir:TUvazkaSprVertDir;
  spr_cnt:Integer;

  procedure Save(ini: TMemIniFile; key: string); override;
end;

implementation

procedure TUvazkaSpr.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'VD', Integer(Self.vertical_dir));
  ini.WriteInteger(key, 'C', Self.spr_cnt);
end;

end.
