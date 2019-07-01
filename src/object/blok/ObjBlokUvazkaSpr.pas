unit ObjBlokUvazkaSpr;

interface

uses ObjBlok, Types, IniFiles;

type

TUvazkaSprVertDir = (top = 0, bottom = 1);

TUvazkaSpr = class(TGraphBlok)
  Pos:TPoint;
  vertical_dir:TUvazkaSprVertDir;
  spr_cnt:Integer;

  constructor Create(index:Integer);
  procedure Load(ini: TMemIniFile; key: string); override;
  procedure Save(ini: TMemIniFile; key: string); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TUvazkaSpr.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.uvazka_spr;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TUvazkaSpr.Load(ini: TMemIniFile; key: string);
begin
 inherited;

 Self.Pos.X := ini.ReadInteger(key, 'X', 0);
 Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
 Self.vertical_dir := TUvazkaSprVertDir(ini.ReadInteger(key, 'VD', 0));
 Self.spr_cnt := ini.ReadInteger(key, 'C', 1);
end;

procedure TUvazkaSpr.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'VD', Integer(Self.vertical_dir));
  ini.WriteInteger(key, 'C', Self.spr_cnt);
end;

////////////////////////////////////////////////////////////////////////////////

end.
