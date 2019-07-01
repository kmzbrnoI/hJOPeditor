unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles;

type

TVykol = class(TGraphBlok)
  Pos:TPoint;
  symbol:Integer;
  obj:integer;               // index useku, na kterem je vykolejka
  vetev:integer;             // cislo vetve, ve kterem je vykolejka

  procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TVykol.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'T', Self.symbol);
  ini.WriteInteger(key, 'O', Self.obj);
  ini.WriteInteger(key, 'V', Self.vetev);
end;

end.
