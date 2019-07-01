unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles;

type

TNavestidlo = class(TGraphBlok)
 Position:TPoint;
 SymbolID:Integer;

 procedure Save(ini:TMemIniFile; key:string); override;
end;//Navestidlo


implementation

procedure TNavestidlo.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'S', Self.SymbolID);
end;

end.
