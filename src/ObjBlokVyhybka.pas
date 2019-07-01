unit ObjBlokVyhybka;

interface

uses ObjBlok, Types, IniFiles;

type

TVyhybka = class(TGraphBlok)
 PolohaPlus:Byte;
 Position:TPoint;
 SymbolID:Integer;
 obj:integer;               //index useku, na kterem vyhybka je

 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TVyhybka.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'S', Self.SymbolID);
 ini.WriteInteger(key, 'P', Self.PolohaPlus);
 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'O', Self.obj);
end;

end.
