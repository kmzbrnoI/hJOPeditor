unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles;

type

TNavestidlo = class(TGraphBlok)
 Position:TPoint;
 SymbolID:Integer;

 procedure Save(ini:TMemIniFile; key:string);
end;//Navestidlo


implementation

procedure TNavestidlo.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('N'+IntToStr(blok.Index), 'X', (blok as TNavestidlo).Position.X);
 inifile.WriteInteger('N'+IntToStr(blok.Index), 'Y', (blok as TNavestidlo).Position.Y);
 inifile.WriteInteger('N'+IntToStr(blok.Index), 'S', (blok as TNavestidlo).SymbolID);

 inifile.WriteInteger('N'+IntToStr(blok.Index), 'B', blok.Blok);
 inifile.WriteInteger('N'+IntToStr(blok.Index), 'OR',blok.OblRizeni);
end;

end.
