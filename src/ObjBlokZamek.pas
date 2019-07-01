unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types;

type

TZamek = class(TGraphBlok)
  Pos:TPoint;

  procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TZamek.Save(ini: TMemIniFile; key: string);
begin
       inifile.WriteInteger('Z'+IntToStr(blok.Index), 'B',  blok.Blok);
       inifile.WriteInteger('Z'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

       inifile.WriteInteger('Z'+IntToStr(blok.Index), 'X', (blok as TZamek).Pos.X);
       inifile.WriteInteger('Z'+IntToStr(blok.Index), 'Y', (blok as TZamek).Pos.Y);

end;

end.
