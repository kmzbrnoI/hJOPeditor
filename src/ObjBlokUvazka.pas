unit ObjBlokUvazka;

interface

uses ObjBlok, IniFiles, Types;

type

TUvazka = class(TGraphBlok)
  Pos:TPoint;
  defalt_dir:Integer;

  procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TUvazka.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('Uv'+IntToStr(blok.Index), 'B',  blok.Blok);
 inifile.WriteInteger('Uv'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

 inifile.WriteInteger('Uv'+IntToStr(blok.Index), 'X', (blok as TUvazka).Pos.X);
 inifile.WriteInteger('Uv'+IntToStr(blok.Index), 'Y', (blok as TUvazka).Pos.Y);
 inifile.WriteInteger('Uv'+IntToStr(blok.Index), 'D', (blok as TUvazka).defalt_dir);
end;

end.
