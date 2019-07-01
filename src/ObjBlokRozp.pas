unit ObjBlokRozp;

interface

uses ObjBlok, IniFiles, Types;

type

TRozp = class(TGraphBlok)
  Pos:TPoint;

  procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TRozp.Save(ini: TMemIniFile; key: string);
begin
       inifile.WriteInteger('R'+IntToStr(blok.Index), 'B',  blok.Blok);
       inifile.WriteInteger('R'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

       inifile.WriteInteger('R'+IntToStr(blok.Index), 'X', (blok as TRozp).Pos.X);
       inifile.WriteInteger('R'+IntToStr(blok.Index), 'Y', (blok as TRozp).Pos.Y);

end;

end.
