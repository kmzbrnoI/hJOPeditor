unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles;

type

TVykol = class(TGraphBlok)
  Pos:TPoint;
  symbol:Integer;
  obj:integer;               // index useku, na kterem je vykolejka
  vetev:integer;             // cislo vetve, ve kterem je vykolejka

  procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TVykol.Save(ini: TMemIniFile; key: string);
begin
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'B',  blok.Blok);
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'X', (blok as TVykol).Pos.X);
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'Y', (blok as TVykol).Pos.Y);
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'T', (blok as TVykol).symbol);
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'O', (blok as TVykol).obj);
       inifile.WriteInteger('Vyk'+IntToStr(blok.Index), 'V', (blok as TVykol).vetev);
end;

end.
