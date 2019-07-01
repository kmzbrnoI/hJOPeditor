unit ObjBlokVyhybka;

interface

uses ObjBlok, Types, IniFiles;

type

TVyhybka = class(TGraphBlok)
 PolohaPlus:Byte;
 Position:TPoint;
 SymbolID:Integer;
 obj:integer;               //index useku, na kterem vyhybka je

 procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TVyhybka.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('V'+IntToStr(blok.Index),'B', blok.Blok);
 inifile.WriteInteger('V'+IntToStr(blok.Index),'OR',blok.OblRizeni);

 inifile.WriteInteger('V'+IntToStr(blok.Index),'S', (blok as TVyhybka).SymbolID);
 inifile.WriteInteger('V'+IntToStr(blok.Index),'P', (blok as TVyhybka).PolohaPlus);
 inifile.WriteInteger('V'+IntToStr(blok.Index),'X', (blok as TVyhybka).Position.X);
 inifile.WriteInteger('V'+IntToStr(blok.Index),'Y', (blok as TVyhybka).Position.Y);
 inifile.WriteInteger('V'+IntToStr(blok.Index),'O', (blok as TVyhybka).obj);
end;

end.
