unit ObjBlokText;

interface

uses ObjBlok, IniFiles;

type

TText = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;

 procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TText.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteString('T'+IntToStr(blok.Index),  'T', (blok as TText).Text);
 inifile.WriteInteger('T'+IntToStr(blok.Index), 'X', (blok as TText).Position.X);
 inifile.WriteInteger('T'+IntToStr(blok.Index), 'Y', (blok as TText).Position.Y);
 inifile.WriteInteger('T'+IntToStr(blok.Index), 'C', (blok as TText).Color);

 if (Length((blok as TText).Text) = 1) then
  begin
   inifile.WriteInteger('T'+IntToStr(blok.Index), 'B', blok.Blok);
   inifile.WriteInteger('T'+IntToStr(blok.Index), 'OR', blok.OblRizeni);
  end;
end;

end.
