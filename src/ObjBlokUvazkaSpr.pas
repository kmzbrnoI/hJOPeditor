unit ObjBlokUvazkaSpr;

interface

uses ObjBlok, Types, IniFiles;

type

TUvazkaSprVertDir = (top = 0, bottom = 1);

//uvazka spr
TUvazkaSpr = class(TGraphBlok)
  Pos:TPoint;
  vertical_dir:TUvazkaSprVertDir;
  spr_cnt:Integer;
end;

implementation

procedure TUvazkaSpr.Save();
begin
       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'B', blok.Blok);
       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'X', (blok as TUvazkaSpr).Pos.X);
       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'Y', (blok as TUvazkaSpr).Pos.Y);
       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'VD', Integer((blok as TUvazkaSpr).vertical_dir));
       inifile.WriteInteger('UvS'+IntToStr(blok.Index), 'C', (blok as TUvazkaSpr).spr_cnt);

end;

end.
