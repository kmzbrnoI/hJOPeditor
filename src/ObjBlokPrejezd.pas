unit ObjBlokPrejezd;

interface

uses ObjBlok, Generics.Collections, Types, IniFiles;

type

TBlikPoint = record
 Pos:TPoint;
 TechUsek:Integer;     // jaky technologicky usek ma tato cast prejezdu
end;

TPrejezd = class(TGraphBlok)
 StaticPositions : TList<TPoint>;
 BlikPositions : TList<TBlikPoint>;

 procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TPrejezd.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('PRJ'+IntToStr(blok.Index), 'B',  blok.Blok);
 inifile.WriteInteger('PRJ'+IntToStr(blok.Index), 'OR', blok.OblRizeni);

 obj := '';
 for j := 0 to (blok as TPrejezd).BlikPositions.Count-1 do
  begin
   if ((blok as TPrejezd).BlikPositions[j].TechUsek >= 0) then
     obj := obj + Format('%.3d%.3d%.3d',[(blok as TPrejezd).BlikPositions[j].Pos.X, (blok as TPrejezd).BlikPositions[j].Pos.Y, (blok as TPrejezd).BlikPositions[j].TechUsek])
   else
     obj := obj + Format('%.3d%.3d-01',[(blok as TPrejezd).BlikPositions[j].Pos.X, (blok as TPrejezd).BlikPositions[j].Pos.Y]);
  end;
 inifile.WriteString('PRJ'+IntToStr(blok.Index), 'BP', obj);

 obj := '';
 for j := 0 to (blok as TPrejezd).StaticPositions.Count-1 do obj := obj + Format('%.3d%.3d',[(blok as TPrejezd).StaticPositions[j].X, (blok as TPrejezd).StaticPositions[j].Y]);
 inifile.WriteString('PRJ'+IntToStr(blok.Index), 'SP', obj);

end;

end.
