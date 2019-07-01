unit ObjBlokPomocny;

interface

uses ObjBlok, IniFiles, Generics.Collections, Types;

type

TPomocnyObj = class(TGraphBlok)
 Positions:TList<TPoint>;
 Symbol:Integer;

 procedure Save(ini:TMemIniFile; key:string);
end;

implementation

procedure TPomocnyObj.Save(ini: TMemIniFile; key: string);
begin
 inifile.WriteInteger('P'+IntToStr(blok.Index),'S', (blok as TPomocnyObj).Symbol);
 obj := '';
 for j := 0 to (blok as TPomocnyObj).Positions.Count-1 do obj := obj + Format('%.3d%.3d',[(blok as TPomocnyObj).Positions[j].X, (blok as TPomocnyObj).Positions[j].Y]);
 inifile.WriteString('P'+IntToStr(blok.Index),'P', obj);

end;

end.
