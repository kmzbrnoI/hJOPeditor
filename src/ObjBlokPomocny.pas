unit ObjBlokPomocny;

interface

uses ObjBlok, IniFiles, Generics.Collections, Types, SysUtils;

type

TPomocnyObj = class(TGraphBlok)
 Positions:TList<TPoint>;
 Symbol:Integer;

 destructor Destroy(); override;
 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

destructor TPomocnyObj.Destroy;
begin
 Self.Positions.Free();
end;

procedure TPomocnyObj.Save(ini: TMemIniFile; key: string);
var obj:string;
    point:TPoint;
begin
 ini.WriteInteger(key, 'S', Self.Symbol);
 obj := '';
 for point in Self.Positions do
   obj := obj + Format('%.3d%.3d',[point.X, point.Y]);
 ini.WriteString(key, 'P', obj);
end;

end.
