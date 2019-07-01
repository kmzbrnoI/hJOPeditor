unit ObjBlokPrejezd;

interface

uses ObjBlok, Generics.Collections, Types, IniFiles, SysUtils;

type

TBlikPoint = record
 Pos:TPoint;
 TechUsek:Integer;     // jaky technologicky usek ma tato cast prejezdu
end;

TPrejezd = class(TGraphBlok)
 StaticPositions : TList<TPoint>;
 BlikPositions : TList<TBlikPoint>;

 destructor Destroy(); override;
 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

destructor TPrejezd.Destroy;
begin
 Self.StaticPositions.Free();
 Self.BlikPositions.Free();
end;

procedure TPrejezd.Save(ini: TMemIniFile; key: string);
var obj:string;
    bp:TBlikPoint;
    point:TPoint;
begin
 inherited;

 obj := '';
 for bp in Self.BlikPositions do
  begin
   if (bp.TechUsek >= 0) then
     obj := obj + Format('%.3d%.3d%.3d',[bp.Pos.X, bp.Pos.Y, bp.TechUsek])
   else
     obj := obj + Format('%.3d%.3d-01',[bp.Pos.X, bp.Pos.Y]);
  end;
 ini.WriteString(key, 'BP', obj);

 obj := '';
 for point in Self.StaticPositions do
   obj := obj + Format('%.3d%.3d',[point.X, point.Y]);
 ini.WriteString(key, 'SP', obj);
end;

end.
