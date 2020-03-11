unit ObjBlokPrejezd;

interface

uses ObjBlok, Generics.Collections, Types, IniFiles, SysUtils, Global, Graphics,
     symbolHelper, PGraphics;

type

TBlikPoint = record
 Pos:TPoint;
 TechUsek:Integer;     // jaky technologicky usek ma tato cast prejezdu
end;

TPrejezd = class(TGraphBlok)
 StaticPositions : TList<TPoint>;
 BlikPositions : TList<TBlikPoint>;

 constructor Create(index:Integer);
 destructor Destroy(); override;
 procedure Load(ini:TMemIniFile; key:string; version: Word); override;
 procedure Save(ini:TMemIniFile; key:string); override;
 procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); override;
end;

implementation

uses ReliefObjects;

////////////////////////////////////////////////////////////////////////////////

constructor TPrejezd.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.prejezd;
 Self.BlikPositions := TList<TBlikPoint>.Create();
 Self.StaticPositions := TList<TPoint>.Create();
end;

destructor TPrejezd.Destroy;
begin
 Self.StaticPositions.Free();
 Self.BlikPositions.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPrejezd.Load(ini:TMemIniFile; key:string; version: Word);
var obj:string;
    pos:TPoint;
    j:Integer;
    blik_pos:TBlikPoint;
    count, section_len: Integer;
begin
 inherited;

 if (version >= _FILEVERSION_13) then
   section_len := 16
 else
   section_len := 9;

 obj := ini.ReadString(key, 'BP', '');
 Self.BlikPositions.Clear();

 count := (Length(obj) div section_len);
 for j := 0 to count-1 do
  begin
   try
     blik_pos.Pos.X    := StrToInt(copy(obj, j*section_len+1, 3));
     blik_pos.Pos.Y    := StrToInt(copy(obj, j*section_len+4, 3));
     blik_pos.TechUsek := StrToInt(copy(obj, j*section_len+7, 3));
   except
     continue;
   end;

   Self.BlikPositions.Add(blik_pos);
  end;

 obj := ini.ReadString(key, 'SP', '');
 Self.StaticPositions.Clear();
 for j := 0 to (Length(obj) div 6)-1 do
  begin
   try
     pos.X := StrToInt(copy(obj, j*6+1, 3));
     pos.Y := StrToInt(copy(obj, j*6+4, 3));
   except
     continue;
   end;
   Self.StaticPositions.Add(pos);
  end;
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
     obj := obj + Format('%.3d%.3d%.10d',[bp.Pos.X, bp.Pos.Y, bp.TechUsek])
   else
     obj := obj + Format('%.3d%.3d-000000001',[bp.Pos.X, bp.Pos.Y]);
  end;
 ini.WriteString(key, 'BP', obj);

 obj := '';
 for point in Self.StaticPositions do
   obj := obj + Format('%.3d%.3d',[point.X, point.Y]);
 ini.WriteString(key, 'SP', obj);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPrejezd.Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                         selected:boolean; mode:TMode);
var color:TColor;
    pos:TPoint;
    bp:TBlikPoint;
begin
 if (selected) then
  begin
   color := colors.Selected;
  end else begin
   case (Self.blok) of
    -1: color := colors.Alert;
    -2: color := colors.IntUnassigned;
   else
     color := colors.Normal;
   end;
  end;//else (Self.Selected > 255)

 for pos in Self.StaticPositions do
   DrawObject.SymbolIL.Draw(DrawObject.Canvas, pos.X*_Symbol_Sirka, pos.Y*_Symbol_Vyska, _Prj_Index+color);

 for bp in Self.BlikPositions do
   DrawObject.SymbolIL.Draw(DrawObject.Canvas, bp.Pos.X*_Symbol_Sirka, bp.Pos.Y*_Symbol_Vyska, _Prj_Index+color);
end;

////////////////////////////////////////////////////////////////////////////////

end.
