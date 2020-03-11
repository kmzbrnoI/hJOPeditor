unit ObjBlokPomocny;

interface

uses ObjBlok, IniFiles, Generics.Collections, Types, SysUtils, Global, PGraphics,
     symbolHelper;

type

TPomocnyObj = class(TGraphBlok)
 Positions:TList<TPoint>;
 Symbol:Integer;

 constructor Create(index:Integer);
 destructor Destroy(); override;
 procedure Load(ini:TMemIniFile; key:string; version: Word); override;
 procedure Save(ini:TMemIniFile; key:string); override;
 procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TPomocnyObj.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.pomocny_obj;
 Self.Positions := TList<TPoint>.Create();
end;

destructor TPomocnyObj.Destroy;
begin
 Self.Positions.Free();
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TPomocnyObj.Load(ini:TMemIniFile; key:string; version: Word);
var obj:string;
    pos:TPoint;
    j:Integer;
begin
 // No inherited
 Self.Blok      := -1;
 Self.OblRizeni := -1;

 Self.Symbol :=  ini.ReadInteger(key, 'S', 0);

 obj := ini.ReadString(key, 'P', '');
 Self.Positions.Clear();
 for j := 0 to (Length(obj) div 6)-1 do
  begin
   pos.X := StrToIntDef(copy(obj, j*6+1, 3), 0);
   pos.Y := StrToIntDef(copy(obj, j*6+4, 3), 0);
   Self.Positions.Add(pos);
  end;
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

////////////////////////////////////////////////////////////////////////////////

procedure TPomocnyObj.Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                            selected:boolean; mode:TMode);
var pos:TPoint;
begin
 for pos in Self.Positions do
   DrawObject.SymbolIL.Draw(DrawObject.Canvas,
                            pos.X*_Symbol_Sirka,
                            pos.Y*_Symbol_Vyska,
                            (Self.Symbol*10)+_Bitmap_DrawColors[Self.Symbol]);

end;

////////////////////////////////////////////////////////////////////////////////

end.
