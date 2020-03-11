unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper;

type

TZamek = class(TGraphBlok)
  Pos:TPoint;

  constructor Create(index:Integer);
  procedure Load(ini:TMemIniFile; key:string; version: Word); override;
  procedure Save(ini:TMemIniFile; key:string); override;
  procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TZamek.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.zamek;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TZamek.Load(ini:TMemIniFile; key:string; version: Word);
begin
 inherited;

 Self.Pos.X := ini.ReadInteger(key, 'X', 0);
 Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
end;

procedure TZamek.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Pos.X);
 ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TZamek.Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                       selected:boolean; mode:TMode);
var color:TColor;
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

 DrawObject.Canvas.Brush.Color := clBlack;
 DrawObject.SymbolIL.Draw(DrawObject.Canvas, Self.Pos.X*_Symbol_Sirka, Self.Pos.Y*_Symbol_Vyska, (_Zamek*10)+color);
end;

////////////////////////////////////////////////////////////////////////////////

end.
