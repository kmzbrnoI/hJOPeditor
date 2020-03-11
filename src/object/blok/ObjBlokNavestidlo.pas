unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles, Global, SymbolHelper, PGraphics;

type

TNavestidlo = class(TGraphBlok)
 Position:TPoint;
 SymbolID:Integer;

 constructor Create(index:Integer);
 procedure Load(ini:TMemIniFile; key:string; version: Word); override;
 procedure Save(ini:TMemIniFile; key:string); override;
 procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TNavestidlo.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.navestidlo;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNavestidlo.Load(ini:TMemIniFile; key:string; version: Word);
begin
 inherited;

 Self.Position.X := ini.ReadInteger(key, 'X', 0);
 Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
 Self.SymbolID   := ini.ReadInteger(key, 'S', 0);
end;

procedure TNavestidlo.Save(ini: TMemIniFile; key: string);
begin
 inherited;

 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'S', Self.SymbolID);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNavestidlo.Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                            selected:boolean; mode:TMode);
begin
 if (selected) then
  begin
   DrawObject.SymbolIL.Draw(DrawObject.Canvas, Self.Position.X*_Symbol_Sirka, Self.Position.Y*_Symbol_Vyska,
                            ((_SCom_Start+Self.SymbolID)*10)+colors.Selected);
  end else begin
   case (Self.Blok) of
     -1: DrawObject.SymbolIL.Draw(DrawObject.Canvas, Self.Position.X*_Symbol_Sirka, Self.Position.Y*_Symbol_Vyska,
                                  ((_SCom_Start+Self.SymbolID)*10)+colors.Alert);
     -2: DrawObject.SymbolIL.Draw(DrawObject.Canvas, Self.Position.X*_Symbol_Sirka, Self.Position.Y*_Symbol_Vyska,
                                  ((_SCom_Start+Self.SymbolID)*10)+colors.IntUnassigned);
   else
     DrawObject.SymbolIL.Draw(DrawObject.Canvas, Self.Position.X*_Symbol_Sirka, Self.Position.Y*_Symbol_Vyska,
                              ((_SCom_Start+Self.SymbolID)*10)+colors.Normal);
   end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.
