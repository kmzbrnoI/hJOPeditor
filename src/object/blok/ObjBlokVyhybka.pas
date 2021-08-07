unit ObjBlokVyhybka;

interface

uses ObjBlok, Types, IniFiles, Global, symbolHelper, PGraphics;

type

  TVyhybka = class(TGraphBlok)
    PolohaPlus: Byte;
    Position: TPoint;
    SymbolID: Integer;
    obj: Integer; // index useku, na kterem vyhybka je

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TVyhybka.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.vyhybka;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TVyhybka.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.SymbolID := ini.ReadInteger(key, 'S', 0);
  Self.PolohaPlus := ini.ReadInteger(key, 'P', 0);
  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.obj := ini.ReadInteger(key, 'O', -1);
end;

procedure TVyhybka.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'S', Self.SymbolID);
  ini.WriteInteger(key, 'P', Self.PolohaPlus);
  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'O', Self.obj);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TVyhybka.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
begin
  if (selected) then
  begin
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, Self.SymbolID, colors.Selected);
  end else begin
    case (Self.Blok) of
      -1: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, Self.SymbolID, colors.Alert);
      -2: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, Self.SymbolID, colors.IntUnassigned);
    else
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, Self.SymbolID, colors.Normal);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
