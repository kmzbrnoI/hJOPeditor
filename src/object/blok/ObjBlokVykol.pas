unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles, Global, symbolHelper, Graphics, PGraphics;

type

  TVykol = class(TGraphBlok)
    Pos: TPoint;
    symbol: Integer;
    obj: Integer; // index useku, na kterem je vykolejka
    vetev: Integer; // cislo vetve, ve kterem je vykolejka

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TVykol.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.vykol;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TVykol.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.symbol := ini.ReadInteger(key, 'T', 0);
  Self.obj := ini.ReadInteger(key, 'O', 0);
  Self.vetev := ini.ReadInteger(key, 'V', -1);
end;

procedure TVykol.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'T', Self.symbol);
  ini.WriteInteger(key, 'O', Self.obj);
  ini.WriteInteger(key, 'V', Self.vetev);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TVykol.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  if (selected) then
  begin
    color := colors.selected;
  end else begin
    case (Self.blok) of
      -1: color := colors.Alert;
      -2: color := colors.IntUnassigned;
    else
      color := colors.Normal;
    end;
  end;

  DrawObject.Canvas.Brush.color := clBlack;
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _Vykol_Start + Self.symbol, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
