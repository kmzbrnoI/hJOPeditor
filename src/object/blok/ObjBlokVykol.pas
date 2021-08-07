unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles, Global, symbolHelper, Graphics, PGraphics;

type

  TDerail = class(TGraphBlok)
    Pos: TPoint;
    symbol: Integer;
    obj: Integer; // index useku, na kterem je vykolejka
    branch: Integer; // cislo vetve, ve kterem je vykolejka

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TDerail.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.derail;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDerail.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.symbol := ini.ReadInteger(key, 'T', 0);
  Self.obj := ini.ReadInteger(key, 'O', 0);
  Self.branch := ini.ReadInteger(key, 'V', -1);
end;

procedure TDerail.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'T', Self.symbol);
  ini.WriteInteger(key, 'O', Self.obj);
  ini.WriteInteger(key, 'V', Self.branch);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDerail.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  if (selected) then
  begin
    color := colors.selected;
  end else begin
    case (Self.block) of
      -1: color := colors.Alert;
      -2: color := colors.IntUnassigned;
    else
      color := colors.Normal;
    end;
  end;

  DrawObject.Canvas.Brush.color := clBlack;
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_DERAIL_B + Self.symbol, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
