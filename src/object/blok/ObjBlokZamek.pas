unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper;

type

  TLock = class(TGraphBlok)
    Pos: TPoint;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TLock.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.lock;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLock.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
end;

procedure TLock.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLock.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
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
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LOCK, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
