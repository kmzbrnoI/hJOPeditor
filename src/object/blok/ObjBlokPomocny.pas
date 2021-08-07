unit ObjBlokPomocny;

interface

uses ObjBlok, IniFiles, Generics.Collections, Types, SysUtils, Global, PGraphics,
  symbolHelper;

const BLK_ASSIGN_SYMBOLS = [_S_CIRCLE];

type

  TObjOther = class(TGraphBlok)
    Positions: TList<TPoint>;
    Symbol: Integer;

    constructor Create(index: Integer);
    destructor Destroy(); override;
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

uses ReliefObjects;

/// /////////////////////////////////////////////////////////////////////////////

constructor TObjOther.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.other;
  Self.Positions := TList<TPoint>.Create();
end;

destructor TObjOther.Destroy;
begin
  Self.Positions.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TObjOther.Load(ini: TMemIniFile; key: string; version: Word);
var obj: string;
  pos: TPoint;
  j: Integer;
begin
  inherited;

  Self.Symbol := ini.ReadInteger(key, 'S', 0);
  if (version < _FILEVERSION_20) then
    Self.Symbol := TranscodeSymbolFromBpnlV3(Self.Symbol);

  obj := ini.ReadString(key, 'P', '');
  Self.Positions.Clear();
  for j := 0 to (Length(obj) div 6) - 1 do
  begin
    pos.X := StrToIntDef(copy(obj, j * 6 + 1, 3), 0);
    pos.Y := StrToIntDef(copy(obj, j * 6 + 4, 3), 0);
    Self.Positions.Add(pos);
  end;
end;

procedure TObjOther.Save(ini: TMemIniFile; key: string);
var obj: string;
  point: TPoint;
begin
  if (Self.Symbol in BLK_ASSIGN_SYMBOLS) then
    inherited;

  ini.WriteInteger(key, 'S', Self.Symbol);
  obj := '';
  for point in Self.Positions do
    obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
  ini.WriteString(key, 'P', obj);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TObjOther.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors;
  selected: boolean; mode: TMode);
var pos: TPoint;
    color: SymbolColor;
begin
  if (Self.Symbol in BLK_ASSIGN_SYMBOLS) then
  begin
    if (selected) then
    begin
      color := colors.Selected;
    end else begin
      case (Self.block) of
        - 1:
          color := colors.Alert;
        -2:
          color := colors.IntUnassigned;
      else
        color := colors.Normal;
      end;
    end;
  end else begin
    color := SymbolDrawColor(Self.Symbol);
  end;

  for pos in Self.Positions do
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, Self.Symbol, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
