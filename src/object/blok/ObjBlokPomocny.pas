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
    procedure Move(d: TPoint); override;

    function GetEqOther(blocks: TList<TGraphBlok>): TObjOther;
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
var color: SymbolColor;
begin
  if (Self.Symbol in BLK_ASSIGN_SYMBOLS) then
  begin
    color := Self.StandardColor(colors, selected, mode);
  end else begin
    color := SymbolDrawColor(Self.Symbol);
    if (mode = dmAreas) then
      Exit(); // not painting in areas mode
  end;

  for var pos: TPoint in Self.Positions do
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, Self.Symbol, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TObjOther.Move(d: TPoint);
begin
  MoveList(Self.Positions, d);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TObjOther.GetEqOther(blocks: TList<TGraphBlok>): TObjOther;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.other) and (TObjOther(block).Positions.Count = 1) and (Self.Positions.Count = 1) and
        (TObjOther(block).Positions[0] = Self.Positions[0])) then
      Exit(TObjOther(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
