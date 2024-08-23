unit ObjBlokPrejezd;

interface

uses ObjBlok, Generics.Collections, Types, IniFiles, SysUtils, Global, Graphics,
  symbolHelper, PGraphics;

type

  TBlikPoint = record
    Pos: TPoint;
    PanelUsek: Integer; // jaky usek panelu (v ciselne radu useku) ma tato cast prejezdu
  end;

  TCrossing = class(TGraphBlok)
    StaticPositions: TList<TPoint>;
    BlikPositions: TList<TBlikPoint>;

    constructor Create(index: Integer);
    destructor Destroy(); override;
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;

    class function AreBlikPointListsEq(a: TList<TBlikPoint>; b: TList<TBlikPoint>): Boolean;
    function GetEqCrossing(blocks: TList<TGraphBlok>): TCrossing;
  end;

implementation

uses ReliefObjects;

/// /////////////////////////////////////////////////////////////////////////////

constructor TCrossing.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.crossing;
  Self.BlikPositions := TList<TBlikPoint>.Create();
  Self.StaticPositions := TList<TPoint>.Create();
end;

destructor TCrossing.Destroy;
begin
  Self.StaticPositions.Free();
  Self.BlikPositions.Free();
  inherited;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TCrossing.Load(ini: TMemIniFile; key: string; version: Word);
var obj: string;
  count, section_len, usek_id_length: Integer;
begin
  inherited;

  section_len := 9;
  usek_id_length := 3;

  obj := ini.ReadString(key, 'BP', '');
  Self.BlikPositions.Clear();

  count := (Length(obj) div section_len);
  for var j: Integer := 0 to count - 1 do
  begin
    var blik_pos: TBlikPoint;
    try
      blik_pos.Pos.X := StrToInt(copy(obj, j * section_len + 1, 3));
      blik_pos.Pos.Y := StrToInt(copy(obj, j * section_len + 4, 3));
      blik_pos.PanelUsek := StrToInt(copy(obj, j * section_len + 7, usek_id_length));
    except
      continue;
    end;

    Self.BlikPositions.Add(blik_pos);
  end;

  obj := ini.ReadString(key, 'SP', '');
  Self.StaticPositions.Clear();
  for var j: Integer := 0 to (Length(obj) div 6) - 1 do
  begin
    var pos: TPoint;
    try
      pos.X := StrToInt(copy(obj, j * 6 + 1, 3));
      pos.Y := StrToInt(copy(obj, j * 6 + 4, 3));
    except
      continue;
    end;
    Self.StaticPositions.Add(pos);
  end;
end;

procedure TCrossing.Save(ini: TMemIniFile; key: string);
var obj: string;
begin
  inherited;

  obj := '';
  for var bp: TBlikPoint in Self.BlikPositions do
    obj := obj + Format('%.3d%.3d%.3d', [bp.Pos.X, bp.Pos.Y, bp.PanelUsek]);
  ini.WriteString(key, 'BP', obj);

  obj := '';
  for var point: TPoint in Self.StaticPositions do
    obj := obj + Format('%.3d%.3d', [point.X, point.Y]);
  ini.WriteString(key, 'SP', obj);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TCrossing.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  color := Self.StandardColor(colors, selected, mode);

  for var pos: TPoint in Self.StaticPositions do
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, pos, _S_CROSSING, color);

  for var bp: TBlikPoint in Self.BlikPositions do
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, bp.Pos, _S_CROSSING, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

class function TCrossing.AreBlikPointListsEq(a: TList<TBlikPoint>; b: TList<TBlikPoint>): Boolean;
begin
  if (a.Count <> b.Count) then
    Exit(False);
  for var i: Integer := 0 to a.Count-1 do
    if (a[i].Pos <> b[i].Pos) then
      Exit(False);
  Result := True;
end;

function TCrossing.GetEqCrossing(blocks: TList<TGraphBlok>): TCrossing;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.crossing) and (ArePointListsEq(TCrossing(block).StaticPositions, Self.StaticPositions)) and
        (TCrossing.AreBlikPointListsEq(TCrossing(block).BlikPositions, Self.BlikPositions))) then
      Exit(TCrossing(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
