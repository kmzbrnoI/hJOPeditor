unit ObjBlokUvazkaSpr;

interface

uses ObjBlok, Types, IniFiles, Global, PGraphics, Graphics, symbolHelper, Generics.Collections;

type

  TUvazkaSprVertDir = (top = 0, bottom = 1);

  TLinkerTrain = class(TGraphBlok)
    Pos: TPoint;
    vertical_dir: TUvazkaSprVertDir;
    spr_cnt: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
    procedure Move(d: TPoint); override;

    function GetEqLinkerTrain(blocks: TList<TGraphBlok>): TLinkerTrain;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TLinkerTrain.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.linker_train;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinkerTrain.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.vertical_dir := TUvazkaSprVertDir(ini.ReadInteger(key, 'VD', 0));
  Self.spr_cnt := ini.ReadInteger(key, 'C', 1);
end;

procedure TLinkerTrain.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'VD', Integer(Self.vertical_dir));
  ini.WriteInteger(key, 'C', Self.spr_cnt);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinkerTrain.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors;
  selected: boolean; mode: TMode);
var color: SymbolColor;
begin
  if ((mode in [dmBlocks, dmRoots]) and (Self.block > -1)) then
    color := scYellow
  else
    color := Self.StandardColor(colors, selected, mode);

  DrawObject.Canvas.Pen.color := SymbolColorToColor(color);
  DrawObject.Canvas.Brush.color := clBlack;

  case (Self.vertical_dir) of
    TUvazkaSprVertDir.top:
      begin
        DrawObject.Canvas.Rectangle(Self.Pos.X * _SYMBOL_WIDTH, Self.Pos.Y * _SYMBOL_HEIGHT + _SYMBOL_HEIGHT - 1,
          (Self.Pos.X + _Uvazka_Spr_Sirka) * _SYMBOL_WIDTH - 1, (Self.Pos.Y - Self.spr_cnt + 1) * _SYMBOL_HEIGHT);
      end;

    TUvazkaSprVertDir.bottom:
      begin
        DrawObject.Canvas.Rectangle(Self.Pos.X * _SYMBOL_WIDTH, Self.Pos.Y * _SYMBOL_HEIGHT,
          (Self.Pos.X + _Uvazka_Spr_Sirka) * _SYMBOL_WIDTH - 1, (Self.Pos.Y + Self.spr_cnt) * _SYMBOL_HEIGHT - 1);
      end;
  end;

  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LINKER_TRAIN, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TLinkerTrain.GetEqLinkerTrain(blocks: TList<TGraphBlok>): TLinkerTrain;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.linker_train) and (TLinkerTrain(block).Pos = Self.Pos)) then
      Exit(TLinkerTrain(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinkerTrain.Move(d: TPoint);
begin
  Self.Pos := Self.Pos + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
