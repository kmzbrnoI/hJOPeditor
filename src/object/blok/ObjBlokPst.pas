unit ObjBlokPst;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper,
  Generics.Collections;

type

  TPSt = class(TGraphBlok)
    Pos: TPoint;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;

    function GetEqPst(blocks: TList<TGraphBlok>): TPSt;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TPSt.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.pst;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPSt.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
end;

procedure TPSt.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TPSt.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  color := Self.StandardColor(colors, selected, mode);
  DrawObject.Canvas.Brush.color := clBlack;
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_PST_TOP, color);
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos.X, Self.Pos.Y+1, _S_PST_BOT, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TPSt.GetEqPst(blocks: TList<TGraphBlok>): TPSt;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.pst) and (TPSt(block).Pos = Self.Pos)) then
      Exit(TPSt(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
