unit ObjBlokUvazka;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper, Generics.Collections;

type

  TLinker = class(TGraphBlok)
    Pos: TPoint;
    defalt_dir: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
    procedure Move(d: TPoint); override;

    function GetEqLinker(blocks: TList<TGraphBlok>): TLinker;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TLinker.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.linker;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinker.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.defalt_dir := ini.ReadInteger(key, 'D', 0);
end;

procedure TLinker.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'D', Self.defalt_dir);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinker.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  color := Self.StandardColor(colors, selected, mode);
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LINKER_B, color);
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos.X+1, Self.Pos.Y, _S_LINKER_B+1, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

function TLinker.GetEqLinker(blocks: TList<TGraphBlok>): TLinker;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.linker) and (TLinker(block).Pos = Self.Pos)) then
      Exit(TLinker(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLinker.Move(d: TPoint);
begin
  Self.Pos := Self.Pos + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
