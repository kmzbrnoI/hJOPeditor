unit ObjBlokVykol;

interface

uses ObjBlok, Types, IniFiles, Global, symbolHelper, Graphics, PGraphics, Generics.Collections;

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
    procedure Move(d: TPoint); override;

    function GetEqDerail(blocks: TList<TGraphBlok>): TDerail;
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
begin
  DrawObject.Canvas.Brush.color := clBlack;
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_DERAIL_B + Self.symbol,
    Self.StandardColor(colors, selected, mode));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TDerail.GetEqDerail(blocks: TList<TGraphBlok>): TDerail;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.derail) and (TDerail(block).Pos = Self.Pos) and (TDerail(block).symbol = Self.symbol)) then
      Exit(TDerail(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDerail.Move(d: TPoint);
begin
  Self.Pos := Self.Pos + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
