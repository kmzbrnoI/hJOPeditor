unit ObjBlokZamek;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper, Generics.Collections;

type

  TLock = class(TGraphBlok)
    Pos: TPoint;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
    procedure Move(d: TPoint); override;

    function GetEqLock(blocks: TList<TGraphBlok>): TLock;
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
begin
  DrawObject.Canvas.Brush.color := clBlack;
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LOCK,
    Self.StandardColor(colors, selected, mode));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TLock.GetEqLock(blocks: TList<TGraphBlok>): TLock;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.lock) and (TLock(block).Pos = Self.Pos)) then
      Exit(TLock(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLock.Move(d: TPoint);
begin
  Self.Pos := Self.Pos + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
