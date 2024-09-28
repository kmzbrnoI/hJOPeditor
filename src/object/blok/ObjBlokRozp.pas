unit ObjBlokRozp;

interface

uses ObjBlok, IniFiles, Types, Global, Graphics, symbolHelper, PGraphics,
  Generics.Collections;

type

  TDisconnector = class(TGraphBlok)
    Pos: TPoint;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
    procedure Move(d: TPoint); override;

    function GetEqDisconnector(blocks: TList<TGraphBlok>): TDisconnector;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TDisconnector.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.disconnector;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDisconnector.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
end;

procedure TDisconnector.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDisconnector.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
begin
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_DISC_ALONE,
    Self.StandardColor(colors, selected, mode));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TDisconnector.GetEqDisconnector(blocks: TList<TGraphBlok>): TDisconnector;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.disconnector) and (TDisconnector(block).Pos = Self.Pos)) then
      Exit(TDisconnector(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TDisconnector.Move(d: TPoint);
begin
  Self.Pos := Self.Pos + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
