unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles, Global, SymbolHelper, PGraphics, Generics.Collections;

type

  TSignal = class(TGraphBlok)
    Position: TPoint;
    SymbolID: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
    procedure Move(d: TPoint); override;

    function GetEqSignal(blocks: TList<TGraphBlok>): TSignal;
  end;

implementation

uses ReliefObjects;

/// /////////////////////////////////////////////////////////////////////////////

constructor TSignal.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.signal;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSignal.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.SymbolID := ini.ReadInteger(key, 'S', 0);
  if (version < _FILEVERSION_20) then
    Self.SymbolID := TranscodeSymbolFromBpnlV3(Self.SymbolID);
end;

procedure TSignal.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'S', Self.SymbolID);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSignal.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors;
  selected: boolean; mode: TMode);
begin
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _S_SIGNAL_B + Self.SymbolID,
    Self.StandardColor(colors, selected, mode));
end;

/// /////////////////////////////////////////////////////////////////////////////

function TSignal.GetEqSignal(blocks: TList<TGraphBlok>): TSignal;
begin
  for var block: TGraphBlok in blocks do
    if ((block.typ = TBlkType.signal) and (TSignal(block).Position = Self.Position) and (TSignal(block).SymbolID = Self.SymbolID)) then
      Exit(TSignal(block));
  Result := nil;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TSignal.Move(d: TPoint);
begin
  Self.Position := Self.Position + d;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
