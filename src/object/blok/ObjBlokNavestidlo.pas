unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles, Global, SymbolHelper, PGraphics;

type

  TSignal = class(TGraphBlok)
    Position: TPoint;
    SymbolID: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
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
  if (selected) then
  begin
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _S_SIGNAL_B + Self.SymbolID, colors.Selected);
  end else begin
    case (Self.block) of
      -1: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _S_SIGNAL_B + Self.SymbolID, colors.Alert);
      -2: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _S_SIGNAL_B + Self.SymbolID, colors.IntUnassigned);
    else
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _S_SIGNAL_B + Self.SymbolID, colors.Normal);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
