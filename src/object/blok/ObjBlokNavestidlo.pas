unit ObjBlokNavestidlo;

interface

uses ObjBlok, Types, IniFiles, Global, SymbolHelper, PGraphics;

type

  TNavestidlo = class(TGraphBlok)
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

constructor TNavestidlo.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.navestidlo;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TNavestidlo.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.SymbolID := ini.ReadInteger(key, 'S', 0);
  if (version < _FILEVERSION_20) then
    Self.SymbolID := TranscodeSymbolFromBpnlV3(Self.SymbolID);
end;

procedure TNavestidlo.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'S', Self.SymbolID);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TNavestidlo.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors;
  selected: boolean; mode: TMode);
begin
  if (selected) then
  begin
    SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _Navestidlo_Start + Self.SymbolID, colors.Selected);
  end else begin
    case (Self.Blok) of
      -1: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _Navestidlo_Start + Self.SymbolID, colors.Alert);
      -2: SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _Navestidlo_Start + Self.SymbolID, colors.IntUnassigned);
    else
      SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Position, _Navestidlo_Start + Self.SymbolID, colors.Normal);
    end;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
