unit ObjBlokUvazka;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper;

type

  TUvazka = class(TGraphBlok)
    Pos: TPoint;
    defalt_dir: Integer;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TUvazka.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.uvazka;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUvazka.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  Self.Pos.X := ini.ReadInteger(key, 'X', 0);
  Self.Pos.Y := ini.ReadInteger(key, 'Y', 0);
  Self.defalt_dir := ini.ReadInteger(key, 'D', 0);
end;

procedure TUvazka.Save(ini: TMemIniFile; key: string);
begin
  inherited;

  ini.WriteInteger(key, 'X', Self.Pos.X);
  ini.WriteInteger(key, 'Y', Self.Pos.Y);
  ini.WriteInteger(key, 'D', Self.defalt_dir);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TUvazka.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  if (selected) then
  begin
    color := colors.Selected;
  end else begin
    case (Self.blok) of
      -1: color := colors.Alert;
      -2: color := colors.IntUnassigned;
    else
      color := colors.Normal;
    end;
  end;

  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos, _S_LINKER_B, color);
  SymbolDraw(DrawObject.SymbolIL, DrawObject.Canvas, Self.Pos.X+1, Self.Pos.Y, _S_LINKER_B+1, color);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
