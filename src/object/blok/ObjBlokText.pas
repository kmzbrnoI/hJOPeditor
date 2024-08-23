unit ObjBlokText;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics, symbolHelper;

type

  TText = class(TGraphBlok)
    Text: string;
    Position: TPoint;
    Color: SymbolColor;

    constructor Create(index: Integer);
    procedure Load(ini: TMemIniFile; key: string; version: Word); override;
    procedure Save(ini: TMemIniFile; key: string); override;
    procedure Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
      mode: TMode); override;
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TText.Create(index: Integer);
begin
  inherited;
  Self.typ := TBlkType.Text;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TText.Load(ini: TMemIniFile; key: string; version: Word);
begin
  inherited;

  if (Self.typ = TBlkType.description) then
  begin
    Self.block := -2;
    Self.area := 0;
  end;

  Self.Text := ini.ReadString(key, 'T', 'text');
  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.Color := SymbolColor(ini.ReadInteger(key, 'C', 0));
end;

procedure TText.Save(ini: TMemIniFile; key: string);
begin
  if ((Length(Self.Text) = 1) and (Self.typ <> TBlkType.description)) then
    inherited;

  ini.WriteString(key, 'T', Self.Text);
  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'C', Integer(Self.Color));
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TText.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
var color: SymbolColor;
begin
  if ((Length(Self.Text) = 1) and (Self.typ = TBlkType.Text)) then
  begin
    color := Self.StandardColor(colors, selected, mode);
  end else begin
    if (mode = dmAreas) then
      Exit(); // not painting non-block texts in dmAreas
    color := Self.Color;
  end;

  if (Self.typ = TBlkType.description) then
    panelGraphics.TextOutputI(Self.Position, Self.Text, Self.Color, clBlack, true)
  else
    panelGraphics.TextOutputI(Self.Position, Self.Text, Color, clBlack);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
