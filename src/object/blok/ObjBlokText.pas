unit ObjBlokText;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics;

type

  TText = class(TGraphBlok)
    Text: string;
    Position: TPoint;
    Color: Integer;

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

  if (Self.typ = TBlkType.blok_popisek) then
  begin
    Self.Blok := -2;
    Self.OblRizeni := 0;
  end;

  Self.Text := ini.ReadString(key, 'T', 'text');
  Self.Position.X := ini.ReadInteger(key, 'X', 0);
  Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
  Self.Color := ini.ReadInteger(key, 'C', 0);
end;

procedure TText.Save(ini: TMemIniFile; key: string);
begin
  if ((Length(Self.Text) = 1) and (Self.typ <> TBlkType.blok_popisek)) then
    inherited;

  ini.WriteString(key, 'T', Self.Text);
  ini.WriteInteger(key, 'X', Self.Position.X);
  ini.WriteInteger(key, 'Y', Self.Position.Y);
  ini.WriteInteger(key, 'C', Self.Color);
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TText.Paint(DrawObject: TDrawObject; panelGraphics: TPanelGraphics; colors: TObjColors; selected: boolean;
  mode: TMode);
begin
  if ((Length(Self.Text) = 1) and (Self.typ = TBlkType.Text)) then
  begin
    if (selected) then
    begin
      Color := colors.selected;
    end else begin
      case (Self.Blok) of
        - 1:
          Color := colors.Alert;
        -2:
          Color := colors.IntUnassigned;
      else
        Color := colors.Normal;
      end;
    end; // else (Self.Selected > 255)
  end else begin
    Color := Self.Color;
  end;

  if (Self.typ = TBlkType.blok_popisek) then
    panelGraphics.TextOutputI(Self.Position, Self.Text, Self.Color, clBlack, true)
  else
    panelGraphics.TextOutputI(Self.Position, Self.Text, Color, clBlack);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.
