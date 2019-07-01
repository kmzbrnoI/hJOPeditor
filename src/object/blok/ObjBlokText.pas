unit ObjBlokText;

interface

uses ObjBlok, IniFiles, Types, Global, PGraphics, Graphics;

type

TText = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;

 constructor Create(index:Integer);
 procedure Load(ini:TMemIniFile; key:string); override;
 procedure Save(ini:TMemIniFile; key:string); override;
 procedure Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                  selected:boolean; mode:TMode); override;
end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TText.Create(index:Integer);
begin
 inherited;
 Self.typ := TBlkType.text;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TText.Load(ini: TMemIniFile; key: string);
begin
 inherited;

 Self.Text := ini.ReadString(key, 'T', 'text');
 Self.Position.X := ini.ReadInteger(key, 'X', 0);
 Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
 Self.Color := ini.ReadInteger(key, 'C', 0);
end;

procedure TText.Save(ini: TMemIniFile; key: string);
begin
 if (Length(Self.Text) = 1) then
   inherited;

 ini.WriteString(key, 'T', Self.Text);
 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'C', Self.Color);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TText.Paint(DrawObject:TDrawObject; panelGraphics:TPanelGraphics; colors:TObjColors;
                      selected:boolean; mode:TMode);
begin
 if ((Length(Self.Text) = 1) and (Self.typ = TBlkType.text)) then
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
    end;//else (Self.Selected > 255)
  end else begin
    color := Self.Color;
  end;

 if (Self.typ = TBlkType.blok_popisek) then
   panelGraphics.TextOutputI(Self.Position, Self.Text, Self.Color, clBlack, true)
 else
   panelGraphics.TextOutputI(Self.Position, Self.Text, color, clBlack);
end;

////////////////////////////////////////////////////////////////////////////////

end.
