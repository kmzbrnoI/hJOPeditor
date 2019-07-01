unit ObjBlokText;

interface

uses ObjBlok, IniFiles, Types;

type

TText = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;

 constructor Create(index:Integer);
 procedure Load(ini:TMemIniFile; key:string); override;
 procedure Save(ini:TMemIniFile; key:string); override;
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

 Self.Text       := ini.ReadString(key, 'T', 'text');
 Self.Position.X := ini.ReadInteger(key, 'X', 0);
 Self.Position.Y := ini.ReadInteger(key, 'Y', 0);
 Self.Color      := ini.ReadInteger(key, 'C', 0);
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

end.
