unit ObjBlokText;

interface

uses ObjBlok, IniFiles, Types;

type

TText = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;

 procedure Save(ini:TMemIniFile; key:string); override;
end;

implementation

procedure TText.Save(ini: TMemIniFile; key: string);
begin
 if (Length(Self.Text) = 1) then
   inherited;

 ini.WriteString(key, 'T', Self.Text);
 ini.WriteInteger(key, 'X', Self.Position.X);
 ini.WriteInteger(key, 'Y', Self.Position.Y);
 ini.WriteInteger(key, 'C', Self.Color);
end;

end.
