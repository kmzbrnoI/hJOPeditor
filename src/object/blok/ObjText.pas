unit ObjText;

interface

uses ObjBlok, IniFiles;

type

TText = class(TGraphBlok)
 Text:string;
 Position:TPoint;
 Color:Integer;

 procedure Save(ini:TMemIniFile; key:string);
end;//Text


implementation

end.
