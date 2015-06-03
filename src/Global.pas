unit Global;
//global function, constants and types

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils;

function GetVersion(const FileName: string): string;//cteni verze z nastaveni

function GetPos(data:string):TPoint; overload;              //format: -1;-1, 5;10 = x;y
function GetPos(data:TPoint):string; overload;

type
 TNEvent = procedure of object;
 TPosAskEvent = function(Pos:TPoint):boolean of object;
 TOpAskEvent = function:boolean of object;

 TGlobalEvent = procedure(Sender:TObject) of object;

 TMode=(dmBitmap = 0, dmOddelovace = 1, dmBloky = 2, dmRoots = 3);

 TCursorDraw=record
   Color:ShortInt;
   //0 - vychozi;1 - operace;2 - OnObject
   Pos1,Pos2:TPoint;
 end;

 TReliefSym=record
  Position:TPoint;
  SymbolID:Integer;
 end;


implementation

function GetVersion(const FileName: string): string;//cteni verze z nastaveni
var
  size, len: longword;
  handle: THandle;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor, Release: word;
begin
  Result:='Verze není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if size > 0 then begin
    GetMem(buffer, size);
    if GetFileVersionInfo(Pointer(FileName), 0, size, buffer)
    then
      if VerQueryValue(buffer, '\', pointer(pinfo), len) then begin
        Major   := HiWord(pinfo.dwFileVersionMS);
        Minor   := LoWord(pinfo.dwFileVersionMS);
        Release := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d.%d',[Major, Minor, Release]);
      end;
    FreeMem(buffer);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

//input format: x;y
function GetPos(data:string):TPoint;
var list:TStrings;
begin
 list := TStringList.Create;
 ExtractStrings([';'], [], PChar(data), list);

 if (list.Count < 2) then
  begin
   Result := Point(-1,-1);
   Exit;
  end;

 Result := Point(StrToIntDef(list[0],-1), StrToIntDef(list[1],-1));

 list.Free;
end;//function

////////////////////////////////////////////////////////////////////////////////

function GetPos(data:TPoint):string;
begin
 Result := IntToStr(data.X) + ';' + IntToStr(data.Y);
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
