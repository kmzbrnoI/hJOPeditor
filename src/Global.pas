﻿unit Global;
//global function, constants and types

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Types;

const
 //zde je definovan pocet barev
 _Symbol_ColorsCount = 10;
 //zde jsou definovany jednotluve barvy
 _Symbol_Colors: array [0..(_Symbol_ColorsCount-1)] of TColor = ($FF00FF,$A0A0A0,$0000FF,$00FF00,$FFFFFF,$FFFF00,$FF0000,$00FFFF,$800080,$000000);

 _Uvazka_Spr_Sirka = 9;
 _MAX_BLK     = 1023;

 _MAX_WIDTH   = 256;
 _MAX_HEIGHT  = 256;

type
 TNEvent = procedure of object;
 TPosAskEvent = function(Pos:TPoint):boolean of object;
 TOpAskEvent = function:boolean of object;

 TGlobalEvent = procedure(Sender:TObject) of object;

 TMode =(dmBitmap = 0, dmSepVert = 1, dmSepHor = 2, dmBloky = 3, dmRoots = 4);

 TCursorDraw=record
   Color:ShortInt;
   //0 - vychozi;1 - operace;2 - OnObject
   Pos1,Pos2:TPoint;
 end;

 TDrawObject = record
   Canvas:TCanvas;
   SymbolIL,TextIL:TImageList;
   Width,Height:Integer;
 end;

 TObjColors = record
   Selected,Normal,Alert,IntUnassigned:Byte;
 end;


function GetVersion(const FileName: string): string;//cteni verze z nastaveni
function GetPos(data:string):TPoint; overload;              //format: -1;-1, 5;10 = x;y
function GetPos(data:TPoint):string; overload;

implementation

uses ownStrUtils;

////////////////////////////////////////////////////////////////////////////////

function GetVersion(const FileName: string): string;//cteni verze z nastaveni
var
  size, len: longword;
  handle: Cardinal;
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
 list := TStringList.Create();
 try
   ExtractStringsEx([';'], [], data, list);
   if (list.Count < 2) then
     Exit(Point(-1,-1));
   Result := Point(StrToIntDef(list[0],-1), StrToIntDef(list[1],-1));
 finally
   list.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function GetPos(data:TPoint):string;
begin
 Result := IntToStr(data.X) + ';' + IntToStr(data.Y);
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit
