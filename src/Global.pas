﻿unit Global;
// global function, constants and types

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Types, symbolHelper;

const
  _Uvazka_Spr_Sirka = 9;

  _MAX_WIDTH = 256;
  _MAX_HEIGHT = 256;

type
  TNEvent = procedure of object;
  TPosAskEvent = function(Pos: TPoint): boolean of object;
  TOpAskEvent = function: boolean of object;

  TGlobalEvent = procedure(Sender: TObject) of object;

  TMode = (dmBitmap = 0, dmSepVert = 1, dmSepHor = 2, dmBloky = 3, dmRoots = 4);

  TCursorColor = (ccDefault = 0, ccActiveOperation = 1, ccOnObject = 2);

  TCursorDraw = record
    color: TCursorColor;
    // 0 - vychozi;1 - operace;2 - OnObject
    pos1, pos2: TPoint;
  end;

  TDrawObject = record
    canvas: TCanvas;
    symbolIL, textIL: TImageList;
    width, height: Integer;
  end;

  TObjColors = record
    selected, normal, alert, intUnassigned: SymbolColor;
  end;

function GetVersion(const FileName: string): string; // cteni verze z nastaveni
function GetPos(data: string): TPoint; overload; // format: -1;-1, 5;10 = x;y
function GetPos(data: TPoint): string; overload;

implementation

uses ownStrUtils;

/// /////////////////////////////////////////////////////////////////////////////

function GetVersion(const FileName: string): string; // cteni verze z nastaveni
var
  size, len: longword;
  handle: Cardinal;
  buffer: pchar;
  pinfo: ^VS_FIXEDFILEINFO;
  Major, Minor, Release: word;
begin
  Result := 'Verze není dostupná';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if size > 0 then
  begin
    GetMem(buffer, size);
    if GetFileVersionInfo(Pointer(FileName), 0, size, buffer) then
      if VerQueryValue(buffer, '\', Pointer(pinfo), len) then
      begin
        Major := HiWord(pinfo.dwFileVersionMS);
        Minor := LoWord(pinfo.dwFileVersionMS);
        Release := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d.%d', [Major, Minor, Release]);
      end;
    FreeMem(buffer);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

// input format: x;y
function GetPos(data: string): TPoint;
var list: TStrings;
begin
  list := TStringList.Create();
  try
    ExtractStringsEx([';'], [], data, list);
    if (list.Count < 2) then
      Exit(Point(-1, -1));
    Result := Point(StrToIntDef(list[0], -1), StrToIntDef(list[1], -1));
  finally
    list.Free();
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function GetPos(data: TPoint): string;
begin
  Result := IntToStr(data.X) + ';' + IntToStr(data.Y);
end;

/// /////////////////////////////////////////////////////////////////////////////

end.// unit
