unit Global;
// global function, constants and types

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Types, symbolHelper;

const
  _Uvazka_Spr_Sirka = 9;

  _MAX_WIDTH = 1000;
  _MAX_HEIGHT = 256;

  _RELEASE: Boolean = False;

type
  TNEvent = procedure of object;
  TPosAskEvent = function(Pos: TPoint): boolean of object;
  TOpAskEvent = function: boolean of object;

  TGlobalEvent = procedure(Sender: TObject) of object;

  TMode = (dmBitmap = 0, dmSepVert = 1, dmSepHor = 2, dmBlocks = 3, dmRoots = 4, dmAreas = 5);

  TCursorColor = (ccDefault = 0, ccActiveOperation = 1, ccOnObject = 2);

  TCursorDraw = record
    color: TCursorColor;
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

function VersionStr(const FileName: string): string; // cteni verze z nastaveni
function BuildDateTime(): TDateTime;
function GetPos(data: string): TPoint; overload; // format: -1;-1, 5;10 = x;y
function GetPos(data: TPoint): string; overload;

implementation

uses ownStrUtils, DateUtils;

/// /////////////////////////////////////////////////////////////////////////////

function VersionStr(const FileName: string): string; // cteni verze z nastaveni
var
  size: longword;
  handle: Cardinal;
  pinfo: ^VS_FIXEDFILEINFO;
begin
  Result := '???';
  size := GetFileVersionInfoSize(Pointer(FileName), handle);
  if (size > 0) then
  begin
    var buffer: pchar;
    GetMem(buffer, size);
    if (GetFileVersionInfo(Pointer(FileName), 0, size, buffer)) then
    begin
      var len: longword;
      if (VerQueryValue(buffer, '\', Pointer(pinfo), len)) then
      begin
        var Major: Word := HiWord(pinfo.dwFileVersionMS);
        var Minor: Word := LoWord(pinfo.dwFileVersionMS);
        var Release: Word := HiWord(pinfo.dwFileVersionLS);
        Result := Format('%d.%d.%d', [Major, Minor, Release]);
        if (not _RELEASE) then
          Result := Result + '-dev';
      end;
    end;
    FreeMem(buffer);
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////

function BuildDateTime(): TDateTime;
begin
  Result := (TTimeZone.Local.ToLocalTime(PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^._lfanew))^.FileHeader.TimeDateStamp / SecsPerDay) + UnixDateDelta);
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
