unit ReliefBitmap;
// tato unita pracuje s bitmapovymi daty reliefu

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, ReliefText, VektorBasedObject, ReliefBitmapSymbols, Global, Forms,
  OblastRizeni, PGraphics, symbolHelper, Generics.Collections, Types, ReliefCommon;

const
  _IMPORT_MYJOP_SUFFIX = '.pnj';
  _INACTIVE_POINT: TPoint = (X: -1; Y: -1);

type
  TORAskEvent = function(Pos: TPoint): Boolean of object;
  TAddType = (apSymbol, apJCClick, apTrackName, apTrainPos, apSeparVert, apSeparHor, apText);

  TPanelBitmap = class
  private
    mPanelWidth, mPanelHeight: Integer;
    mMode: TMode;
    mFileName: string;

    Operations: record
      addStep: TGOpStep;
      addType: TAddType;
      moveSize: TPoint;
      moveStep: TGOpStep;
      deleteStep: TGOpStep;
      isGroup: Boolean;
      groupStart: TPoint;
    end;

    mFileState: TReliefFileState;
    Graphics: TPanelGraphics;
    fShowBlokPopisky: Boolean;

    mORAskEvent: TORAskEvent;
    mOnShow: TNEvent;
    mOnTextEdit: TChangeTextEvent;

    procedure SetGroup(State: Boolean);
    function GetGroup(): Boolean;

    function IsOperation(): Boolean;
    
    procedure Show();
    function IsConflictSymbolPlace(Pos: TPoint): Boolean;
    function IsConflictTextPlace(Pos: TPoint): Boolean;
    function IsConflictVectorPlace(Pos: TPoint): Boolean;
    procedure ChangeTextEvent(Sender: TObject; var popisek: TPanelLabel);
    class procedure BpnlReadAndValidateSeparator(var f: File; where: string);

    function CursorAddMoveDelete(CursorPos: TPoint): TCursorDraw;

    procedure AddMouseUp(CursorPos: TPoint);
    procedure MoveMouseUp(CursorPos: TPoint);
    procedure DeleteMouseUp(CursorPos: TPoint);

    function IsObject(pos: TPoint): Boolean;
    function CompensateMode(pos: TPoint): TPoint;

  public
    Symbols: TBitmapSymbols;
    SeparatorsVert: TVBO;
    SeparatorsHor: TVBO;
    TrackNames: TVBO;
    JCClick: TVBO;
    Texts: TText;
    TrainPoss: TVBO;

    bitmap: array [0 .. _MAX_WIDTH - 1, 0 .. _MAX_HEIGHT - 1] of ShortInt;

    constructor Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Mode: TMode;
      Parent: TForm; Graphics: TPanelGraphics);
    destructor Destroy(); override;

    procedure BpnlLoad(filename: string; var ORs: string);
    procedure BpnlSave(filename: string; const ORs: string);

    procedure Paint();
    function PaintCursor(CursorPos: TPoint): TCursorDraw;
    procedure PaintMove(CursorPos: TPoint);

    procedure SetSize(aWidth, aHeight: Byte);

    procedure Escape(Group: Boolean);

    procedure AddSymbol(symbolID: Integer);
    procedure AddSeparVert();
    procedure AddSeparHor();
    procedure AddTrackName();
    procedure AddJCClick();
    procedure AddTrainPos();
    procedure AddText(text: string; color: SymbolColor; description: Boolean);

    procedure Move();
    procedure Delete();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);
    procedure DblClick(Position: TPoint);

    function ImportMyJOP(fn: string; ORs: TList<TOR>): string;

    property fileName: string read mFileName;
    property fileState: TReliefFileState read mFileState;
    property PanelWidth: Integer read mPanelWidth;
    property PanelHeight: Integer read mPanelHeight;
    property Group: Boolean read GetGroup write SetGroup;
    property Mode: TMode read mMode write mMode;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property OnTextEdit: TChangeTextEvent read mOnTextEdit write mOnTextEdit;
    property IsAreaStuffPresent: TORAskEvent read mORAskEvent write mORAskEvent;
    property ShowBlokPopisky: Boolean read fShowBlokPopisky write fShowBlokPopisky;
  end; // class

implementation

uses ReliefObjects, ownStrUtils;

// nacitani souboru s bitmapovymi daty
procedure TPanelBitmap.BpnlLoad(filename: string; var ORs: string);
var buffer: array [0 .. 5] of Byte;
  aCount: Integer;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := filename;

  Self.Symbols.Clear();
  Self.SeparatorsVert.Clear();
  Self.SeparatorsHor.Clear();
  Self.texts.Clear();

  var f: File;
  AssignFile(f, filename);
  Reset(f, 1);

  try
    BlockRead(f, buffer, 5, aCount);
    if (aCount < 5) then
      raise EFileLoad.Create('Nesprávná délka hlavičky!');

    // --- hlavicka zacatek ---
    // kontrola identifikace
    if ((buffer[0] <> ord('b')) or (buffer[1] <> ord('r'))) then
      raise EFileLoad.Create('Nesprávná identifikace v hlavičce!');

    // kontrola verze
    var version: Byte := buffer[2];
    if ((version <> $21) and (version <> $30) and (version <> $31) and (version <> $32) and (version <> $40) and (version <> $41)) then
      Application.MessageBox(PChar('Otevíráte soubor s verzí ' + IntToHex(version,
        2) + ', která není aplikací plně podporována!'), 'Varování', MB_OK OR MB_ICONWARNING);

    Self.mPanelWidth := buffer[3];
    Self.mPanelHeight := buffer[4];

    Self.BpnlReadAndValidateSeparator(f, 'mezi hlavičkou a bitmapovými daty');

    // --- hlavicka konec ---
    // -------------------------------------------
    // nacitani bitmapovych dat

    Self.Symbols.LoadBpnl(f, version, Self.PanelWidth, Self.PanelHeight);
    Self.BpnlReadAndValidateSeparator(f, 'mezi bitmapovými daty a popisky');

    // -------------------------------------------

    if (version >= $32) then
    begin
      // nacteni poctu popisku
      BlockRead(f, buffer, 2, aCount);
      var len := (buffer[0] shl 8) + buffer[1];

      // nacitani popisku
      var bytesBuf: TBytes;
      SetLength(bytesBuf, len);
      BlockRead(f, bytesBuf[0], len, aCount);
      Self.texts.SetLoadedDataV32(bytesBuf);
    end else begin
      // nacteni poctu popisku
      BlockRead(f, buffer, 1, aCount);
      var len := buffer[0] * ReliefText._Block_Length;

      // nacitani popisku
      var bytesBuf: TBytes;
      SetLength(bytesBuf, len);
      BlockRead(f, bytesBuf[0], len, aCount);
      Self.texts.SetLoadedData(bytesBuf);
    end;

    Self.BpnlReadAndValidateSeparator(f, 'mezi popisky a separátory');

    // -------------------------------------------

    Self.SeparatorsVert.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'za separátory');

    // -------------------------------------------

    if (version >= $30) then
    begin
      Self.SeparatorsHor.LoadBpnl(f, version);
      Self.BpnlReadAndValidateSeparator(f, 'mezi hor. separátory a kpopisky');
    end else begin
      Self.SeparatorsHor.Clear();
    end;

    // -------------------------------------------

    Self.trackNames.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi kolejovými popisky a JCClick');

    // -------------------------------------------

    Self.JCClick.LoadBpnl(f, version);
    Self.BpnlReadAndValidateSeparator(f, 'mezi JCClick a pozicemi pro soupravy');

    // -------------------------------------------

    if (version >= $31) then
    begin
      Self.trainPoss.LoadBpnl(f, version);
      Self.BpnlReadAndValidateSeparator(f, 'mezi pozicemi pro soupravy a oblastmi řízení');
    end;

    // -------------------------------------------

    // nacitani oblasti rizeni
    ORs := '';

    // precteme delku
    BlockRead(f, buffer, 2, aCount);

    // pokud je delka 0, je neco spatne
    if (aCount = 0) then
      raise EFileLoad.Create('Prázdné oblasti řízení!');

    var len := (buffer[0] shl 8) + buffer[1];

    var bytesBuf: TBytes;
    SetLength(bytesBuf, len);
    BlockRead(f, bytesBuf[0], len, aCount);
    ORs := TEncoding.UTF8.GetString(bytesBuf, 0, aCount);
  finally
    CloseFile(f);
  end;
end;

class procedure TPanelBitmap.BpnlReadAndValidateSeparator(var f: File; where: string);
var buf: array [0..1] of Byte;
    count: Integer;
begin
  BlockRead(f, buf, 2, count);
  if ((count < 2) or (buf[0] <> $FF) or (buf[1] <> $FF)) then
    raise EFileLoad.Create('Chybí oddělovací sekvence '+where+'!');
end;

procedure TPanelBitmap.BpnlSave(filename: string; const ORs: string);
var buffer: array [0 .. 4] of Byte;
  separator: array [0 .. 1] of Byte;
begin
  Self.mFileState := fsSaved;
  Self.mFileName := filename;

  separator[0] := $FF;
  separator[1] := $FF;

  var f: File;
  AssignFile(f, filename);
  Rewrite(f, 1);

  try
    // --- hlavicka ---
    // identifikace
    buffer[0] := ord('b');
    buffer[1] := ord('r');
    // verze
    buffer[2] := $41;
    // sirka a vyska
    buffer[3] := Self.PanelWidth;
    buffer[4] := Self.PanelHeight;

    BlockWrite(f, buffer, 5);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // bitmapova data
    Self.Symbols.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // popisky
    var bytesBuf := Self.texts.GetSaveData;
    BlockWrite(f, bytesBuf[0], Length(bytesBuf));
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // vertikalni oddelovace
    Self.SeparatorsVert.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // horizontalni oddelovace
    Self.SeparatorsHor.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // KPopisky
    Self.trackNames.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // JCClick
    Self.JCClick.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    // soupravy
    Self.trainPoss.WriteBpnl(f);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
    var len := TEncoding.UTF8.GetByteCount(ORs);
    SetLength(bytesBuf, len);

    // delka zpravy
    buffer[0] := hi(len);
    buffer[1] := lo(len);
    BlockWrite(f, buffer, 2);

    bytesBuf := TEncoding.UTF8.GetBytes(ORs);
    BlockWrite(f, bytesBuf[0], len);
    BlockWrite(f, separator, 2);

    // -------------------------------------------
  finally
    CloseFile(f);
  end;
end;

procedure TPanelBitmap.SetSize(aWidth, aHeight: Byte);
begin
  if (Self.IsOperation) then
    raise Exception.Create('Právě probíhá jiná operace!');

  Self.mPanelWidth := aWidth;
  Self.mPanelHeight := aHeight;

  Self.Symbols.SetRozmery(aWidth, aHeight);
end;

constructor TPanelBitmap.Create(SymbolIL, TextIL: TImageList; DrawCanvas: TCanvas; Width, Height: Integer; Mode: TMode;
  Parent: TForm; Graphics: TPanelGraphics);
begin
  inherited Create;

  Self.fShowBlokPopisky := true;
  Self.Mode := Mode;
  Self.Graphics := Graphics;

  Self.Operations.addStep := gosNone;
  Self.Operations.moveStep := gosNone;
  Self.Operations.deleteStep := gosNone;
  Self.Operations.groupStart := _INACTIVE_POINT;

  Self.Symbols := TBitmapSymbols.Create(SymbolIL, DrawCanvas, Width, Height);
  Self.Symbols.QIsConflict := Self.IsConflictSymbolPlace;

  Self.SeparatorsVert := TVBO.Create(DrawCanvas, SymbolIL, _S_SEPAR_VERT, scRed, stVert);
  Self.SeparatorsHor := TVBO.Create(DrawCanvas, SymbolIL, _S_SEPAR_HOR, scRed, stHor);

  Self.TrackNames := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scYellow);
  Self.TrackNames.QIsConflict := Self.IsConflictVectorPlace;

  Self.JCClick := TVBO.Create(DrawCanvas, SymbolIL, _S_KC, scLime);
  Self.JCClick.QIsConflict := Self.IsConflictVectorPlace;

  Self.TrainPoss := TVBO.Create(DrawCanvas, SymbolIL, _S_FULL, scBlue);
  Self.TrainPoss.QIsConflict := Self.IsConflictVectorPlace;

  Self.Texts := TText.Create(DrawCanvas, TextIL, Parent, Graphics);
  Self.Texts.OnShow := Self.Show;
  Self.Texts.QIsConflict := Self.IsConflictTextPlace;
  Self.Texts.OnChangeText := Self.ChangeTextEvent;

  Self.SetSize(Width, Height);
end;

destructor TPanelBitmap.Destroy;
begin
  FreeAndNil(Self.Symbols);
  FreeAndNil(Self.SeparatorsVert);
  FreeAndNil(Self.SeparatorsHor);
  FreeAndNil(Self.trackNames);
  FreeAndNil(Self.JCClick);
  FreeAndNil(Self.trainPoss);
  FreeAndNil(Self.texts);

  inherited;
end;

// vykresleni vseho
procedure TPanelBitmap.Paint();
begin
  Self.JCClick.Paint();
  Self.trainPoss.Paint();
  Self.trackNames.Paint();
  Self.Symbols.Paint();
  Self.SeparatorsVert.Paint();
  Self.SeparatorsHor.Paint();
  Self.texts.Paint(Self.fShowBlokPopisky);
end;

procedure TPanelBitmap.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if ((Self.Operations.addStep > TGOpStep.gosNone) and (Button = TMouseButton.mbLeft)) then
    Self.AddMouseUp(Position)
  else if ((Self.Operations.moveStep > TGOpStep.gosNone) and (Button = TMouseButton.mbLeft)) then
    Self.MoveMouseUp(Position)
  else if ((Self.Operations.deleteStep > TGOpStep.gosNone) and (Button = TMouseButton.mbLeft)) then
    Self.DeleteMouseUp(Position);

  case (Self.Mode) of
    dmBitmap:
      Self.texts.MouseUp(Position, Button);
  end;
end;

procedure TPanelBitmap.DblClick(Position: TPoint);
begin
  var texti := Self.Texts.GetTextI(Position);
  if (texti > -1) then
  begin
    var lbl: TPanelLabel := Self.Texts.GetText(texti);
    Self.ChangeTextEvent(Self, lbl);
    Self.Texts.SetText(texti, lbl);
  end;
end;

procedure TPanelBitmap.Show();
begin
  if (Assigned(Self.OnShow)) then
    Self.OnShow();
end;

function TPanelBitmap.IsConflictSymbolPlace(Pos: TPoint): Boolean;
begin
  Result := (Self.Symbols.GetSymbol(Pos) <> -1) or ((Assigned(Self.IsAreaStuffPresent)) and (Self.IsAreaStuffPresent(Pos)))
    or (pos.X < 0) or (pos.Y < 0) or (pos.X >= Self.PanelWidth) or (pos.Y >= Self.PanelHeight);
end;

function TPanelBitmap.IsConflictTextPlace(Pos: TPoint): Boolean;
begin
  Result := false;

  if ((pos.X < 0) or (pos.Y < 0) or (pos.X >= Self.PanelWidth) or (pos.Y >= Self.PanelHeight)) then
    Exit(True);

  // Text could be placed over platform
  var symbol := Self.Symbols.GetSymbol(Pos);
  if ((symbol <> -1) and ((symbol < _S_PLATFORM_B) or (symbol > _S_PLATFORM_E))) then
    Exit(true);
  if (Self.texts.GetTextI(Pos) <> -1) then
    Exit(true);
  if (Assigned(Self.IsAreaStuffPresent)) then
    Exit(Self.IsAreaStuffPresent(Pos));
end;

function TPanelBitmap.IsConflictVectorPlace(Pos: TPoint): Boolean;
begin
  Result := (Self.trackNames.IsObject(Pos)) or (Self.JCClick.IsObject(Pos)) or (Self.trainPoss.IsObject(Pos))
    or (pos.X < 0) or (pos.Y < 0) or (pos.X >= Self.PanelWidth) or (pos.Y >= Self.PanelHeight);
end;

procedure TPanelBitmap.SetGroup(State: Boolean);
begin
  Self.Operations.isGroup := State;
end;

function TPanelBitmap.GetGroup(): Boolean;
begin
  Result := Self.Operations.isGroup;
end;

procedure TPanelBitmap.Escape(Group: Boolean);
begin
  if (Self.Operations.moveStep = gosMoving) then
  begin
    Self.Symbols.MoveDrop(Self.Operations.groupStart);
    Self.SeparatorsVert.MoveDrop(Self.Operations.groupStart);
    Self.SeparatorsHor.MoveDrop(Self.Operations.groupStart);
    Self.TrackNames.MoveDrop(Self.Operations.groupStart);
    Self.JCClick.MoveDrop(Self.Operations.groupStart);
    Self.Texts.MoveDrop(Self.Operations.groupStart);
    Self.TrainPoss.MoveDrop(Self.Operations.groupStart);
  end;
  Self.Operations.groupStart := _INACTIVE_POINT;
  Self.Operations.addStep := gosNone;
  Self.Operations.moveStep := gosNone;
  Self.Operations.deleteStep := gosNone;
end;

function TPanelBitmap.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  if ((Self.Operations.addStep > TGOpStep.gosNone) or (Self.Operations.moveStep > TGOpStep.gosNone) or (Self.Operations.deleteStep > TGOpStep.gosNone)) then
    Exit(Self.CursorAddMoveDelete(CursorPos));

  if (Self.IsObject(CursorPos)) then
    Result.color := TCursorColor.ccOnObject
  else
    Result.color := TCursorColor.ccDefault;

  Result.pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
end;

function TPanelBitmap.IsOperation(): Boolean;
begin
  Result := (Self.Operations.addStep > gosActive) or (Self.Operations.moveStep > gosActive) or (Self.Operations.deleteStep > gosActive);
end;

procedure TPanelBitmap.AddSymbol(symbolID: Integer);
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apSymbol;
  Self.Operations.addStep := TGOpStep.gosActive;
  Self.Symbols.addSymbol := symbolID;
end;

procedure TPanelBitmap.AddSeparVert();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apSeparVert;
  Self.Operations.addStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.AddSeparHor();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apSeparHor;
  Self.Operations.addStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.AddTrackName();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apTrackName;
  Self.Operations.addStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.AddJCClick();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apJCClick;
  Self.Operations.addStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.AddTrainPos();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apTrainPos;
  Self.Operations.addStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.AddText(text: string; color: SymbolColor; description: Boolean);
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.addType := apText;
  Self.Operations.addStep := TGOpStep.gosActive;
  Self.Texts.addText.Text := text;
  Self.Texts.addText.Color := color;
  Self.Texts.addText.Description := description;
end;

procedure TPanelBitmap.Move();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.moveStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.Delete();
begin
  if (Self.IsOperation()) then
    raise EOperationInProgress.Create('Právě probíhá operace!');
  Self.Operations.deleteStep := TGOpStep.gosActive;
end;

procedure TPanelBitmap.PaintMove(CursorPos: TPoint);
begin
  if (Self.Operations.addStep = TGOpStep.gosActive) then
  begin
    case (Self.Operations.addType) of
      apSymbol: Self.Symbols.PaintAddSymbol(CursorPos, CursorPos);
      apJCClick: Self.JCClick.PaintAddSymbol(CursorPos, CursorPos);
      apTrackName: Self.TrackNames.PaintAddSymbol(CursorPos, CursorPos);
      apTrainPos: Self.TrainPoss.PaintAddSymbol(CursorPos, CursorPos);
      apSeparVert: Self.SeparatorsVert.PaintAddSymbol(CursorPos, CursorPos);
      apSeparHor: Self.SeparatorsHor.PaintAddSymbol(CursorPos, CursorPos);
      apText: Self.Texts.PaintAddText(CursorPos);
    end;

  end else if (Self.Operations.addStep = TGOpStep.gosSelecting) then
  begin
    case (Self.Operations.addType) of
      apSymbol: Self.Symbols.PaintAddSymbol(Self.Operations.groupStart, CursorPos);
    end;

  end else if (Self.Operations.moveStep = TGOpStep.gosMoving) then
  begin
    Self.SeparatorsVert.PaintMoveBuffer(CursorPos);
    Self.SeparatorsHor.PaintMoveBuffer(CursorPos);
    Self.TrackNames.PaintMoveBuffer(CursorPos);
    Self.JCClick.PaintMoveBuffer(CursorPos);
    Self.TrainPoss.PaintMoveBuffer(CursorPos);
    Self.Texts.PaintMoveBuffer(CursorPos);
    Self.Symbols.PaintMoveBuffer(CursorPos);
  end;
end;

procedure TPanelBitmap.ChangeTextEvent(Sender: TObject; var popisek: TPanelLabel);
begin
  if (Assigned(Self.OnTextEdit)) then
    Self.OnTextEdit(Self, popisek);
end;

/// ////////////////////////////////////////////////////////////////////////////

function TPanelBitmap.ImportMyJOP(fn: string; ORs: TList<TOR>): string;
var f: TextFile;
  splitted, gsplitted: TStrings;
  linei, eloaded: Integer;
  g: TDictionary<Integer, string>;
const
  OFFSET_X = 0;
  OFFSET_Y = 0;
begin
  Result := 'INFO: OFFSET_X:' + IntToStr(OFFSET_X) + ', OFFSET_Y:' + IntToStr(OFFSET_Y) + #13#10;

  eloaded := 0;
  Self.mFileState := fsSaved;
  AssignFile(f, fn);
  Reset(f);
  ORs.Clear();

  g := TDictionary<Integer, string>.Create();
  splitted := TStringList.Create();
  gsplitted := TStringList.Create();
  linei := 1;
  try
    while (not Eof(f)) do
    begin
      var line: string;
      ReadLn(f, line);

      splitted.Clear();
      ExtractStringsEx([';'], [], line, splitted);

      if (splitted[0] = 'JOP') then
      begin
        Self.mPanelWidth := StrToInt(splitted[4]) + OFFSET_X;
        Self.mPanelHeight := StrToInt(splitted[5]) + OFFSET_Y;
        Self.Symbols.SetRozmery(Self.mPanelWidth, Self.mPanelHeight);
        Result := 'INFO: Width: ' + IntToStr(Self.mPanelWidth) + ', height: ' + IntToStr(Self.mPanelHeight) + #13#10;

      end else if (splitted[0] = 'G') then
      begin
        g.AddOrSetValue(StrToInt(splitted[1]), line);
      end else if (splitted[0] = 'E') then
      begin
        var X := StrToInt(splitted[5]) + OFFSET_X;
        var Y := StrToInt(splitted[6]) + OFFSET_Y;
        var popx := StrToInt(splitted[8]) + OFFSET_X;
        var popy := StrToInt(splitted[9]) + OFFSET_Y;

        if (splitted[3] = '1') then
          Self.Symbols.Bitmap[X][Y] := _S_TRACK_DET_B + StrToInt(splitted[12])
        else if (splitted[3] = '2') then
          Self.Symbols.Bitmap[X][Y] := _S_TRACK_DET_B + 6 + StrToInt(splitted[12])
        else if (splitted[3] = '3') then
          Self.Symbols.Bitmap[X][Y] := _S_BUMPER_R + StrToInt(splitted[12])
        else if (splitted[3] = '4') then
          Self.Symbols.Bitmap[X][Y] := _S_SIGNAL_B + StrToInt(splitted[12])
        else if (splitted[3] = '5') then
          Self.Symbols.Bitmap[X][Y] := _S_SIGNAL_B + 4 + StrToInt(splitted[12])
        else if (splitted[3] = '6') then
        begin
          Self.Symbols.Bitmap[X][Y] := _S_TURNOUT_B + StrToInt(splitted[12]);
          try
            Self.texts.Add(Point(popx, popy), splitted[7], scWhite, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else if (splitted[3] = '10') then
          Self.Symbols.Bitmap[X][Y] := _S_LOCK
        else if (splitted[3] = '7') then
        begin
          if (splitted[12] = '0') then
            Self.Symbols.Bitmap[X][Y] := _S_DERAIL_B
          else
            Self.Symbols.Bitmap[X][Y] := _S_DERAIL_B + 1;
          try
            Self.texts.Add(Point(popx, popy), splitted[7], scWhite, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else if (splitted[3] = '11') then
        begin
          var Width := StrToInt(splitted[15]);
          for var i := 0 to Width - 1 do
            Self.Symbols.Bitmap[X + i][Y] := _S_PLATFORM_B + StrToInt(splitted[12])
        end else if (splitted[3] = '20') then
          Self.Symbols.Bitmap[X][Y] := _S_LINKER_B
        else if (splitted[3] = '12') then
          Self.Symbols.Bitmap[X][Y] := _S_DISC_TRACK
        else if (splitted[3] = '21') then
          Self.Symbols.Bitmap[X][Y] := _S_LINKER_TRAIN
        else if (splitted[3] = '30') then
        begin
          var gref := StrToInt(splitted[17]);
          gsplitted.Clear();
          ExtractStringsEx([';'], [], g[gref], gsplitted);

          var OblR: TOR := TOR.Create();
          OblR.Name := gsplitted[7];
          OblR.id := gsplitted[9];
          OblR.ShortName := gsplitted[9];
          OblR.Poss.DK := Point(X, Y);
          OblR.Poss.Queue := Point(OblR.Poss.DK.X + 10, OblR.Poss.DK.Y);
          OblR.Poss.Time := Point(OblR.Poss.DK.X + 10, OblR.Poss.DK.Y + 1);

          if ((gsplitted[12] = '0') and (gsplitted[13] = '0')) then
          begin
            popx := StrToInt(gsplitted[10]) + OFFSET_X;
            popy := StrToInt(gsplitted[11]) + OFFSET_Y;
          end else begin
            popx := StrToInt(gsplitted[12]) + OFFSET_X;
            popy := StrToInt(gsplitted[13]) + OFFSET_Y;
          end;

          try
            Self.texts.Add(Point(popx, popy), OblR.Name, scWhite, false);
          except
            Result := Result + 'WARN: nepodařilo se přidat název OŘ ' + OblR.Name + #13#10;
          end;
          ORs.Add(OblR);
        end else if (splitted[3] = '50') then
        begin
          var Height := StrToInt(splitted[15]);
          for var i := 0 to Height - 1 do
            Self.Symbols.Bitmap[X][Y + i] := _S_CROSSING
        end else if (splitted[3] = '80') then
        begin
          try
            Self.texts.Add(Point(X, Y), splitted[7], scGray, false);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + splitted[7] + #13#10;
          end;
        end else begin
          Result := Result + 'WARN: řádek ' + IntToStr(linei) + ': nerozpoznaný typ bloku: ' + splitted[3] + #13#10;
          Dec(eloaded);
        end;

        if ((splitted[3] = '0') or (splitted[3] = '1')) then
        begin
          if (StrToBoolDef(splitted[22], false)) then
            Self.SeparatorsVert.Add(Point(X, Y));
          if (StrToBoolDef(splitted[23], false)) then
            Self.SeparatorsHor.Add(Point(X, Y));
          if (StrToBoolDef(splitted[35], false)) then
          begin
            Self.trackNames.Add(Point(X, Y));
            Self.JCClick.Add(Point(X - 1, Y));
            Self.JCClick.Add(Point(X + 1, Y));
          end;
        end;
        if ((splitted[3] = '4') or (splitted[3] = '5') or (splitted[3] = '10')) then
        begin
          var gref := StrToInt(splitted[17]);
          gsplitted.Clear();
          ExtractStringsEx([';'], [], g[gref], gsplitted);
          if ((gsplitted[12] = '0') and (gsplitted[13] = '0')) then
          begin
            popx := StrToInt(gsplitted[10]) + OFFSET_X;
            popy := StrToInt(gsplitted[11]) + OFFSET_Y;
          end else begin
            popx := StrToInt(gsplitted[12]) + OFFSET_X;
            popy := StrToInt(gsplitted[13]) + OFFSET_Y;
          end;
          try
            Self.texts.Add(Point(popx, popy), gsplitted[8], scGray, true);
          except
            Result := Result + 'WARN: nepodařilo se přidat popisek bloku ' + gsplitted[8] + #13#10;
          end;
        end;

        Inc(eloaded);
      end;

      Inc(linei);
    end;
  finally
    splitted.Free();
    gsplitted.Free();
    g.Free();
  end;

  Close(f);
  Result := Result + 'INFO: Načteno ' + IntToStr(eloaded) + ' E řádků.' + #13#10;
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TPanelBitmap.DeleteMouseUp(CursorPos: TPoint);
begin
  if ((Self.Operations.isGroup) or ((Self.Operations.groupStart <> _INACTIVE_POINT))) then
  begin
    // group active
    case (Self.Operations.deleteStep) of
      TGOpStep.gosActive: // upper left point selected
        begin
          Self.Operations.groupStart := CursorPos;
          Self.Operations.deleteStep := TGOpStep.gosSelecting;
        end;
      TGOpStep.gosSelecting: // lower right point selected
        begin
          if ((Self.Operations.groupStart.X > CursorPos.X) or (Self.Operations.groupStart.Y > CursorPos.Y)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          Self.Symbols.Delete(Self.Operations.groupStart, CursorPos);
          Self.TrackNames.Delete(Self.Operations.groupStart, CursorPos);
          Self.JCClick.Delete(Self.Operations.groupStart, CursorPos);
          Self.TrainPoss.Delete(Self.Operations.groupStart, CursorPos);
          Self.Texts.Delete(Self.Operations.groupStart, CursorPos);
          // intentionally delete separators
          Self.SeparatorsVert.Delete(Self.Operations.groupStart, CursorPos);
          Self.SeparatorsHor.Delete(Self.Operations.groupStart, CursorPos);

          Self.Operations.groupStart := _INACTIVE_POINT;

          // flick a little
          Self.Operations.deleteStep := TGOpStep.gosNone;
          Self.Show();
          Sleep(50);
          Self.Operations.deleteStep := TGOpStep.gosActive;
          Self.Show();
        end; // case 2
    end;

  end else begin
    // group not active

    if (Self.Mode = dmBitmap) then
    begin
      Self.Texts.Delete(CursorPos, CursorPos);
      var deleted: Boolean := False;
      deleted := deleted or Self.TrackNames.Delete(CursorPos, CursorPos);
      deleted := deleted or Self.JCClick.Delete(CursorPos, CursorPos);
      deleted := deleted or Self.TrainPoss.Delete(CursorPos, CursorPos);
      if (not deleted) then
        Self.Symbols.Delete(CursorPos, CursorPos);
    end else if (Self.Mode = dmSepVert) then
    begin
      Self.SeparatorsVert.Delete(CursorPos, CursorPos);
    end else if (Self.Mode = dmSepHor) then
    begin
      Self.SeparatorsHor.Delete(CursorPos, CursorPos);
    end;

    // flick a little
    Self.Operations.deleteStep := TGOpStep.gosNone;
    Self.Show();
    Sleep(50);
    Self.Operations.deleteStep := TGOpStep.gosActive;
    Self.Show();
  end; // else (Self.Group.IsGroup)
end;

/// ////////////////////////////////////////////////////////////////////////////

function TPanelBitmap.IsObject(pos: TPoint): Boolean;
begin
  case (Self.Mode) of
    dmBitmap:
      Result := (Self.Symbols.GetSymbol(pos) <> _NO_SYMBOL) or (Self.TrackNames.IsObject(pos)) or
        (Self.JCClick.IsObject(pos)) or (Self.TrainPoss.IsObject(pos)) or (Self.Texts.GetTextI(pos) <> -1);
    dmSepVert:
      Result := Self.SeparatorsVert.IsObject(pos);
    dmSepHor:
      Result := Self.SeparatorsHor.IsObject(pos);
  else
    Result := False;
  end;
end;

function TPanelBitmap.CompensateMode(pos: TPoint): TPoint;
begin
  Result := pos;
  if (Self.Mode = dmSepVert) then
    Result.X := Result.X + (_SYMBOL_WIDTH div 2)
  else if (Self.Mode = dmSepHor) then
    Result.Y := Result.Y + (_SYMBOL_HEIGHT div 2);
end;

/// ////////////////////////////////////////////////////////////////////////////

function TPanelBitmap.CursorAddMoveDelete(CursorPos: TPoint): TCursorDraw;
begin
  Result.color := TCursorColor.ccDefault;
  Result.Pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  if ((Self.Operations.addStep = TGOpStep.gosActive) or (Self.Operations.moveStep = TGOpStep.gosActive) or (Self.Operations.deleteStep = TGOpStep.gosActive)) then
  begin
    Result.pos2.X := CursorPos.X * _SYMBOL_WIDTH;
    Result.pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;
    if (Self.IsObject(CursorPos)) then
      Result.color := TCursorColor.ccOnObject
    else
      Result.color := TCursorColor.ccActiveOperation;
  end
  else if ((Self.Operations.addStep = TGOpStep.gosSelecting) or (Self.Operations.moveStep = TGOpStep.gosSelecting) or (Self.Operations.deleteStep = TGOpStep.gosSelecting)) then
  begin
    if (Self.Operations.isGroup) then
    begin
      Result.pos1.X := Self.Operations.groupStart.X * _SYMBOL_WIDTH;
      Result.pos1.Y := Self.Operations.groupStart.Y * _SYMBOL_HEIGHT;
    end;
    Result.color := TCursorColor.ccOnObject;
  end
  else if ((Self.Operations.addStep = TGOpStep.gosMoving) or (Self.Operations.moveStep = TGOpStep.gosMoving) or (Self.Operations.deleteStep = TGOpStep.gosMoving)) then
  begin
    Result.color := TCursorColor.ccOnObject;
    if (Self.operations.isGroup) then
    begin
      Result.pos1.X := (CursorPos.X - Self.Operations.moveSize.X + 1) * _SYMBOL_WIDTH;
      Result.pos1.Y := (CursorPos.Y - Self.Operations.moveSize.Y + 1) * _SYMBOL_HEIGHT;
    end;
  end;

  Result.pos1 := Self.CompensateMode(Result.pos1);
  Result.pos2 := Self.CompensateMode(Result.pos2);
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TPanelBitmap.MoveMouseUp(CursorPos: TPoint);
begin
  if ((Self.Operations.isGroup) or ((Self.Operations.groupStart <> _INACTIVE_POINT))) then
  begin
    // group active
    case (Self.Operations.moveStep) of
      TGOpStep.gosActive: // upper left point selected
        begin
          Self.Operations.groupStart := CursorPos;
          Self.Operations.moveStep := TGOpStep.gosSelecting;
        end;
      TGOpStep.gosSelecting: // lower right point selected
        begin
          if ((Self.Operations.groupStart.X > CursorPos.X) or (Self.Operations.groupStart.Y > CursorPos.Y)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          Self.Symbols.MoveDrag(Self.Operations.groupStart, CursorPos);
          Self.TrackNames.MoveDrag(Self.Operations.groupStart, CursorPos);
          Self.JCClick.MoveDrag(Self.Operations.groupStart, CursorPos);
          Self.TrainPoss.MoveDrag(Self.Operations.groupStart, CursorPos);
          Self.Texts.MoveDrag(Self.Operations.groupStart, CursorPos);
          // intentionally move separators
          Self.SeparatorsVert.MoveDrag(Self.Operations.groupStart, CursorPos);
          Self.SeparatorsHor.MoveDrag(Self.Operations.groupStart, CursorPos);

          Self.Operations.moveSize := Point(CursorPos.X-Self.Operations.groupStart.X+1, CursorPos.Y-Self.Operations.groupStart.Y+1);
          Self.Operations.moveStep := TGOpStep.gosMoving;
        end;
      TGOpStep.gosMoving: // target position reached
        begin
          if ((not Self.Symbols.CanMoveDrop(CursorPos)) or (not Self.TrackNames.CanMoveDrop(CursorPos)) or
              (not Self.JCClick.CanMoveDrop(CursorPos)) or (not Self.TrainPoss.CanMoveDrop(CursorPos)) or
              (not Self.SeparatorsVert.CanMoveDrop(CursorPos)) or (not Self.SeparatorsHor.CanMoveDrop(CursorPos)) or
              (not Self.Texts.CanMoveDrop(CursorPos))) then
            raise EInvalidPosition.Create('Pozice je již obsazená!');

          Self.Symbols.MoveDrop(CursorPos);
          Self.TrackNames.MoveDrop(CursorPos);
          Self.JCClick.MoveDrop(CursorPos);
          Self.TrainPoss.MoveDrop(CursorPos);
          Self.Texts.MoveDrop(CursorPos);
          Self.SeparatorsVert.MoveDrop(CursorPos);
          Self.SeparatorsHor.MoveDrop(CursorPos);

          // flick a little
          Self.Operations.groupStart := _INACTIVE_POINT;
          Self.Operations.moveStep := TGOpStep.gosNone;
          Self.Show();
          Sleep(50);
          Self.Operations.moveStep := TGOpStep.gosActive;
          Self.Show();
        end; // case 2
    end;

  end else begin
    // group not active

    if (Self.Operations.moveStep = TGOpStep.gosActive) then
    begin
      var dragged: Boolean := False;
      case (Self.Mode) of
        dmBitmap:
          begin
            if (not dragged) then
              dragged := dragged or Self.TrackNames.MoveDrag(CursorPos, CursorPos);
            if (not dragged) then
              dragged := dragged or Self.JCClick.MoveDrag(CursorPos, CursorPos);
            if (not dragged) then
              dragged := dragged or Self.TrainPoss.MoveDrag(CursorPos, CursorPos);
            if (not dragged) then
              dragged := dragged or Self.Texts.MoveDrag(CursorPos, CursorPos);
            if (not dragged) then
              dragged := dragged or Self.Symbols.MoveDrag(CursorPos, CursorPos);
          end;
        dmSepVert:
          dragged := Self.SeparatorsVert.MoveDrag(CursorPos, CursorPos);
        dmSepHor:
          dragged := Self.SeparatorsHor.MoveDrag(CursorPos, CursorPos);
      end;

      if (not dragged) then
        raise EEmptyField.Create('Na pozici nic není!');

      Self.Operations.groupStart := CursorPos; // rest will be done in "group active branch"
      Self.Operations.moveSize := Point(1, 1);
      Self.Operations.moveStep := TGOpStep.gosMoving;
    end;
  end; // else (Self.Group.IsGroup)
end;

/// ////////////////////////////////////////////////////////////////////////////

procedure TPanelBitmap.AddMouseUp(CursorPos: TPoint);
begin
  if (((Self.Operations.isGroup) or ((Self.Operations.groupStart <> _INACTIVE_POINT))) and (Self.Operations.addType = apSymbol)) then
  begin
    // group active
    case (Self.Operations.addStep) of
      TGOpStep.gosActive: // upper left point selected
        begin
          Self.Operations.groupStart := CursorPos;
          Self.Operations.addStep := TGOpStep.gosSelecting;
        end;
      TGOpStep.gosSelecting: // lower right point selected
        begin
          if ((Self.Operations.groupStart.X > CursorPos.X) or (Self.Operations.groupStart.Y > CursorPos.Y)) then
            raise EInvalidPosition.Create('Výběr musí být zleva doprava a shora dolů!');

          for var x: Integer := Self.Operations.groupStart.X to CursorPos.X do
            for var y: Integer := Self.Operations.groupStart.Y to CursorPos.Y do
              if (Self.IsConflictSymbolPlace(Point(x, y))) then
                raise ENonemptyField.Create('Na pozici je již symbol!');

          Self.Symbols.Add(Self.Operations.groupStart, CursorPos, Self.Symbols.addSymbol);

          Self.Operations.groupStart := _INACTIVE_POINT;
          Self.Operations.addStep := TGOpStep.gosNone;
          Self.Show();
          Sleep(50);
          Self.Operations.addStep := TGOpStep.gosActive;
          Self.Show();
        end;
    end;

  end else begin
    // group not active

    if (Self.Operations.addStep = TGOpStep.gosActive) then
    begin
      case (Self.Operations.addType) of
        apSymbol:
          begin
            if (Self.IsConflictSymbolPlace(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.Symbols.Add(CursorPos, Self.Symbols.addSymbol);
          end;
        apJCClick:
          begin
            if (Self.IsConflictVectorPlace(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.JCClick.Add(CursorPos);
          end;
        apTrackName:
          begin
            if (Self.IsConflictVectorPlace(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.TrackNames.Add(CursorPos);
          end;
        apTrainPos:
          begin
            if (Self.IsConflictVectorPlace(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.TrainPoss.Add(CursorPos);
          end;
        apSeparVert:
          begin
            if (Self.SeparatorsVert.IsObject(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.SeparatorsVert.Add(CursorPos);
          end;
        apSeparHor:
          begin
            if (Self.SeparatorsHor.IsObject(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.SeparatorsHor.Add(CursorPos);
          end;
        apText:
          begin
            if (Self.IsConflictTextPlace(CursorPos)) then
              raise ENonemptyField.Create('Na pozici je již symbol!');
            Self.Texts.Add(CursorPos);
          end;
      end;

      // flick a little
      Self.Operations.groupStart := _INACTIVE_POINT;
      Self.Operations.addStep := TGOpStep.gosNone;
      Self.Show();
      Sleep(50);
      Self.Operations.addStep := TGOpStep.gosActive;
      Self.Show();
    end;
  end; // else (Self.Group.IsGroup)
end;

/// ////////////////////////////////////////////////////////////////////////////

end.// unit
