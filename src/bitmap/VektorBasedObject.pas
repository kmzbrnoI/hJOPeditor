unit VektorBasedObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Generics.Collections, symbolHelper, Types, ReliefCommon;

const
  _MAX_VBO = 65535;

type

  TSeparType = (stNone, stVert, stHor);

  EInvalidPosition = class(Exception);
  ENonemptyField = class(Exception);
  EEmptyField = class(Exception);
  EMaxReached = class(Exception);
  ENoSymbol = class(Exception);
  EOperationInProgress = class(Exception);

  TVBO = class
  private
    Data: TList<TPoint>;

    DrawObject: record
      Canvas: TCanvas;
      SymbolIL: TImageList;
      SymbolIndex: Integer;
      SymColor: SymbolColor;
    end;

    Operations: record
      AddStep: TGOpStep;
      MoveStep: TGOpStep;
    end;

    Separ: TSeparType;

    mOnShow: TNEvent;
    mIsSymbol: TPosAskEvent;
    mNullOperations: TNEvent;
    mMoveActivate: TNEvent;
    mOPAsk: TOpAskEvent;

    procedure Adding(Position: TPoint);
    procedure Moving(Position: TPoint);

    function GetCount(): Integer;
    procedure CheckOpInProgressAndExcept();

  public
    constructor Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer;
                       SymColor: SymbolColor; separ: TSeparType = stNone);
    destructor Destroy(); override;

    procedure Add(Position: TPoint); overload;
    procedure Delete(Position: TPoint); overload;

    function GetObjectI(Position: TPoint): Integer;
    function IsObject(Position: TPoint): Boolean;

    procedure Paint();
    procedure PaintMove(KurzorPos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure LoadBpnl(var f: File; fileVersion: Byte);
    procedure WriteBpnl(var f: File);

    procedure Clear();
    procedure Escape();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    procedure Add(); overload;
    procedure Move();
    function Delete(pos1: TPoint; pos2: TPoint): Boolean; overload; // returns if anything deleted

    property addStep: TGOpStep read Operations.AddStep;
    property moveStep: TGOpStep read Operations.MoveStep;
    property count: Integer read GetCount;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property IsSymbol: TPosAskEvent read mIsSymbol write mIsSymbol;
    property IsOp: TOpAskEvent read mOPAsk write mOPAsk;
    property OnNullOperations: TNEvent read mNullOperations write mNullOperations;
    property OnMoveActivate: TNEvent read mMoveActivate write mMoveActivate;
  end; // TVBO

implementation

constructor TVBO.Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer; symColor: SymbolColor; separ: TSeparType);
begin
  inherited Create();

  Self.Data := TList<TPoint>.Create();
  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.SymbolIL := SymbolIL;
  Self.DrawObject.SymbolIndex := symbolIndex;
  Self.DrawObject.SymColor := symColor;
  Self.Separ := separ;

  Self.Clear();
end;

destructor TVBO.Destroy();
begin
  Self.Data.Free();
  inherited;
end;

procedure TVBO.Clear();
begin
  Self.Data.Clear();
  Self.Escape();
end;

procedure TVBO.Escape();
begin
  Self.Operations.AddStep := gosNone;
  Self.Operations.MoveStep := gosNone;
end;

procedure TVBO.Add(Position: TPoint);
begin
  if ((Position.X < 0) or (Position.Y < 0)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.IsObject(Position)) then
    raise ENonemptyField.Create('Na pozici je již symbol!');
  if (Self.Data.Count >= _MAX_VBO) then
    raise EMaxReached.Create('Dosaženo maximálního počtu symbolů!');

  Self.Data.Add(Position);
end;

procedure TVBO.Delete(Position: TPoint);
begin
  if ((Position.X < 0) or (Position.Y < 0)) then
    raise EInvalidPosition.Create('Neplatná pozice!');

  if (not Self.IsObject(Position)) then
    raise ENoSymbol.Create('Na této pozici není žádný symbol!');

  Self.data.Remove(Position);
end;

// zjisteni, zda-li je na dane pozici objekt, popr. jeho index v poli
function TVBO.GetObjectI(Position: TPoint): Integer;
begin
  Result := -1;

  for var i: Integer := 0 to Self.data.Count - 1 do
    if ((Self.data[i].X = Position.X) and (Self.data[i].Y = Position.Y)) then
      Exit(i);
end;

function TVBO.IsObject(Position: TPoint): Boolean;
begin
  Result := (Self.GetObjectI(Position) <> -1);
end;

procedure TVBO.LoadBpnl(var f: File; fileVersion: Byte);
var buf: array [0 .. 1] of Byte;
    readCount: Integer;
begin
  Self.data.Clear();

  var countSize: Integer := 1;
  if (fileVersion >= $41) then
    countSize := 2;

  BlockRead(f, buf, countSize, readCount);
  if (readCount <> countSize) then
    raise EFileLoad.Create('Chybí velikost bloku!');

  var count: Integer := buf[0];
  if (countSize = 2) then
    count := (count shl 8) or buf[1];

  for var i: Integer := 0 to count-1 do
  begin
    BlockRead(f, buf, 2, readCount);
    if (readCount <> 2) then
      raise EFileLoad.Create('Chybí data!');
    Self.data.Add(Point(buf[0], buf[1]));
  end;
end;

procedure TVBO.WriteBpnl(var f: File);
var buf: array [0 .. 1] of Byte;
begin
  buf[0] := Self.data.Count shr 8;
  buf[1] := Self.data.Count and $FF;
  BlockWrite(f, buf, 2);
  for var point: TPoint in Self.data do
  begin
    buf[0] := point.X;
    buf[1] := point.Y;
    BlockWrite(f, buf, 2);
  end;
end;

procedure TVBO.Paint();
var posun: TPoint;
begin
  if (Self.separ = stVert) then
    posun := Point(_SYMBOL_WIDTH div 2, 0)
  else if (Self.separ = stHor) then
    posun := Point(0, _SYMBOL_HEIGHT div 2)
  else
    posun := Point(0, 0);

  for var i := 0 to Self.data.Count - 1 do
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, (Self.data[i].X * _SYMBOL_WIDTH) + posun.X,
      (Self.data[i].Y * _SYMBOL_HEIGHT) + posun.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
end;

procedure TVBO.Adding(Position: TPoint);
begin
  // kontrola obsazenosti
  if (Assigned(mIsSymbol)) then
  begin
    if (mIsSymbol(Position)) then
      raise ENonemptyField.Create('Na této pozici je již symbol!');
  end else begin
    if (Self.IsObject(Position)) then
      raise ENonemptyField.Create('Na této pozici je již symbol!');
  end;

  if (Assigned(mNullOperations)) then
    mNullOperations();

  Self.Add(Position);

  // znovu pripraveni dosazeni objektu
  Self.Operations.addStep := gosNone;
  if (Assigned(mOnShow)) then
  begin
    mOnShow();
    Sleep(50);
  end;

  Self.Add();
end;

procedure TVBO.Moving(Position: TPoint);
begin
  case (Self.Operations.moveStep) of
    TGOpStep.gosActive:
      begin
        if (not Self.IsObject(Position)) then
          Exit();

        if (Assigned(mNullOperations)) then
          mNullOperations();
        Self.Delete(Position);

        Self.Operations.moveStep := gosMoving;
      end; // case 1

    TGOpStep.gosMoving:
      begin
        if (Assigned(mIsSymbol)) then
        begin
          if (mIsSymbol(Position)) then
            raise ENonemptyField.Create('Na této pozici je již symbol!');
        end else begin
          if (Self.IsObject(Position)) then
            raise ENonemptyField.Create('Na této pozici je již symbol!');
        end;

        Self.Add(Position);
        Self.Operations.moveStep := gosNone;

        // znovu pripraveni dosazeni objektu
        if Assigned(mOnShow) then
        begin
          mOnShow();
          Sleep(50);
        end;

        if (Self.separ = stNone) then
        begin
          if (Assigned(Self.mMoveActivate)) then
            Self.mMoveActivate();
        end else begin
          Self.Move();
        end;
      end; // case 2
  end; // case
end;

procedure TVBO.Add();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.addStep := gosActive;
  if Assigned(mOnShow) then
    mOnShow();
end;

procedure TVBO.Move();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.moveStep := gosActive;
end;

function TVBO.Delete(pos1: TPoint; pos2: TPoint): Boolean;
begin
  Result := False;
  for var x := pos1.X to pos2.X do
  begin
    for var y := pos1.Y to pos2.Y do
    begin
      if (Self.IsObject(Point(x, y))) then
      begin
        Self.Delete(Point(x, y));
        Result := True;
      end;
    end;
  end;
end;

procedure TVBO.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.Operations.addStep > gosNone) then
      Self.Adding(Position)
    else if (Self.Operations.moveStep > gosNone) then
      Self.Moving(Position);
  end;
end;

// zabyva se vykreslovanim posouvanych objektu v oddelovacich
procedure TVBO.PaintMove(KurzorPos: TPoint);
var posun: TPoint;
begin
  if (Self.separ = stVert) then
    posun := Point(_SYMBOL_WIDTH div 2, 0)
  else if (Self.separ = stHor) then
    posun := Point(0, _SYMBOL_HEIGHT div 2)
  else
    posun := Point(0, 0);

  // pridavani, posouvani
  if ((Self.Operations.addStep = gosActive) or (Self.Operations.moveStep = gosMoving)) then
  begin
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, KurzorPos.X * _SYMBOL_WIDTH + posun.X,
      KurzorPos.Y * _SYMBOL_HEIGHT + posun.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
  end; // if (Self.Oddelovace.AddKrok = 1)
end;

// vykresleni kurzoru - vraci data PIXELECH!
function TVBO.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  Result.color := TCursorColor.ccDefault;

  Result.Pos1.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos1.Y := CursorPos.Y * _SYMBOL_HEIGHT;
  Result.Pos2.X := CursorPos.X * _SYMBOL_WIDTH;
  Result.Pos2.Y := CursorPos.Y * _SYMBOL_HEIGHT;

  if (Self.Operations.addStep = gosActive) then
    Result.color := TCursorColor.ccOnObject;
  if (Self.Operations.moveStep = gosSelecting) then
    if (not Self.IsObject(CursorPos)) then
      Result.color := TCursorColor.ccActiveOperation
    else
      Result.color := TCursorColor.ccOnObject;
  if (Self.Operations.moveStep = gosMoving) then
    Result.color := TCursorColor.ccOnObject;
end;

function TVBO.GetCount(): Integer;
begin
  Result := Self.data.Count;
end;

procedure TVBO.CheckOpInProgressAndExcept();
begin
  if (Assigned(Self.IsOp)) then
  begin
    if (Self.IsOp) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if (Self.Operations.addStep > gosNone) or (Self.Operations.moveStep > gosNone) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.// unit
