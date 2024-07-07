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
    data: TList<TPoint>;

    drawObject: record
      canvas: TCanvas;
      symbolIL: TImageList;
      symbolIndex: Integer;
      symColor: SymbolColor;
    end;

    operations: record
      FAddKrok: Byte;
      FMoveKrok: Byte;
      FDeleteKrok: Byte;
    end;

    separ: TSeparType;

    mOnShow: TNEvent;
    mIsSymbol: TPosAskEvent;
    mNullOperations: TNEvent;
    mMoveActivate: TNEvent;
    mDeleteActivate: TNEvent;
    mOPAsk: TOpAskEvent;

    procedure Adding(Position: TPoint);
    procedure Moving(Position: TPoint);
    procedure Deleting(Position: TPoint);

    function GetCount(): Integer;
    procedure CheckOpInProgressAndExcept();

  public
    constructor Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer;
                       SymColor: SymbolColor; separ: TSeparType = stNone);
    destructor Destroy(); override;

    procedure Add(Position: TPoint); overload;
    procedure Delete(Position: TPoint); overload;

    function GetObject(Position: TPoint): Integer;

    procedure Paint;
    procedure PaintMove(KurzorPos: TPoint);
    function PaintCursor(CursorPos: TPoint): TCursorDraw;

    procedure LoadBpnl(var f: File; fileVersion: Byte);
    procedure WriteBpnl(var f: File);

    procedure Clear();
    procedure Escape();

    procedure MouseUp(Position: TPoint; Button: TMouseButton);

    procedure Add(); overload;
    procedure Move();
    procedure Delete(); overload;

    property AddKrok: Byte read operations.FAddKrok;
    property MoveKrok: Byte read operations.FMoveKrok;
    property DeleteKrok: Byte read operations.FDeleteKrok;
    property Count: Integer read GetCount;

    property OnShow: TNEvent read mOnShow write mOnShow;
    property IsSymbol: TPosAskEvent read mIsSymbol write mIsSymbol;
    property IsOp: TOpAskEvent read mOPAsk write mOPAsk;
    property OnNullOperations: TNEvent read mNullOperations write mNullOperations;
    property OnMoveActivate: TNEvent read mMoveActivate write mMoveActivate;
    property OnDeleteActivate: TNEvent read mDeleteActivate write mDeleteActivate;
  end; // TVBO

implementation

constructor TVBO.Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer; symColor: SymbolColor; separ: TSeparType);
begin
  inherited Create();

  Self.data := TList<TPoint>.Create();
  Self.drawObject.canvas := DrawCanvas;
  Self.drawObject.symbolIL := SymbolIL;
  Self.drawObject.symbolIndex := symbolIndex;
  Self.drawObject.symColor := symColor;
  Self.separ := separ;

  Self.Clear();
end;

destructor TVBO.Destroy();
begin
  Self.data.Free();
  inherited;
end;

procedure TVBO.Clear();
begin
  Self.data.Clear();
  Self.Escape();
end;

procedure TVBO.Escape();
begin
  Self.operations.FAddKrok := 0;
  Self.operations.FMoveKrok := 0;
  Self.operations.FDeleteKrok := 0;
end;

procedure TVBO.Add(Position: TPoint);
begin
  if ((Position.X < 0) or (Position.Y < 0)) then
    raise EInvalidPosition.Create('Neplatná pozice!');
  if (Self.GetObject(Position) <> -1) then
    raise ENonemptyField.Create('Na pozici je již symbol!');
  if (Self.data.Count >= _MAX_VBO) then
    raise EMaxReached.Create('Dosaženo maximálního počtu symbolů!');

  Self.data.Add(Position);
end;

procedure TVBO.Delete(Position: TPoint);
begin
  if ((Position.X < 0) or (Position.Y < 0)) then
    raise EInvalidPosition.Create('Neplatná pozice!');

  var OIndex: Integer := Self.GetObject(Position);
  if (OIndex = -1) then
    raise ENoSymbol.Create('Na této pozici není žádný symbol!');

  Self.data.Remove(Position);
end;

// zjisteni, zda-li je na dane pozici objekt, popr. jeho index v poli
function TVBO.GetObject(Position: TPoint): Integer;
begin
  Result := -1;

  for var i: Integer := 0 to Self.data.Count - 1 do
    if ((Self.data[i].X = Position.X) and (Self.data[i].Y = Position.Y)) then
      Exit(i);
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
    posun := Point(_Symbol_Sirka div 2, 0)
  else if (Self.separ = stHor) then
    posun := Point(0, _Symbol_Vyska div 2)
  else
    posun := Point(0, 0);

  for var i := 0 to Self.data.Count - 1 do
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, (Self.data[i].X * _Symbol_Sirka) + posun.X,
      (Self.data[i].Y * _Symbol_Vyska) + posun.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
end;

procedure TVBO.Adding(Position: TPoint);
begin
  // kontrola obsazenosti
  if (Assigned(mIsSymbol)) then
  begin
    if (mIsSymbol(Position)) then
      raise ENonemptyField.Create('Na této pozici je již symbol!');
  end else begin
    if (Self.GetObject(Position) <> -1) then
      raise ENonemptyField.Create('Na této pozici je již symbol!');
  end;

  if (Assigned(mNullOperations)) then
    mNullOperations();

  Self.Add(Position);

  // znovu pripraveni dosazeni objektu
  Self.Operations.FAddKrok := 0;
  if (Assigned(mOnShow)) then
  begin
    mOnShow();
    Sleep(50);
  end;

  Self.Add();
end;

procedure TVBO.Moving(Position: TPoint);
begin
  case (Self.Operations.FMoveKrok) of
    1:
      begin
        if (Self.GetObject(Position) = -1) then
          Exit();

        if (Assigned(mNullOperations)) then
          mNullOperations();
        Self.Delete(Position);

        Self.Operations.FMoveKrok := 2;
      end; // case 1

    2:
      begin
        if (Assigned(mIsSymbol)) then
        begin
          if (mIsSymbol(Position)) then
            raise ENonemptyField.Create('Na této pozici je již symbol!');
        end else begin
          if (Self.GetObject(Position) <> -1) then
            raise ENonemptyField.Create('Na této pozici je již symbol!');
        end;

        Self.Add(Position);
        Self.Operations.FMoveKrok := 0;

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

procedure TVBO.Deleting(Position: TPoint);
begin
  // kontrola obsazenosti
  if (Self.GetObject(Position) = -1) then
    Exit();

  if (Assigned(mNullOperations)) then
    mNullOperations();

  Self.Delete(Position);
  Self.Operations.FDeleteKrok := 0;

  // znovu pripraveni dosazeni objektu
  if Assigned(mOnShow) then
  begin
    mOnShow();
    Sleep(50);
  end;

  if (Self.separ = stNone) then
  begin
    if (Assigned(Self.mDeleteActivate)) then
      Self.mDeleteActivate();
  end else begin
    Self.Delete();
  end;
end;

procedure TVBO.Add();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.FAddKrok := 1;
  if Assigned(mOnShow) then
    mOnShow();
end;

procedure TVBO.Move();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.FMoveKrok := 1;
end;

procedure TVBO.Delete();
begin
  Self.CheckOpInProgressAndExcept();
  Self.Operations.FDeleteKrok := 1;
end;

procedure TVBO.MouseUp(Position: TPoint; Button: TMouseButton);
begin
  if (Button = mbLeft) then
  begin
    if (Self.Operations.FAddKrok > 0) then
      Self.Adding(Position)
    else if (Self.Operations.FMoveKrok > 0) then
      Self.Moving(Position)
    else if (Self.Operations.FDeleteKrok > 0) then
      Self.Deleting(Position);
  end;
end;

// zabyva se vykreslovanim posouvanych objektu v oddelovacich
procedure TVBO.PaintMove(KurzorPos: TPoint);
var posun: TPoint;
begin
  if (Self.separ = stVert) then
    posun := Point(_Symbol_Sirka div 2, 0)
  else if (Self.separ = stHor) then
    posun := Point(0, _Symbol_Vyska div 2)
  else
    posun := Point(0, 0);

  // pridavani, posouvani
  if ((Self.Operations.FAddKrok = 1) or (Self.Operations.FMoveKrok = 2)) then
  begin
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, KurzorPos.X * _Symbol_Sirka + posun.X,
      KurzorPos.Y * _Symbol_Vyska + posun.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
  end; // if (Self.Oddelovace.AddKrok = 1)
end;

// vykresleni kurzoru - vraci data PIXELECH!
function TVBO.PaintCursor(CursorPos: TPoint): TCursorDraw;
begin
  Result.Color := 0;

  Result.Pos1.X := CursorPos.X * _Symbol_Sirka;
  Result.Pos1.Y := CursorPos.Y * _Symbol_Vyska;
  Result.Pos2.X := CursorPos.X * _Symbol_Sirka;
  Result.Pos2.Y := CursorPos.Y * _Symbol_Vyska;

  if (Self.Operations.FAddKrok = 1) then
    Result.Color := 2;
  if ((Self.Operations.FMoveKrok = 1) or (Self.Operations.FDeleteKrok = 1)) then
    if (Self.GetObject(CursorPos) = -1) then
      Result.Color := 1
    else
      Result.Color := 2;
  if (Self.Operations.FMoveKrok = 2) then
    Result.Color := 2;
end;

function TVBO.GetCount(): Integer;
begin
  Result := Self.data.Count;
end;

procedure TVBO.CheckOpInProgressAndExcept();
begin
  if (Assigned(Self.mOPAsk)) then
  begin
    if (Self.mOPAsk) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
    if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
      raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.// unit
