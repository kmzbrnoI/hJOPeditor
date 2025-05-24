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

    Separ: TSeparType;
    MoveBuf: TList<TPoint>;

    mIsConflict: TPosAskEvent;

    function GetCount(): Integer;
    
    function IsConflict(pos: TPoint): Boolean;
    function SeparOffset(): TPoint;

  public
    constructor Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer;
                       SymColor: SymbolColor; separ: TSeparType = stNone);
    destructor Destroy(); override;

    function MoveDrag(pos1: TPoint; pos2: TPoint): Boolean; // returns if anything dragged
    procedure MoveDrop(pos: TPoint); // pos = right bottom position
    function CanMoveDrop(pos: TPoint): Boolean; // pos = right bottom position
    procedure Delete(Position: TPoint); overload;

    function GetObjectI(Position: TPoint): Integer;
    function IsObject(Position: TPoint): Boolean;

    procedure Paint();
    procedure PaintMoveBuffer(pos: TPoint);
    procedure PaintAddSymbol(pos1: TPoint; pos2: TPoint);

    procedure LoadBpnl(var f: File; fileVersion: Byte);
    procedure WriteBpnl(var f: File);

    procedure Clear();

    procedure Add(Position: TPoint); overload;
    procedure Add(pos1: TPoint; pos2: TPoint); overload;
    function Delete(pos1: TPoint; pos2: TPoint): Boolean; overload; // returns if anything deleted

    property count: Integer read GetCount;

    property QIsConflict: TPosAskEvent read mIsConflict write mIsConflict;
  end; // TVBO

implementation

uses ReliefBitmap;

constructor TVBO.Create(DrawCanvas: TCanvas; SymbolIL: TImageList; symbolIndex: Integer; symColor: SymbolColor; separ: TSeparType);
begin
  inherited Create();

  Self.Data := TList<TPoint>.Create();
  Self.DrawObject.Canvas := DrawCanvas;
  Self.DrawObject.SymbolIL := SymbolIL;
  Self.DrawObject.SymbolIndex := symbolIndex;
  Self.DrawObject.SymColor := symColor;
  Self.Separ := separ;

  Self.MoveBuf := TList<TPoint>.Create();

  Self.Clear();
end;

destructor TVBO.Destroy();
begin
  Self.Data.Free();
  Self.MoveBuf.Free();
  inherited;
end;

procedure TVBO.Clear();
begin
  Self.Data.Clear();
  Self.MoveBuf.Clear();
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
var buf: array [0 .. 3] of Byte;
begin
  Self.data.Clear();

  var countSize: Integer := 1;
  if (fileVersion >= $41) then
    countSize := 2;

  TPanelBitmap.BlockReadOrException(f, buf, countSize, 'hlavička vektorového bloku');
  var count: Integer := buf[0];
  if (countSize = 2) then
    count := (count shl 8) or buf[1];

  for var i: Integer := 0 to count-1 do
  begin
    if (fileVersion >= $42) then
    begin
      TPanelBitmap.BlockReadOrException(f, buf, 4, 'data vektorového bloku');
      Self.data.Add(Point((buf[0] shl 8) or buf[1], (buf[2] shl 8) or buf[3]));
    end else begin
      TPanelBitmap.BlockReadOrException(f, buf, 2, 'data vektorového bloku');
      Self.data.Add(Point(buf[0], buf[1]));
    end;
  end;
end;

procedure TVBO.WriteBpnl(var f: File);
var buf: array [0 .. 3] of Byte;
begin
  buf[0] := Hi(Self.data.Count);
  buf[1] := Lo(Self.data.Count);
  BlockWrite(f, buf, 2);
  for var point: TPoint in Self.data do
  begin
    buf[0] := Hi(point.X);
    buf[1] := Lo(point.X);
    buf[2] := Hi(point.Y);
    buf[3] := Lo(point.Y);
    BlockWrite(f, buf, 4);
  end;
end;

procedure TVBO.Paint();
begin
  var offset: TPoint := Self.SeparOffset();
  for var i := 0 to Self.data.Count - 1 do
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, (Self.data[i].X * _SYMBOL_WIDTH) + offset.X,
      (Self.data[i].Y * _SYMBOL_HEIGHT) + offset.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
end;

function TVBO.SeparOffset(): TPoint;
begin
  if (Self.separ = stVert) then
    Result := Point(_SYMBOL_WIDTH div 2, 0)
  else if (Self.separ = stHor) then
    Result := Point(0, _SYMBOL_HEIGHT div 2)
  else
    Result := Point(0, 0);
end;

procedure TVBO.PaintMoveBuffer(pos: TPoint);
begin
  var offset: TPoint := Self.SeparOffset();
  for var item: TPoint in Self.MoveBuf do
    Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, ((pos.X+item.X) * _SYMBOL_WIDTH) + offset.X,
      ((pos.Y+item.Y) * _SYMBOL_HEIGHT) + offset.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
end;

procedure TVBO.PaintAddSymbol(pos1: TPoint; pos2: TPoint);
begin
  var offset: TPoint := Self.SeparOffset();
  for var x: Integer := pos1.X to pos2.X do
    for var y: Integer := pos1.Y to pos2.Y do
      Self.DrawObject.SymbolIL.Draw(Self.DrawObject.canvas, (x * _SYMBOL_WIDTH) + offset.X,
        (y * _SYMBOL_HEIGHT) + offset.Y, SymbolIndex(Self.DrawObject.symbolIndex, Self.DrawObject.symColor));
end;

procedure TVBO.Add(pos1: TPoint; pos2: TPoint);
begin
  for var x: Integer := pos1.X to pos2.X do
    for var y: Integer := pos1.Y to pos2.Y do
      Self.Add(Point(x, y));
end;

function TVBO.MoveDrag(pos1: TPoint; pos2: TPoint): Boolean;
begin
  Result := False;
  Self.MoveBuf.Clear();
  for var x := pos1.X to pos2.X do
  begin
    for var y := pos1.Y to pos2.Y do
    begin
      var i: Integer := Self.GetObjectI(Point(x, y));
      if (i > -1) then
      begin
        Self.MoveBuf.Add(Point(x-pos2.X, y-pos2.Y));
        Self.Data.Delete(i);
        Result := True;
      end;
    end;
  end;
end;

procedure TVBO.MoveDrop(pos: TPoint);
begin
  for var item: TPoint in Self.MoveBuf do
    Self.Add(Point(pos.X+item.X, pos.Y+item.Y));
  Self.MoveBuf.Clear();
end;

function TVBO.CanMoveDrop(pos: TPoint): Boolean;
begin
  Result := True;
  for var item: TPoint in Self.MoveBuf do
    if (Self.IsConflict(Point(item.X+pos.X, item.Y+pos.Y))) then
      Exit(False);
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

function TVBO.GetCount(): Integer;
begin
  Result := Self.data.Count;
end;

function TVBO.IsConflict(pos: TPoint): Boolean;
begin
  if (Assigned(Self.mIsConflict)) then
    Result := Self.mIsConflict(pos)
  else
    Result := Self.IsObject(pos);
end;

end.// unit
