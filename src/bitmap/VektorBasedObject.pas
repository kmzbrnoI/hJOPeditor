unit VektorBasedObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Generics.Collections, symbolHelper;

const
  _MAX_DATA = 256;

type

 TSeparType = (stNone, stVert, stHor);

 TVBOData = record
  Count:Integer;
  Data:array [0..(_MAX_DATA*2)] of Byte;
 end;

 EInvalidPosition = class(Exception);
 ENonemptyField = class(Exception);
 EEmptyField = class(Exception);
 EMaxReached = class(Exception);
 ENoSymbol = class(Exception);
 EOperationInProgress = class(Exception);

 TVBO = class
  private
    Data:TList<TPoint>;

    DrawObject:record
     Canvas:TCanvas;
     SymbolIL:TImageList;
     SymbolIndex:Integer;
    end;

    Operations:record
     FAddKrok:Byte;
     FMoveKrok:Byte;
     FDeleteKrok:Byte;
    end;

    Separ:TSeparType;

    procedure Add(Position:TPoint); overload;
    procedure Delete(Position:TPoint); overload;

    procedure Adding(Position:TPoint);
    procedure Moving(Position:TPoint);
    procedure Deleting(Position:TPoint);

    function GetCount():Integer;
    procedure CheckOpInProgressAndExcept();

  public
   FOnShow : TNEvent;
   FIsSymbol : TPosAskEvent;
   FNullOperations : TNEvent;
   FMoveActivate : TNEvent;
   FDeleteActivate : TNEvent;
   FOPAsk : TOpAskEvent;

    constructor Create(DrawCanvas:TCanvas;SymbolIL:TImageList;SymbolIndex:Integer;Separ:TSeparType = stNone);
    destructor Destroy(); override;

    function GetObject(Position:TPoint):Integer;

    procedure Paint;
    procedure PaintMove(KurzorPos:TPoint);
    function PaintCursor(CursorPos:TPoint):TCursorDraw;

    procedure SetLoadedData(LoadData:TVBOData);
    function GetSaveData:TVBOData;

    procedure Reset;
    procedure Escape;

    procedure MouseUp(Position:TPoint;Button:TMouseButton);

    procedure Add(); overload;
    procedure Move();
    procedure Delete(); overload;

    property AddKrok:Byte read Operations.FAddKrok;
    property MoveKrok:Byte read Operations.FMoveKrok;
    property DeleteKrok:Byte read Operations.FDeleteKrok;
    property Count:Integer read GetCount;

    property OnShow: TNEvent read FOnShow write FOnShow;
    property IsSymbol: TPosAskEvent read FIsSymbol write FIsSymbol;
    property IsOp: TOpAskEvent read FOPAsk write FOPAsk;
    property OnNullOperations: TNEvent read FNullOperations write FNullOperations;
    property OnMoveActivate: TNEvent read FMoveActivate write FMoveActivate;
    property OnDeleteActivate: TNEvent read FDeleteActivate write FDeleteActivate;
 end;//TVBO


implementation

constructor TVBO.Create(DrawCanvas:TCanvas;SymbolIL:TImageList;SymbolIndex:Integer;Separ:TSeparType);
begin
 inherited Create();

 Self.Data := TList<TPoint>.Create();
 Self.DrawObject.Canvas       := DrawCanvas;
 Self.DrawObject.SymbolIL     := SymbolIL;
 Self.DrawObject.SymbolIndex  := SymbolIndex;
 Self.Separ                   := Separ;

 Self.Reset();
end;

destructor TVBO.Destroy();
begin
 Self.Data.Free();
 inherited;
end;

procedure TVBO.Reset();
begin
 Self.Data.Count := 0;

 Self.Escape;
end;

procedure TVBO.Escape();
begin
 Self.Operations.FAddKrok    := 0;
 Self.Operations.FMoveKrok   := 0;
 Self.Operations.FDeleteKrok := 0;
end;

procedure TVBO.Add(Position:TPoint);
begin
 if ((Position.X < 0) or (Position.Y < 0)) then
   raise EInvalidPosition.Create('Neplatná pozice!');
 if (Self.GetObject(Position) <> -1) then
   raise ENonemptyField.Create('Na pozici je již symbol!');
 if (Self.Data.Count >= _MAX_DATA) then
   raise EMaxReached.Create('Dosaženo maximálního počtu symbolů!');

 Self.Data.Add(Position);
end;

procedure TVBO.Delete(Position:TPoint);
var OIndex:Integer;
begin
 if ((Position.X < 0) or (Position.Y < 0)) then
   raise EInvalidPosition.Create('Neplatná pozice!');

 OIndex := Self.GetObject(Position);
 if (OIndex = -1) then
   raise ENoSymbol.Create('Na této pozici není žádný symbol!');

 Self.Data.Remove(Position);
end;

//zjisteni, zda-li je na dane pozici objekt, popr. jeho index v poli
function TVBO.GetObject(Position:TPoint):Integer;
var i:Integer;
begin
 Result := -1;

 for i := 0 to Self.Data.Count-1 do
   if ((Self.Data[i].X = Position.X) and (Self.Data[i].Y = Position.Y)) then
     Exit(i);
end;

//nacteni surovych dat do struktur
procedure TVBO.SetLoadedData(LoadData:TVBOData);
var i:Integer;
begin
 Self.Data.Clear();
 for i := 0 to (LoadData.Count div 2)-1 do
   Self.Data.Add(Point(LoadData.Data[(i*2)], LoadData.Data[(i*2)+1]));
end;

//ziskani surovych dat zapisovanych do souboru z dat programu
function TVBO.GetSaveData():TVBOData;
var i:Integer;
begin
 Result.Count := (Self.Data.Count*2)+1;

 Result.Data[0] := Self.Data.Count;

 for i := 0 to Self.Data.Count-1 do
  begin
   Result.Data[(i*2)+1] := Self.Data[i].X;
   Result.Data[(i*2)+2] := Self.Data[i].Y;
  end;//for i
end;

procedure TVBO.Paint();
var i:Integer;
    posun:TPoint;
begin
 if (Self.Separ = stVert) then posun := Point(_Symbol_Sirka div 2, 0)
 else if (Self.Separ = stHor) then posun := Point(0, _Symbol_Vyska div 2)
 else posun := Point(0, 0);
 
 for i := 0 to Self.Data.Count-1 do
   Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas,(Self.Data[i].X * _Symbol_Sirka) + posun.X,
   (Self.Data[i].Y * _Symbol_Vyska) + posun.Y, Self.DrawObject.SymbolIndex);
end;

procedure TVBO.Adding(Position:TPoint);
begin
 //kontrola obsazenosti
 if (Assigned(FIsSymbol)) then
  begin
   if (FIsSymbol(Position)) then
     raise ENonemptyField.Create('Na této pozici je již symbol!');
  end else begin
   if (Self.GetObject(Position) <> -1) then
     raise ENonemptyField.Create('Na této pozici je již symbol!');
  end;

 if (Assigned(FNullOperations)) then FNullOperations();

 Self.Add(Position);

 //znovu pripraveni dosazeni objektu
 Self.Operations.FAddKrok := 0;
 if (Assigned(FOnShow)) then
  begin
   FOnShow();
   Sleep(50);
  end;

 Self.Add();
end;

procedure TVBO.Moving(Position:TPoint);
begin
 case (Self.Operations.FMoveKrok) of
  1:begin
     if (Self.GetObject(Position) = -1) then
       Exit();

     if (Assigned(FNullOperations)) then FNullOperations;
     Self.Delete(Position);

     Self.Operations.FMoveKrok := 2;
    end;//case 1

  2:begin
     if (Assigned(FIsSymbol)) then
      begin
       if (FIsSymbol(Position)) then
         raise ENonemptyField.Create('Na této pozici je již symbol!');
      end else begin
       if (Self.GetObject(Position) <> -1) then
         raise ENonemptyField.Create('Na této pozici je již symbol!');
      end;

     Self.Add(Position);
     Self.Operations.FMoveKrok := 0;

     //znovu pripraveni dosazeni objektu
     if Assigned(FOnShow) then
      begin
       FOnShow();
       Sleep(50);
      end;

     if (Self.Separ = stNone) then
      begin if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate();
      end else begin Self.Move(); end;
    end;//case 2
  end;//case
end;

procedure TVBO.Deleting(Position:TPoint);
begin
 //kontrola obsazenosti
 if (Self.GetObject(Position) = -1) then
   Exit();

 if (Assigned(FNullOperations)) then FNullOperations();

 Self.Delete(Position);
 Self.Operations.FDeleteKrok := 0;

 //znovu pripraveni dosazeni objektu
 if Assigned(FOnShow) then
  begin
   FOnShow();
   Sleep(50);
  end;

 if (Self.Separ = stNone) then
  begin if (Assigned(Self.FDeleteActivate)) then Self.FDeleteActivate();
  end else begin Self.Delete(); end;
end;

procedure TVBO.Add();
begin
 Self.CheckOpInProgressAndExcept();
 Self.Operations.FAddKrok := 1;
 if Assigned(FOnShow) then FOnShow();
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

procedure TVBO.MouseUp(Position:TPoint;Button:TMouseButton);
begin
 if (Button = mbLeft) then
  begin
   if (Self.Operations.FAddKrok > 0) then Self.Adding(Position)
   else if (Self.Operations.FMoveKrok > 0) then Self.Moving(Position)
   else if (Self.Operations.FDeleteKrok > 0) then Self.Deleting(Position);
  end;
end;

//zabyva se vykreslovanim posouvanych objektu v oddelovacich
procedure TVBO.PaintMove(KurzorPos:TPoint);
var posun:TPoint;
begin
 if (Self.Separ = stVert) then posun := Point(_Symbol_Sirka div 2, 0)
 else if (Self.Separ = stHor) then posun := Point(0, _Symbol_Vyska div 2)
 else posun := Point(0, 0);

 //pridavani, posouvani
 if ((Self.Operations.FAddKrok = 1) or (Self.Operations.FMoveKrok = 2)) then
  begin
   Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas,
      KurzorPos.X * _Symbol_Sirka + posun.X,
      KurzorPos.Y * _Symbol_Vyska + posun.Y,
      Self.DrawObject.SymbolIndex);
  end;//if (Self.Oddelovace.AddKrok = 1)
end;

//vykresleni kurzoru - vraci data PIXELECH!
function TVBO.PaintCursor(CursorPos:TPoint):TCursorDraw;
begin
 Result.Color := 0;

 Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;
 Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

 if (Self.Operations.FAddKrok = 1) then Result.Color := 2;
 if ((Self.Operations.FMoveKrok = 1) or (Self.Operations.FDeleteKrok = 1)) then if (Self.GetObject(CursorPos) = -1) then Result.Color := 1 else Result.Color := 2;
 if (Self.Operations.FMoveKrok = 2) then Result.Color := 2;
end;

function TVBO.GetCount():Integer;
begin
 Result := Self.Data.Count;
end;

procedure TVBO.CheckOpInProgressAndExcept();
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
     raise EOperationInProgress.Create('Právě probíhá operace!');
  end else begin
   if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
     raise EOperationInProgress.Create('Právě probíhá operace!');
  end;
end;

end.//unit

