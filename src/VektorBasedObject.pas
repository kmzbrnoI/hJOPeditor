unit VektorBasedObject;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global;

const
  _MAX_DATA = 64;
  _Symbol_Sirka = 8;
  _Symbol_Vyska = 12;

type

 TSeparType = (stNone, stVert, stHor);

 TVBOData=record
  Count:Integer;
  Data:array [0..(_MAX_DATA*2)] of Byte;
 end;

 TVBO=class
  private
    Data:record
      Data:array [0.._MAX_DATA-1] of TPoint;
      Count:Byte;
     end;
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

    function AddToStructure(Position:TPoint):Byte;
    function DeleteFromStructure(Position:TPoint):Byte;

    function Adding(Position:TPoint):Byte;
    function Moving(Position:TPoint):Byte;
    function Deleting(Position:TPoint):Byte;
  public
   FOnShow : TNEvent;
   FIsSymbol : TPosAskEvent;
   FNullOperations : TNEvent;
   FMoveActivate : TNEvent;
   FDeleteActivate : TNEvent;
   FOPAsk : TOpAskEvent;

    constructor Create(DrawCanvas:TCanvas;SymbolIL:TImageList;SymbolIndex:Integer;Separ:TSeparType = stNone);

    function GetObject(Position:TPoint):SmallInt;

    procedure Paint;
    procedure PaintMove(KurzorPos:TPoint);
    function PaintCursor(CursorPos:TPoint):TCursorDraw;

    procedure SetLoadedData(LoadData:TVBOData);
    function GetSaveData:TVBOData;

    procedure Reset;
    procedure Escape;

    function MouseUp(Position:TPoint;Button:TMouseButton):Byte;

    function Add:Byte;
    function Move:Byte;
    function Delete:Byte;

    property AddKrok:Byte read Operations.FAddKrok;
    property MoveKrok:Byte read Operations.FMoveKrok;
    property DeleteKrok:Byte read Operations.FDeleteKrok;
    property Count:Byte read Data.Count;

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
 Self.DrawObject.Canvas       := DrawCanvas;
 Self.DrawObject.SymbolIL     := SymbolIL;
 Self.DrawObject.SymbolIndex  := SymbolIndex;
 Self.Separ                   := Separ;

 Self.Reset;
end;//function

procedure TVBO.Reset;
begin
 Self.Data.Count := 0;

 Self.Escape;
end;//procedure

procedure TVBO.Escape;
begin
 Self.Operations.FAddKrok    := 0;
 Self.Operations.FMoveKrok   := 0;
 Self.Operations.FDeleteKrok := 0;
end;//procedure

//pridani objektu
function TVBO.AddToStructure(Position:TPoint):Byte;
begin
 if ((Position.X < 0) or (Position.Y < 0)) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu
 if (Self.GetObject(Position) <> -1) then
  begin
   Result := 2;
   Exit;
  end;//if (Self.Bitmap[aPos.X,aPos.Y] <> -1)
 if (Self.Data.Count >= _MAX_DATA) then
  begin
   Result := 3;
   Exit;
  end;

 Self.Data.Count := Self.Data.Count + 1;
 Self.Data.Data[Self.Data.Count-1] := Position;

 Result := 0;
end;//function

//smazani objektu
function TVBO.DeleteFromStructure(Position:TPoint):Byte;
var OIndex,i:Integer;
begin
 if ((Position.X < 0) or (Position.Y < 0)) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu

 OIndex := Self.GetObject(Position);
 if (OIndex = -1) then
  begin
   Result := 2;
   Exit;
  end;

 //samotne mazani
 for i := OIndex to Self.Data.Count-2 do
  begin
   Self.Data.Data[i] := Self.Data.Data[i+1];
  end;//for i

 Self.Data.Count := Self.Data.Count - 1;

 Result := 0;
end;//function

//zjisteni, zda-li je na dane pozici objekt, popr. jeho index v poli
function TVBO.GetObject(Position:TPoint):SmallInt;
var i:Integer;
begin
 Result := -1;

 for i := 0 to Self.Data.Count-1 do
  begin
   if ((Self.Data.Data[i].X = Position.X) and (Self.Data.Data[i].Y = Position.Y)) then
    begin
     Result := i;
     Break;
    end;//if ((Self.Data.Data[i].X = Position.X) and (Self.Data.Data[i].Y = Position.Y))
  end;//for i
end;//function

//nacteni surovych dat do struktur
procedure TVBO.SetLoadedData(LoadData:TVBOData);
var i:Integer;
begin
 Self.Data.Count := LoadData.Count div 2;
 for i := 0 to Self.Data.Count-1 do
  begin
   Self.Data.Data[i].X := LoadData.Data[(i*2)];
   Self.Data.Data[i].Y := LoadData.Data[(i*2)+1];
  end;//for i
end;//function

//ziskani surovych dat zapisovanych do souboru z dat programu
function TVBO.GetSaveData:TVBOData;
var i:Integer;
begin
 Result.Count := (Self.Data.Count*2)+1;

 Result.Data[0] := Self.Data.Count;

 for i := 0 to Self.Data.Count-1 do
  begin
   Result.Data[(i*2)+1] := Self.Data.Data[i].X;
   Result.Data[(i*2)+2] := Self.Data.Data[i].Y;
  end;//for i
end;//procedure

procedure TVBO.Paint;
var i:Integer;
    posun:TPoint;
begin
 if (Self.Separ = stVert) then posun := Point(_Symbol_Sirka div 2, 0)
 else if (Self.Separ = stHor) then posun := Point(0, _Symbol_Vyska div 2)
 else posun := Point(0, 0);
 
 for i := 0 to Self.Data.Count-1 do
   Self.DrawObject.SymbolIL.Draw(Self.DrawObject.Canvas,(Self.Data.Data[i].X * _Symbol_Sirka) + posun.X,
   (Self.Data.Data[i].Y * _Symbol_Vyska) + posun.Y, Self.DrawObject.SymbolIndex);
end;//procedure

function TVBO.Adding(Position:TPoint):Byte;
begin
 //kontrola obsazenosti
 if (Assigned(FIsSymbol)) then
  begin
   if (FIsSymbol(Position)) then
    begin
     Result := 4;
     Exit;
    end;
  end else begin
   if (Self.GetObject(Position) <> -1) then
    begin
     Result := 4;
     Exit;
    end;
  end;//else Assigned(FIsOperation)

 if (Assigned(FNullOperations)) then FNullOperations;

 if (Self.AddToStructure(Position) <> 0) then
  begin
   //chyba externi funkce
   Result := 3;
   Exit;
  end;//if (aReturn <> 0)

 //znovu pripraveni dosazeni objektu
 Self.Operations.FAddKrok := 0;
 if Assigned(FOnShow) then
  begin
   FOnShow;
   Sleep(50);
  end;

 Self.Add;

 Result := 0;
end;//procedure

function TVBO.Moving(Position:TPoint):Byte;
begin
 case (Self.Operations.FMoveKrok) of
  1:begin
     //kontrola obsazenosti
     if (Self.GetObject(Position) = -1) then
      begin
       Result := 2;
       Exit;
      end;

     if (Assigned(FNullOperations)) then FNullOperations;

     //prevzeti objektu a smazani puvodnich objektu
     if (Self.DeleteFromStructure(Position) <> 0) then
      begin
       //chyba externi funkce
       Result := 3;
       Exit;
      end;//if (PanelBitmap.DeleteSymbol(Position) <> 0)

     Self.Operations.FMoveKrok := 2;
    end;//case 1
  2:begin
     //kontrola obsazenosti
     if (Assigned(FIsSymbol)) then
      begin
       if (FIsSymbol(Position)) then
        begin
         Result := 4;
         Exit;
        end;
      end else begin
       if (Self.GetObject(Position) <> -1) then
        begin
         Result := 4;
         Exit;
        end;
      end;//else Assigned(FIsOperation)

     if (Self.AddToStructure(Position) <> 0) then
      begin
       //chyba externi funkce
       Result := 3;
       Exit;
      end;//if (aReturn <> 0)

     Self.Operations.FMoveKrok := 0;

     //znovu pripraveni dosazeni objektu
     if Assigned(FOnShow) then
      begin
       FOnShow;
       Sleep(50);
      end;
     if (Self.Separ = stNone) then
      begin if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate;
      end else begin Self.Move; end;
    end;//case 3
  end;//case

 Result := 0;
end;//procedure

function TVBO.Deleting(Position:TPoint):Byte;
begin
 //kontrola obsazenosti
 if (Self.GetObject(Position) = -1) then
  begin
   Result := 2;
   Exit;
  end;

 if (Assigned(FNullOperations)) then FNullOperations;

 if (Self.DeleteFromStructure(Position) <> 0) then
  begin
   //chyba externi funkce
   Result := 3;
   Exit;
  end;//if (aReturn <> 0)

 Self.Operations.FDeleteKrok := 0;

 //znovu pripraveni dosazeni objektu
 if Assigned(FOnShow) then
  begin
   FOnShow;
   Sleep(50);
  end;

 if (Self.Separ = stNone) then
  begin if (Assigned(Self.FDeleteActivate)) then Self.FDeleteActivate;
  end else begin Self.Delete; end;

 Result := 0;
end;//procedure

function TVBO.Add:Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.FAddKrok := 1;

 if Assigned(FOnShow) then FOnShow;

 Result := 0;
end;//function

function TVBO.Move:Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.FMoveKrok := 1;

 Result := 0;
end;//function

function TVBO.Delete:Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.FAddKrok > 0) or (Self.Operations.FMoveKrok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.FDeleteKrok := 1;

 Result := 0;
end;//function

function TVBO.MouseUp(Position:TPoint;Button:TMouseButton):Byte;
begin
 Result := 0;

 if (Button = mbLeft) then
  begin
   if (Self.Operations.FAddKrok    > 0) then Result := Self.Adding(Position);
   if (Self.Operations.FMoveKrok   > 0) then Result := Self.Moving(Position);
   if (Self.Operations.FDeleteKrok > 0) then Result := Self.Deleting(Position);
  end;//mbLeft
end;//procedure

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
end;//procedure

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
end;//function

end.//unit

