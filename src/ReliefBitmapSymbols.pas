unit ReliefBitmapSymbols;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, ReliefText, VektorBasedObject, Global, RPConst;

const
  _MAX_WIDTH   = 256;
  _MAX_HEIGHT  = 256;
  _MAX_MOVE = 64;
  _Symbol_Sirka = 8;
  _Symbol_Vyska = 12;
  _Symbol_Uvazka_Spr = 45;
  _Symbol_Uvazka     = 43;

  //zde je definovano, jaky symbol se ma vykreslovat jakou barvou (mimo separatoru)
  _Bitmap_DrawColors:array [0..60] of Byte = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,6,6,1,1,1,1,1,1,1,1,1,1,7,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);

type

  TBSData=record
   Width,Height:Integer;
   Data:array [0..((_MAX_WIDTH*_MAX_HEIGHT)-1+2)] of Byte;  //+2 - 2 na rozmery
  end;

  TBitmapSymbols=class
    private

     Panel:record
      Width,Height:Integer;
     end;

     DrawObject:record
       Canvas:TCanvas;
       IL:TImageList;
     end;

     Operations:record
      Add:record
       Krok:Byte;
       Symbol:Integer;
      end;
      FDeleteKrok:Byte;
      Move:record
       Krok:Byte;
       aWidth,aHeight:Byte;
       Symbols:array [0.._MAX_MOVE-1,0.._MAX_MOVE-1] of ShortInt;
      end;//Move
      Group:record
       IsGroup:Boolean;
       Start:TPoint;
      end;
     end;//Operations

      procedure PaintSymbol(IL:TImageList; X,Y:Integer; index:Integer; color:Integer);

      function AddToStructure(aPos:TPoint;SymbolID:Integer):Byte;
      function DeleteFromStructure(aPos:TPoint):Byte;

      function Adding(Position:TPoint):Byte;
      function Moving(Position:TPoint):Byte;
      function Deleting(Position:TPoint):Byte;

      function ControlCursorPos(Pos1,Pos2:TPoint):Boolean;

    public
     FOnShow : TNEvent;
     FIsSymbol : TPosAskEvent;
     FNullOperations : TNEvent;
     FMoveActivate : TNEvent;
     FDeleteActivate : TNEvent;
     FOPAsk : TOpAskEvent;

     Bitmap:array [0.._MAX_WIDTH-1,0.._MAX_HEIGHT-1] of ShortInt;

      constructor Create(IL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer);

      procedure SetRozmery(Width,Height:Integer);

      function MouseUp(Position:TPoint;Button:TMouseButton):Byte;

      function GetSymbol(Position:TPoint):ShortInt;

      procedure Escape(Group:boolean);
      procedure Reset;

      procedure Paint;
      procedure PaintBitmapMove(KurzorPos:TPoint);
      function PaintCursor(CursorPos:TPoint):TCursorDraw;

      function Add(SymbolID:Integer):Byte;
      function Move:Byte;
      function Delete:Byte;

      procedure SetLoadedData(LoadData:TBSData);
      function GetSaveData:TBSData;


      property AddKrok:Byte read Operations.Add.Krok;
      property MoveKrok:Byte read Operations.Move.Krok;
      property DeleteKrok:Byte read Operations.FDeleteKrok;
      property Group:Boolean read Operations.Group.IsGroup write Operations.Group.IsGroup;

      property OnShow: TNEvent read FOnShow write FOnShow;
      property IsSymbol: TPosAskEvent read FIsSymbol write FIsSymbol;
      property IsOp: TOpAskEvent read FOPAsk write FOPAsk;
      property OnNullOperations: TNEvent read FNullOperations write FNullOperations;
      property OnMoveActivate: TNEvent read FMoveActivate write FMoveActivate;
      property OnDeleteActivate: TNEvent read FDeleteActivate write FDeleteActivate;
  end;//class

implementation

//construcotr
constructor TBitmapSymbols.Create(IL:TImageList;DrawCanvas:TCanvas;Width,Height:Integer);
begin
 Self.DrawObject.Canvas := DrawCanvas;
 Self.DrawObject.IL     := IL;
 Self.Panel.Width       := Width;
 Self.Panel.Height      := Height;
 Self.Operations.Group.IsGroup := false;

 Self.Operations.Group.Start.X := -1;
 Self.Operations.Group.Start.Y := -1;

 Self.Reset;
end;//function

procedure TBitmapSymbols.Escape(Group:boolean);
begin
 Self.Operations.Add.Krok    := 0;
 Self.Operations.Move.Krok   := 0;
 Self.Operations.FDeleteKrok := 0;

 if (Group) then
  begin
   Self.Operations.Group.IsGroup := false;
   Self.Operations.Group.Start.X := -1;
   Self.Operations.Group.Start.Y := -1;
  end;
end;//procedure

//pridani noveho bitmapoveho symbolu
function TBitmapSymbols.AddToStructure(aPos:TPoint;SymbolID:Integer):Byte;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > Self.Panel.Width) or (aPos.Y > Self.Panel.Height)) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu
 if (Self.Bitmap[aPos.X,aPos.Y] <> -1) then
  begin
   Result := 2;
   Exit;
  end;//if (Self.Bitmap[aPos.X,aPos.Y] <> -1)

 Self.Bitmap[aPos.X,aPos.Y] := SymbolID;

 Result := 0;
end;//procedure

//smazani bitmapovaho symbolu
function TBitmapSymbols.DeleteFromStructure(aPos:TPoint):Byte;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > _MAX_WIDTH) or (aPos.Y > _MAX_HEIGHT)) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu
 if (Self.Bitmap[aPos.X,aPos.Y] = -1) then
  begin
   Result := 2;
   Exit;
  end;

 Self.Bitmap[aPos.X,aPos.Y] := -1;

 Result := 0;
end;//procedure

function TBitmapSymbols.GetSymbol(Position:TPoint):ShortInt;
begin
 if ((Position.X < 0) or (Position.Y < 0) or (Position.X > _MAX_WIDTH) or (Position.Y > _MAX_HEIGHT)) then
  begin
   Result := -2;
   Exit;
  end;

 Result := Self.Bitmap[Position.X,Position.Y];
end;//function

procedure TBitmapSymbols.Reset;
var i,j:Integer;
begin
 for i := 0 to _MAX_WIDTH-1 do
  begin
   for j := 0 to _MAX_HEIGHT-1 do Self.Bitmap[i,j] := -1;
  end;//for i

 Self.Escape(true);
end;//procedure

procedure TBitmapSymbols.Paint;
var i,j:Integer;
begin
 for i := 0 to Self.Panel.Width-1 do
   for j := 0 to Self.Panel.Height-1 do
     Self.PaintSymbol(Self.DrawObject.IL, i, j, Self.Bitmap[i,j], _Bitmap_DrawColors[Self.Bitmap[i,j]]);
end;//procedure

//nacteni surovych dat do struktur
procedure TBitmapSymbols.SetLoadedData(LoadData:TBSData);
var i,j:Integer;
begin
 Self.Panel.Width  := LoadData.Width;
 Self.Panel.Height := LoadData.Height;

 for i := 0 to Self.Panel.Height-1 do
  begin
   for j := 0 to Self.Panel.Width-1 do
    begin
     Self.Bitmap[j,i] := LoadData.Data[(i*Self.Panel.Width)+j]-1;
    end;//for j
  end;//for i
end;//function

//ziskani surovych dat zapisovanych do souboru z dat programu
function TBitmapSymbols.GetSaveData:TBSData;
var i,j:Integer;
begin
 Result.Width  := Self.Panel.Width;
 Result.Height := Self.Panel.Height;

 for i := 0 to Self.Panel.Height-1 do
  begin
   for j := 0 to Self.Panel.Width-1 do
    begin
     Result.Data[(i*Self.Panel.Width)+j] := Self.Bitmap[j,i]+1;
    end;//for j
  end;//for i
end;//procedure

procedure TBitmapSymbols.SetRozmery(Width,Height:Integer);
begin
 Self.Panel.Width  := Width;
 Self.Panel.Height := Height;
end;//procedure

function TBitmapSymbols.ControlCursorPos(Pos1,Pos2:TPoint):Boolean;
begin
 Result := true;

 if (Pos1.X > Pos2.X) or (Pos1.Y > Pos2.Y) then Result := false;
end;//function

function TBitmapSymbols.Adding(Position:TPoint):Byte;
var i,j:Integer;
begin
 //overovani skupiny - 2. cast podminky pridana, kdyby nekdo po 1. kroku vypl IsGroup
 if ((Self.Operations.Group.IsGroup) or ((Self.Operations.Group.Start.X <> -1) and (Self.Operations.Group.Start.Y <> -1))) then
  begin
   //pokud je skupina
   case (Self.Operations.Add.Krok) of
    1:begin
       //pokud jsme na 1. bode
       Self.Operations.Group.Start.X := Position.X;
       Self.Operations.Group.Start.Y := Position.Y;

       if (Assigned(FNullOperations)) then FNullOperations;
       Self.Operations.Add.Krok := 2;
      end;//case 1
    2:begin
       //pokud jsme na 2. bode
       //kontrola zapornych limitu vyberu
       if (not Self.ControlCursorPos(Self.Operations.Group.Start,Position)) then
        begin
         //zleva doprava a zhora dolu
         Result := 1;
         Exit;
        end;

       //kontrola obsazenosti
       for i := Self.Operations.Group.Start.X to Position.X do
        begin
          for j := Self.Operations.Group.Start.Y to Position.Y do
           begin
            if Assigned(FIsSymbol) then
             begin
              if (FIsSymbol(Point(i,j))) then
               begin
                Result := 4;
                Exit;
               end;
             end else begin
              if (Self.GetSymbol(Point(i,j)) <> -1) then
               begin
                //na pozici je jiz objekt
                Result := 4;
                Exit;
               end;
             end;//else Assigned(FIsOperation)
           end;//for j
        end;//for i

       if (Assigned(FNullOperations)) then FNullOperations;

       //dosazeni vlastnich objektu
       for i := Self.Operations.Group.Start.X to Position.X do
        begin
         for j := Self.Operations.Group.Start.Y to Position.Y do Self.AddToStructure(Point(i,j),Self.Operations.Add.Symbol);
        end;//for i

       Self.Operations.Group.Start.X := -1;
       Self.Operations.Group.Start.Y := -1;

       //znovu pripraveni dosazeni objektu
       Self.Operations.Add.Krok := 0;
       if Assigned(FOnShow) then
        begin
         FOnShow;
         Sleep(50);
        end;
       Self.Add(Self.Operations.Add.Symbol);
      end;//case 2
   end;//case
  end else begin
   //pokud neni skupina

   if Assigned(FIsSymbol) then
    begin
     if (FIsSymbol(Position)) then
      begin
       Result := 4;
       Exit;
      end;
    end else begin
     if (Self.GetSymbol(Position) <> -1) then
      begin
       //na pozici je jiz objekt
       Result := 4;
       Exit;
      end;
    end;//else Assigned(FIsOperation)

   if (Assigned(FNullOperations)) then FNullOperations;

   if (Self.AddToStructure(Position,Self.Operations.Add.Symbol) <> 0) then
    begin
     //chyba externi funkce
     Result := 3;
     Exit;
    end;//if (aReturn <> 0)

   //znovu pripraveni dosazeni objektu
   Self.Operations.Add.Krok := 0;
   if Assigned(FOnShow) then
    begin
     FOnShow;
     Sleep(50);
    end;
   Self.Add(Self.Operations.Add.Symbol);
  end;//else (Self.Group.IsGroup)

 Result := 0;
end;//procedure

function TBitmapSymbols.Moving(Position:TPoint):Byte;
var i,j:Integer;
begin
 //overovani skupiny - 2. cast podminky pridana, kdyby nekdo po 1. kroku vypl IsGroup
 if ((Self.Operations.Group.IsGroup) or ((Self.Operations.Group.Start.X <> -1) and (Self.Operations.Group.Start.Y <> -1))) then
  begin
   //pokud je skupina
   case (Self.Operations.Move.Krok) of
    1:begin
       Self.Operations.Group.Start.X := Position.X;
       Self.Operations.Group.Start.Y := Position.Y;

       if (Assigned(FNullOperations)) then FNullOperations;
       Self.Operations.Move.Krok := 2;
      end;//case 1
    2:begin
       //kontrola zapornych limitu vyberu
       if (not Self.ControlCursorPos(Self.Operations.Group.Start,Position)) then
        begin
         //zleva doprava a zhora dolu
         Result := 1;
         Exit;
        end;

       //kontrola limitu pole
       if (((Position.X - Self.Operations.Group.Start.X + 1) > _MAX_MOVE) or (Position.Y - Self.Operations.Group.Start.Y + 1 > _MAX_MOVE)) then
        begin
         //presah limitu
         Result := 5;
         Exit;
        end;


       //prevzeti objektu do .Symbols[] a smazani puvodnich objektu
       Self.Operations.Move.aWidth  := Position.X - Self.Operations.Group.Start.X + 1;
       Self.Operations.Move.aHeight := Position.Y - Self.Operations.Group.Start.Y + 1;

       for i := Self.Operations.Group.Start.X to Position.X do
        begin
         for j := Self.Operations.Group.Start.Y to Position.Y do
          begin
           Self.Operations.Move.Symbols[i-Self.Operations.Group.Start.X,j-Self.Operations.Group.Start.Y] := Self.GetSymbol(Point(i,j));
           Self.DeleteFromStructure(Point(i,j));
          end;//for j
        end;//for i

       Self.Operations.Move.Krok := 3;
      end;//case 2
    3:begin
       //kontrola obsazenosti
       for i := 0 to Self.Operations.Move.aWidth-1 do
        begin
         for j := 0 to Self.Operations.Move.aHeight-1 do
          begin
           if (Assigned(FIsSymbol)) then
            begin
             if (FIsSymbol(Point(i+(Position.X - Self.Operations.Move.aWidth)+1,j+(Position.Y - Self.Operations.Move.aHeight)+1))) then
              begin
               Result := 4;
               Exit;
              end;
            end else begin
             if ((Self.GetSymbol(Point(i+(Position.X - Self.Operations.Move.aWidth)+1,j+(Position.Y - Self.Operations.Move.aHeight)+1)) <> -1)) then
              begin
               //na pozici je jiz objekt
               Result := 4;
               Exit;
              end;//if PanelBitmap...
            end;//else (Assigned(FIsOperation))
          end;//for j
        end;//for i

       for i := 0 to Self.Operations.Move.aWidth-1 do
        begin
         for j := 0 to Self.Operations.Move.aHeight-1 do
          begin
           Self.AddToStructure(Point(i+(Position.X - Self.Operations.Move.aWidth)+1,j+(Position.Y - Self.Operations.Move.aHeight)+1),Self.Operations.Move.Symbols[i,j]);
           Self.Operations.Move.Symbols[i,j] := 0;
          end;//for j
        end;//for i

       Self.Operations.Group.Start.X := -1;
       Self.Operations.Group.Start.Y := -1;

       Self.Operations.Move.aWidth  := 0;
       Self.Operations.Move.aHeight := 0;
       Self.Operations.Move.Krok    := 0;

       //znovu pripraveni dosazeni objektu
       if Assigned(FOnShow) then
        begin
         FOnShow;
         Sleep(50);
        end;
       if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate;
      end;//case 3
   end;//case

  end else begin
   //pokud neni skupina
   case (Self.Operations.Move.Krok) of
    1:begin
       //prevzeti objektu do .Symbols[] a smazani puvodnich objektu
       Self.Operations.Move.aWidth  := 1;
       Self.Operations.Move.aHeight := 1;

       if (Self.GetSymbol(Position) = -1) then
        begin
         //neni s cim pohybovat
         Result := 2;
         Exit;
        end;

       if (Assigned(FNullOperations)) then FNullOperations;

       Self.Operations.Move.Symbols[0,0] := Self.GetSymbol(Position);
       if (Self.DeleteFromStructure(Position) <> 0) then
        begin
         //chyba externi funkce
         Result := 3;
         Exit;
        end;//if (PanelBitmap.DeleteSymbol(Position) <> 0)

       Self.Operations.Move.Krok := 2;
      end;//case 1
    2:begin
       //kontrola obsazenosti pozice
       if (Assigned(FIsSymbol)) then
        begin
         if (FIsSymbol(Position)) then
          begin
           Result := 4;
           Exit;
          end;
        end else begin
         if (Self.GetSymbol(Position) <> -1) then
          begin
           Result := 4;
           Exit;
          end;//if PanelBitmap...
        end;//else (Assigned(FIsOperation))

       if (Self.AddToStructure(Position,Self.Operations.Move.Symbols[0,0]) <> 0) then
        begin
         //chyba externi funkce
         Result := 3;
         Exit;
        end;//if (aReturn <> 0)

       Self.Operations.Move.aWidth  := 0;
       Self.Operations.Move.aHeight := 0;
       Self.Operations.Move.Krok    := 0;

       //znovu pripraveni dosazeni objektu
       if Assigned(FOnShow) then
        begin
         FOnShow;
         Sleep(50);
        end;
       if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate;
      end;//case 3
    end;//case
  end;//else ((Self.Group.IsGroup) or ((Self.Group.Start.X <> -1) and (Self.Group.Start.Y <> -1)))

 Result := 0;
end;//procedure

function TBitmapSymbols.Deleting(Position:TPoint):Byte;
var i,j:Integer;
begin
 Result := 0;

 if ((Self.Operations.Group.IsGroup) or ((Self.Operations.Group.Start.X <> -1) and (Self.Operations.Group.Start.Y <> -1))) then
  begin
   //pokud je skupina
   case (Self.Operations.fDeleteKrok) of
    1:begin
       //pokud jsme na 1. bode
       Self.Operations.Group.Start.X := Position.X;
       Self.Operations.Group.Start.Y := Position.Y;

       if (Assigned(Self.FNullOperations)) then Self.FNullOperations;
       Self.Operations.fDeleteKrok := 2;
      end;//case 1
    2:begin
       //kontrola zapornych limitu vyberu
       if (not Self.ControlCursorPos(Self.Operations.Group.Start,Position)) then
        begin
         //zleva doprava a zhora dolu
         Result := 1;
         Exit;
        end;

       //pokud jsme na 2. bode
       for i := Self.Operations.Group.Start.X to Position.X do
        begin
         for j := Self.Operations.Group.Start.Y to Position.Y do Self.DeleteFromStructure(Point(i,j));
        end;//for i

       Self.Operations.Group.Start.X := -1;
       Self.Operations.Group.Start.Y := -1;

       Self.Operations.fDeleteKrok := 0;
       if Assigned(FOnShow) then
        begin
         FOnShow;
         Sleep(50);
        end;
       if (Assigned(Self.FDeleteActivate)) then Self.FDeleteActivate;
      end;//case 2
   end;//case
  end else begin
   //pokud neni skupina
   if (Self.GetSymbol(Position) = -1) then
    begin
     //neni s cim operovat
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
   if Assigned(FOnShow) then
    begin
     FOnShow;
     Sleep(50);
    end;
   if (Assigned(Self.FDeleteActivate)) then Self.FDeleteActivate;
  end;//else (Self.Group.IsGroup)
end;//procedure

function TBitmapSymbols.MouseUp(Position:TPoint;Button:TMouseButton):Byte;
begin
 Result := 0;

 if (Button = mbLeft) then
  begin
   if (Self.Operations.Add.Krok    > 0) then Result := Self.Adding(Position);
   if (Self.Operations.Move.Krok   > 0) then Result := Self.Moving(Position);
   if (Self.Operations.FDeleteKrok > 0) then Result := Self.Deleting(Position);
  end;//mbLeft
end;//procedure

function TBitmapSymbols.Add(SymbolID:Integer):Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.Add.Krok > 0) or (Self.Operations.Move.Krok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.Add.Krok   := 1;
 Self.Operations.Add.Symbol := SymbolID;

 if Assigned(FOnShow) then FOnShow;
 Result := 0;
end;//function

function TBitmapSymbols.Move:Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.Add.Krok > 0) or (Self.Operations.Move.Krok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.Move.Krok := 1;

 Result := 0;
end;//function

function TBitmapSymbols.Delete:Byte;
begin
 if (Assigned(Self.FOPAsk)) then
  begin
   if (Self.FOPAsk) then
    begin
     Result := 1;
     Exit;
    end;
  end else begin
   if ((Self.Operations.Add.Krok > 0) or (Self.Operations.Move.Krok > 0) or (Self.Operations.FDeleteKrok > 0)) then
    begin
     Result := 1;
     Exit;
    end;
  end;//else (Assigned(Self.FOPAsk))

 Self.Operations.FDeleteKrok := 1;

 Result := 0;
end;//function

//zabyva se vykreslovanim posouvanych objektu v bitmape
procedure TBitmapSymbols.PaintBitmapMove(KurzorPos:TPoint);
var i,j:Integer;
    aPos:TPoint;
begin
 //pohyb
 if (Self.Operations.Move.Krok > 1) then
  begin
   if ((Self.Operations.Group.Start.X = -1) and (Self.Operations.Group.Start.Y = -1)) then
    begin
     //presun 1 objektu
     Self.PaintSymbol(Self.DrawObject.IL, KurzorPos.X, KurzorPos.Y, Self.Operations.Move.Symbols[0,0], _Bitmap_DrawColors[Self.Operations.Move.Symbols[0,0]]);
    end else begin
     //presun skupiny
     if (Self.Operations.Move.Krok > 2) then
      begin
       for i := 0 to Self.Operations.Move.aWidth-1 do
        begin
          for j := 0 to Self.Operations.Move.aHeight-1 do
           begin
            aPos.X := i + KurzorPos.X - Self.Operations.Move.aWidth + 1;
            aPos.Y := j + KurzorPos.Y - Self.Operations.Move.aHeight + 1;
            if (Self.GetSymbol(aPos) = -1) then
              Self.PaintSymbol(Self.DrawObject.IL, aPos.X, aPos.Y, Self.Operations.Move.Symbols[i,j], _Bitmap_DrawColors[Self.Operations.Move.Symbols[i,j]]);
           end;//for j
        end;//for i
      end;//if (Self.Move.Krok > 2)
    end;//else ((Self.Group.Start.X = -1) and (Self.Group.Start.Y = -1))
  end;//if Self.Move.Krok > 0

 //pridavani
 if (Self.Operations.Add.Krok > 0) then
  begin
   if ((Self.Operations.Group.Start.X = -1) and (Self.Operations.Group.Start.Y = -1)) then
    begin
     //presun 1 objektu
     Self.PaintSymbol(Self.DrawObject.IL, KurzorPos.X, KurzorPos.Y, Self.Operations.Add.Symbol, _Bitmap_DrawColors[Self.Operations.Add.Symbol]);
    end else begin
     //presun skupiny
     for i := Self.Operations.Group.Start.X to KurzorPos.X do
      begin
       for j := Self.Operations.Group.Start.Y to KurzorPos.Y do
        begin
         if (Self.GetSymbol(Point(i,j)) = -1) then
          Self.PaintSymbol(Self.DrawObject.IL, i, j, Self.Operations.Add.Symbol, _Bitmap_DrawColors[Self.Operations.Add.Symbol]);
        end;//for j
      end;//for i
    end;//else ((Self.Group.Start.X = -1) and (Self.Group.Start.Y = -1))
  end;//if Self.Add > -1
end;//procedure

//vykresleni kurzoru - vraci data PIXELECH!
function TBitmapSymbols.PaintCursor(CursorPos:TPoint):TCursorDraw;
var SymbolI:Integer;
begin
 Result.Color := 0;
 Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;
 Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

 if (Self.Operations.Add.Krok = 1) then
  begin
   Result.Color := 2;
  end;
 if (Self.Operations.Add.Krok = 2) or (Self.Operations.Move.Krok = 2) or (Self.Operations.FDeleteKrok = 2) then
  begin
   if (Self.Operations.Group.IsGroup) then
    begin
     Result.Pos1.X := (Self.Operations.Group.Start.X)*_Symbol_Sirka;
     Result.Pos1.Y := (Self.Operations.Group.Start.Y)*_Symbol_Vyska;
    end;//else IsGroup

   Result.Color := 2;
  end;
 if ((Self.Operations.Move.Krok = 1) or (Self.Operations.FDeleteKrok = 1)) then
  begin
   SymbolI := Self.GetSymbol(CursorPos);
   Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
   Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

   if (SymbolI = -1) then Result.Color := 1 else Result.Color := 2;
  end;
 if (Self.Operations.Move.Krok = 3) then
  begin
   //zde uz musi byt skupina - jinak by nebylo mozne skocit do kroku 2
   Result.Color := 2;

   if (Self.Operations.Group.IsGroup) then
    begin
     Result.Pos1.X := (CursorPos.X-Self.Operations.Move.aWidth+1)*_Symbol_Sirka;
     Result.Pos1.Y := (CursorPos.Y-Self.Operations.Move.aHeight+1)*_Symbol_Vyska;
    end;//if (Self.Operations.Group.IsGroup)
  end;
end;//function

// tato funkce ve skutenocnosti jen vykresluje veci z ImageListu
// zaroven ale resi specialni pripady velikosti, jako jsou naprikald uvazky, ci seznam souprav u uvazky
procedure TBitmapSymbols.PaintSymbol(IL:TImageList; X,Y:Integer; index:Integer; color:Integer);
begin
 // specialni pripady: symbol seznamu souprav uvazky
 if (index = _Symbol_Uvazka_Spr) then
  begin
   Self.DrawObject.Canvas.Pen.Color   := clYellow;
   Self.DrawObject.Canvas.Brush.Color := clBlack;

   Self.DrawObject.Canvas.Rectangle(X*_Symbol_Sirka, Y*_Symbol_Vyska, (X+_Uvazka_Spr_Sirka)*_Symbol_Sirka - 1, Y*_Symbol_Vyska + _Symbol_Vyska - 1);
  end;

 // specialni pripad: k trati vykreslujeme druhy symbol do paru
 if (index = _Symbol_Uvazka) then
   Self.DrawObject.IL.Draw(Self.DrawObject.Canvas,(X+1)*_Symbol_Sirka, Y*_Symbol_Vyska,((_Symbol_Uvazka+1)*10)+color);

 IL.Draw(Self.DrawObject.Canvas, X*_Symbol_Sirka, Y*_Symbol_Vyska, (index*10) + color);
end;//procedure

end.//unit
