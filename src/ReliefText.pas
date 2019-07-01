unit ReliefText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, IniFiles,
  StrUtils, Global, Menus, Forms, PGraphics, Generics.Collections;

const
 _MAX_POPISKY     = 64;
 _MAX_TEXT_LENGTH = 32;
 _MIN_TEXT_LENGTH = 1;
 _MAX_WIDTH       = 256;
 _MAX_HEIGHT      = 256;
 _Symbol_Sirka = 8;
 _Symbol_Vyska = 12;
 _Block_Length = 35;

type
 TChangeTextEvent = procedure(Sender:TObject; var Text:string;var Color:Integer) of object;

  TPopisek=record
    Position:TPoint;
    Text:string;
    Color:ShortInt;
  end;

  TPopisky=class
   private
     Data:TList<TPopisek>;

     TextMenu:TPopupMenu;
     MenuPosition:TPoint;
     Graphics:TPanelGraphics;

     DrawObject:record
      Canvas:TCanvas;
      TextIL:TImageList;
     end;

     Operations:record
      FAddKrok:Byte;
      FMoveKrok:Byte;
      FDeleteKrok:Byte;
      TextProperties:record
       Text:string;
       Color:Integer;
      end;
     end;//Operations

    function AddToStructure(aPos:TPoint;aText:string;aColor:ShortInt):Byte;
    function DeleteFromStructure(aPos:TPoint):Byte;

    function Adding(Position:TPoint):Byte;
    function Moving(Position:TPoint):Byte;
    function Deleting(Position:TPoint):Byte;

    procedure MITextPropertiesClick(Sender:TObject);
    procedure InitializeTextMenu(var Menu:TPopupMenu;Parent:TForm);

    function GetCount():Integer;

   public
    FOnShow : TNEvent;
    FIsSymbol : TPosAskEvent;
    FNullOperations : TNEvent;
    FMoveActivate : TNEvent;
    FDeleteActivate : TNEvent;
    FOPAsk : TOpAskEvent;
    FOnChangeText: TCHangeTextEvent;

     constructor Create(DrawCanvas:TCanvas;TextIL:TImageList;Parent:TForm; Graphics:TPanelGraphics);
     destructor Destroy; override;

     function GetPopisekData(Index:Integer):TPopisek;
     function SetPopisekData(Index:Integer;Data:TPopisek):Byte;

     procedure Paint;
     procedure PaintTextMove(KurzorPos:TPoint);
     function PaintCursor(CursorPos:TPoint):TCursorDraw;

     procedure Escape;

     function GetPopisek(aPos:TPoint):SmallInt;
     function IsObsazeno(Pos1,Pos2:TPoint):boolean;

     function Add(aText:string;aColor:ShortInt):Byte;
     function Move:Byte;
     function Delete:Byte;

     procedure SetLoadedData(LoadData:TBytes);
     procedure SetLoadedDataV32(LoadData:TBytes);
     function GetSaveData:TBytes;
     procedure Reset;

     function MouseUp(Position:TPoint;Button:TMouseButton):Byte;

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
     property OnChangeText: TChangeTextEvent read FOnChangeText write FOnChangeText;
  end;//TPopisky

implementation

constructor TPopisky.Create(DrawCanvas:TCanvas;TextIL:TImageList;Parent:TForm; Graphics:TPanelGraphics);
begin
 Self.DrawObject.Canvas := DrawCanvas;
 Self.DrawObject.TextIL := TextIL;
 Self.Graphics          := Graphics;
 Self.Data := TList<TPopisek>.Create();

 Self.InitializeTextMenu(Self.TextMenu,Parent);

 Self.Reset;
end;//function

destructor TPopisky.Destroy;
begin
 Self.Data.Free();

 if (Assigned(Self.TextMenu)) then
  begin
   Self.TextMenu.Free;
   Self.TextMenu := nil;
  end;
end;//destructor

//pridani popisku
function TPopisky.AddToStructure(aPos:TPoint;aText:string;aColor:ShortInt):Byte;
var p:TPopisek;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH-1)) or (aPos.Y > (_MAX_HEIGHT-1))) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu
 if (Self.GetPopisek(aPos) <> -1) then
  begin
   Result := 2;
   Exit;
  end;//if (Self.Bitmap[aPos.X,aPos.Y] <> -1)
 if (Self.Count >= 255) then
  begin
   Result := 3;
   Exit;
  end;
 if ((Length(aText) < _MIN_TEXT_LENGTH) or (Length(aText) > _MAX_TEXT_LENGTH)) then
  begin
   Result := 4;
   Exit;
  end;


 p.Position  := aPos;
 p.Text      := aText;
 p.Color     := aColor;

 Self.Data.Add(p);

 Result := 0;
end;//function

//smazani popisku
function TPopisky.DeleteFromStructure(aPos:TPoint):Byte;
var PIndex:Integer;
begin
 if ((aPos.X < 0) or (aPos.Y < 0) or (aPos.X > (_MAX_WIDTH-1)) or (aPos.Y > (_MAX_HEIGHT-1))) then
  begin
   Result := 1;
   Exit;
  end;//if kontrola limitu

 PIndex := Self.GetPopisek(aPos);
 if (PIndex = -1) then
  begin
   Result := 2;
   Exit;
  end;

 //samotne mazani
 Self.Data.Delete(PIndex);

 Result := 0;
end;//function

//zjisteni, zda-li je na dane pozici popisek, popr jeho index v poli separatoru
function TPopisky.GetPopisek(aPos:TPoint):SmallInt;
var i,j:Integer;
begin
 Result := -1;

 //vychazime z toho, ze 1 popisek lze zapsat pouze na 1 radek
 for i := 0 to Self.Count-1 do
  begin
   if (Self.Data[i].Position.Y = aPos.Y) then
    begin
     for j := Self.Data[i].Position.X to Self.Data[i].Position.X+(Length(Self.Data[i].Text))-1 do
      begin
       if (j = aPos.X) then
        begin
         Result := i;
         Break;
        end;//if (Self.Popisky[i].Position.X = j)
      end;//for j
    end;//if (Self.Popisky[i].Position.Y = aPos.Y)
  end;//for i
end;//function

//nacteni surovych dat do struktur
procedure TPopisky.SetLoadedData(LoadData:TBytes);
var i, j:Integer;
    p:TPopisek;
    count:Integer;
begin
 Self.Data.Clear();
 count := Length(LoadData) div _Block_Length;

 for i := 0 to count-1 do
  begin
   p.Position.X := LoadData[(i*_Block_Length)];
   p.Position.Y := LoadData[(i*_Block_Length)+1];
   p.Color      := LoadData[(i*_Block_Length)+2];
   p.Text       := '';

   for j := 0 to _MAX_TEXT_LENGTH do
    begin
     if ((LoadData[(i*_Block_Length)+3+(j*2)] = 0) and (LoadData[(i*_Block_Length)+3+(j*2)+1] = 0)) then Break;
     p.Text := p.Text + chr(((LoadData[(i*_Block_Length)+3+(j*2)]) shl 8) +
                        LoadData[(i*_Block_Length)+3+(j*2)+1]);
    end;//for j

   Self.Data.Add(p);
  end;//for i
end;//function

procedure TPopisky.SetLoadedDataV32(LoadData:TBytes);
var pos:Integer;
    i:Integer;
    len:Integer;
    p:TPopisek;
begin
 Self.Data.Clear();
 pos := 0;
 i := 0;

 while pos < Length(LoadData) do
  begin
   p.Position.X := LoadData[pos];
   p.Position.Y := LoadData[pos+1];
   p.Color      := LoadData[pos+2];
   len := LoadData[pos+3];
   p.Text := TEncoding.UTF8.GetString(LoadData, pos+4, len);

   Self.Data.Add(p);
   i := i + 1;
   pos := pos + 4 + len;
  end;//for i

 Self.Data.Count := i;
end;

//ziskani surovych dat zapisovanych do souboru z dat programu
function TPopisky.GetSaveData:TBytes;
var bytesBuf:TBytes;
    len:Integer;
    currentLen:Integer;
    p:TPopisek;
begin
 SetLength(Result, 1024);
 currentLen := 2;

 for p in Self.Data do
  begin
   len := TEncoding.UTF8.GetByteCount(p.Text);
   SetLength(bytesBuf, len);
   bytesBuf := TEncoding.UTF8.GetBytes(p.Text);

   if (Length(Result) < currentLen + len + 4) then
     SetLength(Result, Length(Result)*2);

   Result[currentLen] := p.Position.X;
   Result[currentLen+1] := p.Position.Y;
   Result[currentLen+2] := p.Color;
   Result[currentLen+3] := len;

   CopyMemory(@Result[currentLen+4], bytesBuf, len);

   currentLen := currentLen + len + 4;
  end;//for i

 SetLength(Result, currentLen);
 Result[0] := hi(currentLen-2);
 Result[1] := lo(currentLen-2);
end;//procedure

procedure TPopisky.Reset;
begin
 Self.Data.Count := 0;

 Self.Escape;
end;//procedure

procedure TPopisky.Escape;
begin
 Self.Operations.FAddKrok    := 0;
 Self.Operations.FMoveKrok   := 0;
 Self.Operations.FDeleteKrok := 0;
end;//procedure

//vykresleni textu
procedure TPopisky.Paint;
var i:Integer;
begin
 //vykresleni textu
 for i := 0 to Self.Count-1 do
   Self.Graphics.TextOutputI(Self.Data[i].Position, Self.Data[i].Text, Self.Data[i].Color, clBlack);
end;//procedure

function TPopisky.GetPopisekData(Index:Integer):TPopisek;
begin
 if (Index >= _MAX_POPISKY) then
  begin
   Result.Color := -1;
   Exit;
  end;

 Result := Self.Data[Index];
end;//function

function TPopisky.SetPopisekData(Index:Integer;Data:TPopisek):Byte;
begin
 if (Index >= _MAX_POPISKY) then
  begin
   Result := 1;
   Exit;
  end;//if (Index => _MAX_POPISKY)

 Self.Data[Index] := Data;

 Result := 0;
end;//function

//pridavani textu
function TPopisky.Adding(Position:TPoint):Byte;
var i:Integer;
begin
 Result := 0;

 //kontrola obsazenosti pozice
 if (Assigned(FIsSymbol)) then
  begin
   for i := Position.X to Position.X+Length(Self.Operations.TextProperties.Text)-1 do
    begin
     if (FIsSymbol(Point(i,Position.Y))) then
      begin
       Result := 4;
       Exit;
      end;
    end;//for i
  end else begin
   if (Self.IsObsazeno(Position,Point(Position.X+Length(Self.Operations.TextProperties.Text),Position.Y))) then
    begin
     Result := 4;
     Exit;
    end;//if PanelBitmap...
  end;//else (Assigned(FIsOperation))

 //kontrola obsazenosti
 if (Self.IsObsazeno(Position,Point(Position.X+Length(Self.Operations.TextProperties.Text),Position.Y))) then
  begin
   Result := 4;
   Exit;
  end;//if Self.IsBitmapObsazeno

 if (Self.AddToStructure(Position,Self.Operations.TextProperties.Text,Self.Operations.TextProperties.Color) <> 0) then
  begin
   //chyba externi funkce
   Result := 3;
   Exit;
  end;//if (aReturn <> 0)

 Self.Operations.FAddKrok := 0;
end;//procedure

//pohyb textu
function TPopisky.Moving(Position:TPoint):Byte;
var PopisekIndex:Integer;
    i:Integer;
begin
 Result := 0;

 //zde neni skupina pripustna
 case (Self.Operations.FMoveKrok) of
  1:begin
     PopisekIndex := Self.GetPopisek(Position);
     if (PopisekIndex = -1) then
      begin
       //neni s cim pohybovat
       Result := 2;
       Exit;
      end;

     //ziskani dat popisku
     Self.Operations.TextProperties.Text  := Self.GetPopisekData(PopisekIndex).Text;
     Self.Operations.TextProperties.Color := Self.GetPopisekData(PopisekIndex).Color;

     if (Self.DeleteFromStructure(Position) <> 0) then
      begin
       //chyba externi funkce
       Result := 3;
       Exit;
      end;//if (PanelBitmap.DeleteSymbol(Position) <> 0)

     if (Assigned(FNullOperations)) then FNullOperations;
     Self.Operations.FMoveKrok := 2;
    end;//case 1
  2:begin
     if (Assigned(FIsSymbol)) then
      begin
       for i := Position.X to Position.X+Length(Self.Operations.TextProperties.Text)-1 do
        begin
         if (FIsSymbol(Point(i,Position.Y))) then
          begin
           Result := 4;
           Exit;
          end;
        end;//for i
      end else begin
       if (Self.IsObsazeno(Position,Point(Position.X+Length(Self.Operations.TextProperties.Text),Position.Y))) then
        begin
         Result := 4;
         Exit;
        end;
      end;//else Assigned(FIsOperation)

     if (Self.AddToStructure(Position,Self.Operations.TextProperties.Text,Self.Operations.TextProperties.Color) <> 0) then
      begin
       //chyba externi funkce
       Result := 3;
       Exit;
      end;//if (aReturn <> 0)

     Self.Operations.TextProperties.Text  := '';
     Self.Operations.TextProperties.Color := 0;
     Self.Operations.FMoveKrok             := 0;

     //znovu pripraveni pohybu objektu
     if Assigned(FOnShow) then
      begin
       FOnShow;
       Sleep(50);
      end;
     if (Assigned(Self.FMoveActivate)) then Self.FMoveActivate;
    end;//case 3
  end;//case
end;//procedure

//mazani textu
function TPopisky.Deleting(Position:TPoint):Byte;
begin
 Result := 0;

 if (Self.GetPopisek(Position) = -1) then
  begin
   Result := 2;
   Exit;
  end;

 Self.Operations.FDeleteKrok := 0;

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
end;//procedure

function TPopisky.IsObsazeno(Pos1,Pos2:TPoint):Boolean;
var i,j:Integer;
begin
 Result := false;

 //kontrola obsazenosti
 for i := Pos1.X to Pos2.X do
  begin
    for j := Pos1.Y to Pos2.Y do
     begin
      if (Self.GetPopisek(Point(i,j)) <> -1) then
       begin
        Result := true;
        Exit;
       end;
     end;//for j
  end;//for i
end;//function

function TPopisky.Add(aText:string;aColor:ShortInt):Byte;
begin
 if ((Length(aText) < _MIN_TEXT_LENGTH) or (Length(aText) > _MAX_TEXT_LENGTH)) then
  begin
   Result := 2;
   Exit;
  end;

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


 Self.Operations.FAddKrok             := 1;
 Self.Operations.TextProperties.Text  := aText;
 Self.Operations.TextProperties.Color := aColor;

 Result := 0;
end;//function

function TPopisky.Move:Byte;
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

function TPopisky.Delete:Byte;
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

function TPopisky.MouseUp(Position:TPoint;Button:TMouseButton):Byte;
begin
 Result := 0;

 if (Button = mbLeft) then
  begin
   if (Self.Operations.FAddKrok    > 0) then Result := Self.Adding(Position);
   if (Self.Operations.FMoveKrok   > 0) then Result := Self.Moving(Position);
   if (Self.Operations.FDeleteKrok > 0) then Result := Self.Deleting(Position);
  end;//if Button = mbLeft

 if (Button = mbRight) then
  begin
   Self.MenuPosition := Position;

   if (Self.GetPopisek(Position) <> -1) then Self.TextMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
  end;//if Button = mbRight
end;//procedure

//zabyva se vykreslovanim posouvanych objektu textu
procedure TPopisky.PaintTextMove(KurzorPos:TPoint);
begin
 //pridavani, posouvani
 if ((Self.Operations.FAddKrok = 1) or (Self.Operations.FMoveKrok = 2)) then
     Self.Graphics.TextOutputI(KurzorPos, Self.Operations.TextProperties.Text, Self.Operations.TextProperties.Color, clBlack);
end;//procedure

//vykresleni kurzoru - vraci data PIXELECH!
function TPopisky.PaintCursor(CursorPos:TPoint):TCursorDraw;
var PopisekI:Integer;
    PData:TPopisek;
begin
 Result.Color := 0;
 Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;
 Result.Pos2.X := CursorPos.X*_Symbol_Sirka;
 Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;

 if (Self.Operations.FAddKrok = 1) then
  begin
   Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;
   Result.Pos2.X := (CursorPos.X+(Length(Self.Operations.TextProperties.Text)-1))*_Symbol_Sirka;

   Result.Color := 2;
  end;
 if ((Self.Operations.FMoveKrok = 1) or (Self.Operations.FDeleteKrok = 1)) then
  begin
   PopisekI := Self.GetPopisek(CursorPos);
   if (PopisekI = -1) then
    begin
     Result.Color := 1;
    end else begin
     Result.Color := 2;
     PData := Self.GetPopisekData(PopisekI);
     Result.Pos1.X := PData.Position.X*_Symbol_Sirka;
     Result.Pos1.Y := PData.Position.Y*_Symbol_Vyska;
     Result.Pos2.X := (PData.Position.X+Length(Pdata.Text)-1)*_Symbol_Sirka;
     Result.Pos2.Y := PData.Position.Y*_Symbol_Vyska;
    end;//else PopisekI = -1
  end;
 if (Self.Operations.FMoveKrok = 2) then
  begin
   Result.Color := 2;
   Result.Pos2.X := (CursorPos.X+Length(Self.Operations.TextProperties.Text)-1)*_Symbol_Sirka;
   Result.Pos2.Y := CursorPos.Y*_Symbol_Vyska;
  end;
end;//function

procedure TPopisky.MITextPropertiesClick(Sender:TObject);
var PIndex:Integer;
    aText:string;
    aColor:Integer;
    aPopisek:TPopisek;
begin
 PIndex := Self.GetPopisek(MenuPosition);

 aPopisek := Self.GetPopisekData(PIndex);
 aText  := aPopisek.Text;
 aColor := aPopisek.Color;
 if Assigned(FOnChangeText) then FOnChangeText(Self, aText, aColor);

 aPopisek.Text  := aText;
 aPopisek.Color := aColor;
 Self.SetPopisekData(PIndex,aPopisek);

 if Assigned(FOnShow) then FOnShow;
end;//procedure

procedure TPopisky.InitializeTextMenu(var Menu:TPopupMenu;Parent:TForm);
var MI:TMenuItem;
begin
 Menu := TPopupMenu.Create(Parent);

 MI := TMenuItem.Create(Menu);
 MI.Caption := 'Vlastnosti';
 MI.Enabled := true;
 MI.Visible := true;
 MI.OnClick := Self.MITextPropertiesClick;

 Menu.Items.Add(MI);
end;//procedure

function TPopisky.GetCount():Integer;
begin
 Result := Self.Data.Count;
end;

end.//unit

