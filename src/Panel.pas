unit Panel;

interface

uses DXDraws, ImgList, Controls, Windows, SysUtils, Graphics, Classes,
     ReliefObjects, Forms, StdCtrls, ExtCtrls, ReliefBitmap, Menus, ReliefText,
     Global, BitmapToObj, OblastRizeni, StrUtils, DirectX, RPConst, PGraphics;

type
 TObjectPointer=record
  SymbolID,ObjID:Integer;
 end;

// TMode=(dmBitmap,dmOddelovace,dmBloky);     presunuto do Global.pas

 //error ID:
  //0 - pretahovat lze pouze zleva doprava a zhora dolu
  //1 - prazdne pole (neni s cim operovat)
  //2 - chyba externi funkce
  //3 - obsazeno
  //4 - presazeny limity
 TErrorEvent = procedure(Sender:TObject; ErrorID:Integer) of object;
 TMoveEvent  = procedure(Sender:TObject; Position:TPoint) of object;

 TRelief=class
  private const
    _MAX_MOVE = 64;
    _MAX_TEXT_LENGTH = 32;
    _MIN_TEXT_LENGTH = 1;

    _Resource_Symbols = 'All8';
    _Resource_Text    = 'Text8';
    _Resource_DK      = 'DK8';

    //vychozi data
    _Def_Color_Pozadi = clBlack;
    _Def_Color_Mrizka = clGray;
    _Def_Color_Kurzor = clYellow;
    _Def_Color_Kurzor_OnObject = clRed;
    _Def_Color_Kurzor_Operation = clFuchsia;
    _Def_Color_Objects = 1;
    _Def_Mrizka = true;

    //rozmery DK, zasobniku, casovace
    _OR_Size:array [0..5] of Byte = (5,3,14,1,16,1);

    _Separator_BitmapIndex = 352;
    _Symbols_DefColor      = clBlack;
    //barvy symbolu
//    _Symbol_Navaznost: array [0..23] of ShortInt = (-1,0,1,0,0,-1,0,1,0,1,1,0,-1,0,0,-1,-1,0,0,1,0,-1,1,0);
     //zde jsou ulozeny navaznosti na objekty useku: navaznost 1:XY;navaznost 2:XY atd.
     //kazdy objekt zabira 4 polozky

     //pripony bitmapoveho a vektoroveho formatu souboru
     _Suf_bmp = '.bpnl';
     _Suf_obj = '.opnl';

  private
   DrawObject:TDXDraw;
   ParentForm:TForm;

   PanelBitmap:TPanelBitmap;
   PanelObjects:TPanelObjects;

   IL_Symbols:TImageList;
   IL_Text:TImageList;
   IL_DK:TImageList;

   DrawMode:TMode;
   Graphics:TPanelGraphics;

   Zobrazeni:record
    PanelWidth,PanelHeight:SmallInt;
    Mrizka:Boolean;
   end;

   Colors:record
    Mrizka,Pozadi,Kurzor,KurzorOnObject,KurzorOperation:TColor;
    Objects:ShortInt;
   end;

   LastPos:TPoint;

  Panel:record
   FileStav:SmallInt;
   FileCesta:string;
  end;

  //oblasti rizeni
  ORs:TOrs;

  DK_Menu:TPopUpMenu;

  ORMove:TORGraf;
  ORClick:TORGraf;
  FMove:Boolean;

  FOnError         : TErrorEvent;
  FOnMove          : TMoveEvent;
  FOnChangeText    : TChangeTextEvent;
  FOnBlokEdit      : TBlokAskEvent;
  FOnMsg           : TMsgEvent;
  FFormBlkClose    : TGlobalEvent;

   function Initialize(Rozmery:TPoint;Mode:TMode):Byte;

   function LoadIL(var IL:TImageList;ResourceName:string;PartWidth,PartHeight:Byte;MaskColor:TColor = clPurple):Integer;
   procedure PaintMrizka(MrizkaColor:TColor);

   function PaintKurzor(CursorData:TCursorDraw):Byte;

   procedure PaintOR();
   function PaintORCursor(CursorPos:TPoint):TCursorDraw;
   function ORMouseUp(Position:TPoint;Button:TMouseButton):Byte;
   function GetORGraf(pos:TPoint):TORGraf;
   procedure DKMenuInit();
   procedure DKPropClick(Sender:TObject);
   procedure DKDeleteClick(Sender:TObject);

   function ORSave:string;
   function ORLoad(data:string):Byte;

   procedure DXDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
   procedure DXDMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
   procedure DXDDoubleClick(Sender: TObject);

   procedure BitmapMouseUp(Position:TPoint;Button:TMouseButton);
   procedure ObjectMouseUp(Position:TPoint;Button:TMouseButton);

   procedure BitmapDblClick(Position:TPoint);
   procedure ObjectDblClick(Position:TPoint);

   procedure SetMrizka(aMrizka:Boolean);

   function FLoad(aFile:string):Byte;

   procedure ShowEvent;
   procedure BlokEditEvent(Sender:TObject; Blok:TGraphBlok);
   procedure MessageEvent(Sender:TObject; msg:string);
   procedure BlkFormCloseEvent(Sender:TObject);

   function IsOREvent(Pos:TPoint):boolean;

   procedure SetGroup(State:boolean);
   function GetGroup:boolean;

   procedure AssignBitmapEvents();
   procedure AssignObjectEvents();


  public
   constructor Create(DDRaw:TDXDraw;aParentForm:TForm);
   destructor Destroy; override;

   function New(size:TPoint;firstOR:TOR):Byte;
   function Open(aFile:string):Byte;
   function Save(aFile:string):Byte;

   procedure Show(CursorPos:TPoint);
   function Escape(Group:boolean):Byte;
   function SetRozmery(aWidth,aHeight:Byte):Byte;

   function AddSymbol(SymbolID:Integer):Byte;
   function AddText(Text:string;Color:Integer):Byte;
   function AddJCClick:Byte;
   function AddSeparator:Byte;
   function AddKPopisek:Byte;

   procedure MoveBitmapSymbol;
   procedure DeleteBitmapSymbol;
   procedure KeyPress(Key:Integer);     // accepts keys from ApplicationEvent component

   function SwitchMode(aMode:TMode):Byte;
   procedure HideMouse();

   //OR
   function GetORList():TORList;
   function GetOR(index:Integer):TOR;
   function SetOR(index:Integer;data:TOR):Byte;
   function AddOR(data:TOR):Byte;
   function DeleteOR(index:Integer):Byte; overload;
   function DeleteOR(pos:TPoint):Byte; overload;

   function CheckValid(var error_cnt:Byte):TStrings;     //overi validitu naeditovanych dat a vrati chybove hlasky

   property MrizkaColor:TColor read Colors.Mrizka write Colors.Mrizka;
   property PozadiColor:TColor read Colors.Pozadi write Colors.Pozadi;
   property KurzorColor:TColor read Colors.Kurzor write Colors.Kurzor;
   property KurzorOnObjectColor:TColor read Colors.KurzorOnObject write Colors.KurzorOnObject;
   property KurzorOperation:TColor read Colors.KurzorOperation write Colors.KurzorOperation;
   property ObjectColor:ShortInt read Colors.Objects write Colors.Objects;

   property Mode:TMode read DrawMode;
   property Skupina:boolean read GetGroup write SetGroup;
   property Mrizka:boolean read Zobrazeni.Mrizka write SetMrizka;

   property PanelWidth:SmallInt read Zobrazeni.PanelWidth;
   property PanelHeight:SmallInt read Zobrazeni.PanelHeight;

   property FileStav:SmallInt read Panel.FileStav;
   property FileCesta:string read Panel.FileCesta;

   //events
   property OnError: TErrorEvent read FOnError write FOnError;
   property OnMove: TMoveEvent read FOnMove write FOnMove;
   property OnChangeText: TChangeTextEvent read FOnChangeText write FOnChangeText;
   property OnBlokEdit: TBlokAskEvent read FOnBlokEdit write FOnBlokEdit;
   property OnMsg: TMsgEvent read FOnMsg write FOnMsg;
   property FormBlkClose: TGlobalEvent read FFormBlkClose write FFormBlkClose;
 end;

implementation
{$R Resource.res}

uses fOREdit, fMain;

constructor TRelief.Create(DDRaw:TDXDraw;aParentForm:TForm);
begin
 inherited Create;

 Self.DrawObject := DDraw;
 Self.ParentForm := aParentForm;

 Self.DrawMode := dmBitmap;

 Self.DKMenuInit();
end;//contructor

//inicializace objektu Relief
function TRelief.Initialize(Rozmery:TPoint;Mode:TMode):Byte;
begin
 Self.DrawObject.OnMouseUp   := Self.DXDMouseUp;
 Self.DrawObject.OnMouseMove := Self.DXDMouseMove;
 Self.DrawObject.OnDblClick  := Self.DXDDoubleClick;

 Self.Colors.Mrizka           := _Def_Color_Mrizka;
 Self.Colors.Pozadi           := _Def_Color_Pozadi;
 Self.Colors.Kurzor           := _Def_Color_Kurzor;
 Self.Colors.KurzorOnObject   := _Def_Color_Kurzor_OnObject;
 Self.Colors.Objects          := _Def_Color_Objects;
 Self.Colors.KurzorOperation  := _Def_Color_Kurzor_Operation;

 Self.DrawMode          := Mode;
 Self.Zobrazeni.Mrizka  := _Def_Mrizka;
 Self.Panel.FileStav    := 1;


 Self.ORs.Cnt := 0;
 Self.ORMove.MovingOR  := -1;
 Self.ORClick.MovingOR := -1;
 Self.FMove := false;

 if (Self.LoadIL(Self.IL_Symbols,_Resource_Symbols,8,12) <> 0) then
  begin
   Result := 1;
   Exit;
  end;//if (Self.LoadIL(Self.IL_Symbols,_Resource_Symbols,8,12) <> 0)
 if (Self.LoadIL(Self.IL_Text,_Resource_Text,8,12) <> 0) then
  begin
   Result := 2;
   Exit;
  end;//if (Self.LoadIL(Self.IL_Symbols,_Resource_Symbols,8,12) <> 0)
 if (Self.LoadIL(Self.IL_DK,_Resource_DK,8*_OR_Size[0],12*_OR_Size[1]) <> 0) then
  begin
   Result := 3;
   Exit;
  end;//if (Self.LoadIL(Self.IL_Symbols,_Resource_Symbols,8,12) <> 0)

 Self.Graphics := TPanelGraphics.Create(Self.DrawObject, Self.IL_Text);

 case (Mode) of
  dmBitmap:begin
            Self.PanelBitmap := TPanelBitmap.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas, Rozmery.X, Rozmery.Y, dmBitmap, Self.ParentForm, Self.Graphics);
            Self.AssignBitmapEvents();
           end;//dmBitmap
  dmBloky:begin
            Self.PanelObjects := TPanelObjects.Create(Self.IL_Symbols, Self.IL_Text, Self.DrawObject.Surface.Canvas, Rozmery.X, Rozmery.Y, Self.DrawObject, Self.Graphics);
            Self.AssignObjectEvents();
           end;//dmBitmap
 end;//case

 Self.Zobrazeni.PanelWidth  := Rozmery.X;
 Self.Zobrazeni.PanelHeight := Rozmery.Y;
 Self.DrawObject.Width  := Self.Zobrazeni.PanelWidth*_Symbol_Sirka;
 Self.DrawObject.Height := Self.Zobrazeni.PanelHeight*_Symbol_Vyska;

 Self.Show(Point(-1,-1));

 Self.DrawObject.Visible := true;
 Result := 0;
end;//function

//novy relief
function TRelief.New(size:TPoint;firstOR:TOR):Byte;
begin
 if (Self.Initialize(size,dmBitmap) <> 0) then
  begin
   Result := 1;
   Exit;
  end;

 if (Self.AddOR(firstOR) <> 0) then
  begin
   Result := 2;
   Exit;
  end;

 Result := 0;
end;//function

function TRelief.Open(aFile:string):Byte;
var Mode:TMode;
    return:Byte;
begin
 if (not FileExists(aFile)) then
  begin
   Result := 1;
   Exit;
  end;

 if (RightStr(aFile, 5) = _Suf_bmp) then Mode := dmBitmap
 else if (RightStr(aFile, 5) = _Suf_obj) then Mode := dmBloky
 else begin
   Result := 2;
   Exit;
 end;

 if (Self.Initialize(Point(0,0), Mode) <> 0) then
  begin
   Result := 3;
   Exit;
  end;

 return := Self.FLoad(aFile);
 if (return <> 0) then
  begin
   Result := return+3;
   Exit;
  end;

 Result := 0;
end;//function

destructor TRelief.Destroy();
begin
 if (Assigned(Self.PanelBitmap)) then
  begin
   Self.PanelBitmap.Destroy;
   Self.PanelBitmap := nil;
  end;

 if (Assigned(Self.PanelObjects)) then
  begin
   Self.PanelObjects.Destroy;
   Self.PanelObjects := nil;
  end;

 Self.DrawObject.Visible := false;

 Self.IL_Symbols.Free;
 Self.IL_Text.Free;
 Self.IL_DK.Free;

 Self.Graphics.Free();

 Self.DK_Menu.Free();

 inherited Destroy;
end;//destructor

function TRelief.LoadIL(var IL:TImageList;ResourceName:string;PartWidth,PartHeight:Byte;MaskColor:TColor = clPurple):Integer;
var AllImages,ColouredImages:TBitmap;
    i,j,k,symbol:byte;
begin
 IL := TImageList.Create(nil);

 AllImages := TBitmap.Create;
 AllImages.LoadFromResourceName(HInstance,ResourceName);
 ColouredImages := TBitmap.Create;

 IL.SetSize(PartWidth,PartHeight);
 ColouredImages.SetSize(PartWidth*_Symbol_ColorsCount,PartHeight);

 for symbol := 0 to (AllImages.Width div PartWidth)-1 do
  begin
   for i := 0 to _Symbol_ColorsCount-1 do
    begin
     ColouredImages.Canvas.CopyRect(Rect(i*PartWidth,0,(i*PartWidth)+PartWidth,PartHeight),AllImages.Canvas,Rect(symbol*PartWidth,0,(symbol*PartWidth)+PartWidth,PartHeight));

     for j := 0 to PartWidth-1 do
      begin
       for k := 0 to PartHeight-1 do
        begin
         if (ColouredImages.Canvas.Pixels[j+(i*PartWidth),k] = _Symbols_DefColor) then ColouredImages.Canvas.Pixels[j+(i*PartWidth),k] := _Symbol_Colors[i];
        end;//for k
      end;//for j
    end;//for i

   IL.AddMasked(ColouredImages,MaskColor);
  end;//for symbol

 ColouredImages.Free;
 AllImages.Free;

 Result := 0;
end;//procedure

//hlavni zobrazeni celeho reliefu
procedure TRelief.Show(CursorPos:TPoint);
begin
 Self.DrawObject.Surface.Canvas.Release();
 Self.DrawObject.BeginScene;

 // smazani reliefu
 Self.DrawObject.Surface.Fill(Self.Colors.Pozadi);

 case (Self.DrawMode) of
  dmBitmap,dmOddelovace:Self.PanelBitmap.PaintMove(CursorPos);
 end;//case

 //vykresleni objektu s ohledem na DrawMode
 case (Self.DrawMode) of
   dmBitmap,dmOddelovace : Self.PanelBitmap.Paint;
   dmBloky,dmRoots       : Self.PanelObjects.Paint;
 end;
 Self.PaintOR();
 if (Self.Zobrazeni.Mrizka) then Self.PaintMrizka(Self.Colors.Mrizka);

 //prioritu ma posun OR
 if (Self.PaintKurzor(Self.PaintORCursor(CursorPos)) <> 0) then
  begin
   //vykreslovani kurzoru a specialnich barev vzhledem k modum a operacim
   case (Self.DrawMode) of
     dmBitmap,dmOddelovace : Self.PaintKurzor(Self.PanelBitmap.PaintCursor(CursorPos));
     dmBloky,dmRoots       : Self.PaintKurzor(Self.PanelObjects.PaintCursor(CursorPos));
   end;//case
  end;

 Self.DrawObject.EndScene;
 Self.DrawObject.Surface.Canvas.Release;
 Self.DrawObject.Flip;
end;//procedure

//vykresleni mrizky
procedure TRelief.PaintMrizka(MrizkaColor:TColor);
var i:Integer;
begin
 Self.DrawObject.Surface.Canvas.Pen.Color := MrizkaColor;

 for i := 1 to Self.Zobrazeni.PanelWidth-1 do
  begin
   Self.DrawObject.Surface.Canvas.MoveTo((i*_Symbol_Sirka)-1,0);
   Self.DrawObject.Surface.Canvas.LineTo((i*_Symbol_Sirka)-1,Self.Zobrazeni.PanelHeight*_Symbol_Vyska);
  end;//for i
 for i := 1 to Self.Zobrazeni.PanelHeight-1 do
  begin
   Self.DrawObject.Surface.Canvas.MoveTo(0,(i*_Symbol_Vyska)-1);
   Self.DrawObject.Surface.Canvas.LineTo(Self.Zobrazeni.PanelWidth*_Symbol_Sirka,(i*_Symbol_Vyska)-1);
  end;//for i
end;//procedure

//vykresluje kurzor
function TRelief.PaintKurzor(CursorData:TCursorDraw):Byte;
var Colors:array [0..2] of TColor;
begin
 if ((CursorData.Pos1.X < 0) and (CursorData.Pos1.Y < 0) and (CursorData.Pos2.X < 0) and (CursorData.Pos2.Y < 0)) then
  begin
   Result := 1;
   Exit;
  end;
 if (CursorData.Color < 0) then
  begin
   Result := 2;
   Exit;
  end;

 CursorData.Pos1.X := CursorData.Pos1.X - 1;
 CursorData.Pos1.Y := CursorData.Pos1.Y - 1;
 CursorData.Pos2.X := CursorData.Pos2.X - 1;
 CursorData.Pos2.Y := CursorData.Pos2.Y - 1;

 Colors[0] := Self.Colors.Kurzor;
 Colors[1] := Self.Colors.KurzorOperation;
 Colors[2] := Self.Colors.KurzorOnObject;

 Self.DrawObject.Surface.Canvas.Pen.Color := Colors[CursorData.Color];
 if ((CursorData.Pos2.X >= CursorData.Pos1.X) and (CursorData.Pos2.Y >= CursorData.Pos1.Y)) then
  begin
   Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X,CursorData.Pos1.Y);
   Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos2.X+8,CursorData.Pos1.Y);

   Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos2.X+8,CursorData.Pos1.Y);
   Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos2.X+8,CursorData.Pos2.Y+12);

   Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos2.X+8,CursorData.Pos2.Y+12);
   Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X,CursorData.Pos2.Y+12);

   Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X,CursorData.Pos2.Y+12);
   Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X,CursorData.Pos1.Y);
  end else begin
   if (CursorData.Pos2.X < CursorData.Pos1.X) then
    begin
     Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X,CursorData.Pos1.Y);
     Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X,CursorData.Pos1.Y+12);
    end;
   if (CursorData.Pos2.Y < CursorData.Pos1.Y) then
    begin
     Self.DrawObject.Surface.Canvas.MoveTo(CursorData.Pos1.X,CursorData.Pos1.Y);
     Self.DrawObject.Surface.Canvas.LineTo(CursorData.Pos1.X+8,CursorData.Pos1.Y);
    end;
  end;//else ((KurzorPos2.X > KurzorPos1.X) and (KurzorPos2.Y > KurzorPos1.Y))

 Result := 0;
end;//procedure

procedure TRelief.DXDMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 LastPos.X := X div _Symbol_Sirka;
 LastPos.Y := Y div _Symbol_Vyska;

 if (Self.ORMouseUp(LastPos, Button) = 0) then
  begin
   case (Self.DrawMode) of
    dmBitmap,dmOddelovace : Self.BitmapMouseUp(LastPos, Button);
    dmBloky,dmRoots       : Self.ObjectMouseUp(LastPos, Button);
   end;//case
  end;//ORMouseUp = 0

 Self.Show(LastPos);
end;//procedure

procedure TRelief.DXDMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
 if ((X div _Symbol_Sirka = LastPos.X) and (Y div _Symbol_Vyska = LastPos.Y)) then Exit;

 LastPos.X := X div _Symbol_Sirka;
 LastPos.Y := Y div _Symbol_Vyska;

 case (Self.DrawMode) of
  dmBloky,dmRoots       : Self.PanelObjects.MouseMove(LastPos);
 end;//case

 Self.Show(LastPos);

 if Assigned(FOnMove) then FOnMove(Self, LastPos);
end;//procedure

procedure TRelief.DXDDoubleClick(Sender: TObject);
begin
 case (Self.DrawMode) of
  dmBitmap,dmOddelovace : Self.BitmapDblClick(LastPos);
  dmBloky,dmRoots       : Self.ObjectDblClick(LastPos);
 end;//case

 Self.Show(LastPos);
end;//procedure

procedure TRelief.KeyPress(Key:Integer);
var mouse:TPoint;
begin
 case (Key) of
   VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN: begin
    if (not Self.ParentForm.Active) then Exit();

    GetCursorPos(mouse);

    case (Key) of
     VK_LEFT  : mouse.X := mouse.X - _Symbol_Sirka;
     VK_RIGHT : mouse.X := mouse.X + _Symbol_Sirka;
     VK_UP    : mouse.Y := mouse.Y - _Symbol_Vyska;
     VK_DOWN  : mouse.Y := mouse.Y + _Symbol_Vyska;
    end;

    SetCursorPos(mouse.X, mouse.Y);
   end;

   VK_RETURN: Self.DXDMouseUp(Self.DrawObject, mbLeft, [], Self.LastPos.X*_Symbol_Sirka, Self.LastPos.Y*_Symbol_Vyska);      // enter
 end;
end;//procedure

function TRelief.Escape(Group:boolean):Byte;
begin
 Result := 0;

 case (Self.DrawMode) of
  dmBitmap,dmOddelovace : if (Assigned(Self.PanelBitmap)) then Self.PanelBitmap.Escape(Group) else Result := 1;
  dmBloky,dmRoots       : if (Assigned(Self.PanelObjects)) then Self.PanelObjects.Escape else Result := 1;
 end;

 Self.FMove := false;

 Self.Show(LastPos);
end;//function

function TRelief.SwitchMode(aMode:TMode):Byte;
begin
 if (((Self.DrawMode = dmBitmap) or (Self.DrawMode = dmOddelovace)) and (aMode = dmBloky)) then
  begin
   //konverze z Bitmap na Objects

   Self.PanelObjects            := TPanelObjects.Create(Self.IL_Symbols,Self.IL_Text,Self.DrawObject.Surface.Canvas,Self.Zobrazeni.PanelWidth,Self.Zobrazeni.PanelHeight, Self.DrawObject, Self.Graphics);
   Self.AssignObjectEvents();
   Result := Self.PanelObjects.Import(Self.PanelBitmap);

   if (Result <> 0) then
    begin
     FreeAndNil(Self.PanelObjects);
     Exit;
    end;

   FreeAndNil(Self.PanelBitmap);

   Self.Panel.FileStav := 1;
  end;//if (((Self.DrawMode = dmBitmap) or (Self.DrawMode = dmOddelovace)) and (aMode = dmBloky))

 //konverze Bitmap <-> oddelovace
 if (((Self.DrawMode = dmBitmap) and (aMode = dmOddelovace)) or ((Self.DrawMode = dmOddelovace) and (aMode = dmBitmap))) then Self.PanelBitmap.Mode := aMode;

 //konverze Bloky <-> Koreny
 if (((Self.DrawMode = dmBloky) and (aMode = dmRoots)) or ((Self.DrawMode = dmRoots) and (aMode = dmBloky))) then
  begin
   Self.PanelObjects.Mode := aMode;
  end;

 DrawMode := aMode;

 Self.Show(Point(-1,-1));
 Result := 0;
end;//procedure

procedure TRelief.BitmapMouseUp(Position:TPoint;Button:TMouseButton);
var Return:Byte;
begin
 Return := Self.PanelBitmap.MouseUp(Position,Button);

 if (Return <> 0) then if Assigned(FOnError) then FOnError(Self, Return-1);
end;//procedure

procedure TRelief.ObjectMouseUp(Position:TPoint;Button:TMouseButton);
var Return:Byte;
begin
 Return := Self.PanelObjects.MouseUp(Position,Button);

 if (Return <> 0) then if Assigned(FOnError) then FOnError(Self, Return-1);
end;//procedure

procedure TRelief.BitmapDblClick(Position:TPoint);
var Return:Byte;
begin
 Return := Self.PanelBitmap.DblClick(Position);

 if (Return <> 0) then if Assigned(FOnError) then FOnError(Self, Return-1);
end;//procedure

procedure TRelief.ObjectDblClick(Position:TPoint);
var Return:Byte;
begin
 Return := Self.PanelObjects.DblClick(Position);

 if (Return <> 0) then if Assigned(FOnError) then FOnError(Self, Return-1);
end;//procedure

function TRelief.SetRozmery(aWidth,aHeight:Byte):Byte;
begin
 if (Self.Mode <> dmBitmap) then
  begin
   Result := 1;
   Exit;
  end;

 if (PanelBitmap.SetRozmery(aWidth,aHeight) <> 0) then
  begin
   Result := 2;
   Exit;
  end;
// PanelObjects.SetRozmery(aWidth,aHeight);

 Self.Zobrazeni.PanelWidth  := aWidth;
 Self.Zobrazeni.PanelHeight := aHeight;

 Self.DrawObject.Width  := aWidth * _Symbol_Sirka;
 Self.DrawObject.Height := aHeight * _Symbol_Vyska;

 Self.Show(LastPos);

 Result := 0;
end;//function

procedure TRelief.SetMrizka(aMrizka:Boolean);
begin
 Self.Zobrazeni.Mrizka := aMrizka;

 Self.Show(Point(-1,-1));
end;//procedure

function TRelief.FLoad(aFile:string):Byte;
var ORs:string;
begin
 case (Self.DrawMode) of
  dmBitmap,dmOddelovace:begin
                         if (Self.PanelBitmap.FLoad(aFile, ORs) <> 0) then
                          begin
                           Result := 1;
                           Exit;
                          end;// <> 0
                         if (Self.ORLoad(ORs) <> 0) then
                          begin
                           Result := 2;
                           Exit;
                          end;
                         Self.Panel.FileStav  := Self.PanelBitmap.FileStav;
                         Self.Zobrazeni.PanelWidth   := Self.PanelBitmap.PanelWidth;
                         Self.Zobrazeni.PanelHeight  := Self.PanelBitmap.PanelHeight;
                        end;//dmBitmap
  dmBloky,dmRoots:begin
           if (Self.PanelObjects.FLoad(aFile,ORs) <> 0) then
            begin
             Result := 1;
             Exit;
            end;// <> 0
           if (Self.ORLoad(ORs) <> 0) then
            begin
             Result := 2;
             Exit;
            end;

           Self.Panel.FileStav  := Self.PanelObjects.FileStav;
           Self.Zobrazeni.PanelWidth   := Self.PanelObjects.PanelWidth;
           Self.Zobrazeni.PanelHeight  := Self.PanelObjects.PanelHeight;
          end;//dmBloky
 end;//case


 Self.DrawObject.Width  := Self.Zobrazeni.PanelWidth*_Symbol_Sirka;
 Self.DrawObject.Height := Self.Zobrazeni.PanelHeight*_Symbol_Vyska;

 Self.Panel.FileCesta := aFile;
 Result := 0;
end;//function

function TRelief.Save(aFile:string):Byte;
begin
 Result := 0;

 case (Self.DrawMode) of
  dmBitmap,dmOddelovace:begin
     Result := Self.PanelBitmap.FSave(aFile, Self.ORSave());
     if (Result <> 0) then Exit;
     Self.Panel.FileStav := Self.PanelBitmap.FileStav;
    end;//dmBitmap

  dmBloky,dmRoots:begin
     Result := Self.PanelObjects.FSave(aFile,Self.ORSave());
     if (Result <> 0) then Exit;
     Self.Panel.FileStav := Self.PanelObjects.FileStav;
    end;//dmBloky
 end;///case

 Self.Panel.FileCesta := aFile;
end;//function

procedure TRelief.ShowEvent;
begin
 Self.Show(LastPos);
end;//procedure

procedure TRelief.SetGroup(State:boolean);
begin
 if (Assigned(Self.PanelBitmap)) then Self.PanelBitmap.Group := State;
end;//procedure

function TRelief.GetGroup:boolean;
begin
 Result := false;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.Group;
end;//fucntion

procedure TRelief.MoveBitmapSymbol;
begin
 if (Assigned(Self.PanelBitmap)) then Self.PanelBitmap.Move;
 Self.FMove := true;
 Self.Show(Self.LastPos);
end;//procedure

procedure TRelief.DeleteBitmapSymbol();
begin
 if (Assigned(Self.PanelBitmap)) then Self.PanelBitmap.Delete;
 Self.FMove := false;
 Self.Show(Self.LastPos);
end;//procedure

function TRelief.AddSymbol(SymbolID:Integer):Byte;
begin
 Result := 255;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.Symbols.Add(SymbolID);
end;//procedure

function TRelief.AddText(Text:string;Color:Integer):Byte;
begin
 Result := 255;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.Popisky.Add(Text,Color);
end;//procedure

function TRelief.AddJCClick:Byte;
begin
 Result := 255;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.JCClick.Add;
end;//procedure

function TRelief.AddSeparator:Byte;
begin
 Result := 255;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.Separators.Add;
end;//procedure

function TRelief.AddKPopisek:Byte;
begin
 Result := 255;

 if (Assigned(Self.PanelBitmap)) then Result := Self.PanelBitmap.KPopisky.Add;
end;//procedure

procedure TRelief.MessageEvent(Sender:TObject; msg:string);
begin
 if (Assigned(Self.FOnMsg)) then Self.FOnMsg(Self, msg);
end;//procedure

////////////////////////////////////////////////////////////////////////////////
//operace s oblastmi rizeni:

function TRelief.GetORList():TORList;
var i:Integer;
begin
 Result.Cnt := Self.ORs.Cnt;
 for i := 0 to Self.ORs.Cnt-1 do Result.data[i] := Self.ORs.Data[i].Name;   
end;//function

function TRelief.GetOR(index:Integer):TOR;
begin
 if (index < 0) or (index >= Self.ORs.Cnt) then Exit;

 Result := Self.ORs.Data[index];
end;//function

function TRelief.SetOR(index:Integer;data:TOR):Byte;
begin
 if (index < 0) or (index >= Self.ORs.Cnt) then
  begin
   Result := 1;
   Exit;
  end;

 Self.ORs.Data[index] := data;
 Result := 0;
end;//function

function TRelief.AddOR(data:TOR):Byte;
begin
 if (Self.ORs.Cnt >= _MAX_OR) then
  begin
   Result := 1;
   Exit;
  end;

 Self.ORs.Cnt := Self.ORs.Cnt + 1;
 Self.ORs.Data[Self.ORs.Cnt-1] := data;

 Self.ORMove.MovingOR     := Self.ORs.Cnt-1;
 Self.ORMove.MovingSymbol := 0; //0 = dopravni kancelar

 Result := 0;
end;//function

function TRelief.DeleteOR(index:Integer):Byte;
var i:Integer;
begin
 if (index < 0) or (index >= Self.ORs.Cnt) then
  begin
   Result := 1;
   Exit;
  end;

 for i := index to Self.ORs.Cnt-2 do Self.ORs.Data[i] := Self.ORs.Data[i+1];
 Self.ORs.Cnt := Self.ORs.Cnt - 1;

 Result := 0;
end;//function

function TRelief.DeleteOR(pos:TPoint):Byte;
var i:Integer;
begin
 for i := 0 to Self.ORs.Cnt-1 do
  begin
   if ((Self.ORs.Data[i].Poss.DK.X = pos.X) and (Self.ORs.Data[i].Poss.DK.Y = pos.Y)) then
    begin
     Self.DeleteOR(i);
     Result := 0;
     Exit;
    end;
  end;

 Result := 1;
end;//function

//vykresli vsechny oblasti rizeni
//zatim jen baracky
//bereme ohled na posun
procedure TRelief.PaintOR();
var i:Integer;
begin
 for i := 0 to Self.ORs.Cnt-1 do
  begin
   if (Self.ORMove.MovingOR = i) then
    begin
     case (Self.ORMove.MovingSymbol) of
      0:Self.ORs.Data[i].Poss.DK    := Self.LastPos;
      1:Self.ORs.Data[i].Poss.Queue := Self.LastPos;
      2:Self.ORs.Data[i].Poss.Time  := Self.LastPos;
     end;
    end;

   Self.IL_DK.Draw(Self.DrawObject.Surface.Canvas,Self.ORs.Data[i].Poss.DK.X*_Symbol_Sirka,Self.ORs.Data[i].Poss.DK.Y*_Symbol_Vyska,(Self.ORs.Data[i].Poss.DKOr*10)+1);

   Self.Graphics.TextOutputI(Self.ORs.Data[i].Poss.Queue, '00 VZ PV EZ 00', 1, clBlack);
   Self.Graphics.TextOutputI(Self.ORs.Data[i].Poss.Time, 'MER CASU', 1, clBlack);
   Self.Graphics.TextOutputC(Point(Self.ORs.Data[i].Poss.Time.X+8, Self.ORs.Data[i].Poss.Time.Y), '        ', clBlack, clWhite);
  end;//for i
end;//procedure

//vykresluje kurzor pri pohybu
function TRelief.PaintORCursor(CursorPos:TPoint):TCursorDraw;
var tmp_or:TORGraf;
begin
 Result.Color := -1;

 if (not Self.FMove) then Exit;

 if (Self.ORMove.MovingOR < 0) then
  begin
   tmp_or := Self.GetORGraf(CursorPos);
   if (tmp_or.MovingOR = -1) then Exit;

   Result.Color := 2;

   case (tmp_or.MovingSymbol) of
    0:begin
       Result.Pos1.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.DK.X)*_Symbol_Sirka;
       Result.Pos1.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.DK.Y)*_Symbol_Vyska;
       Result.Pos2.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.DK.X+_OR_Size[tmp_or.MovingSymbol*2]-1)*_Symbol_Sirka;
       Result.Pos2.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.DK.Y+_OR_Size[tmp_or.MovingSymbol*2+1]-1)*_Symbol_Vyska;
    end;
    1:begin
       Result.Pos1.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.Queue.X)*_Symbol_Sirka;
       Result.Pos1.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.Queue.Y)*_Symbol_Vyska;
       Result.Pos2.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.Queue.X+_OR_Size[tmp_or.MovingSymbol*2]-1)*_Symbol_Sirka;
       Result.Pos2.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.Queue.Y+_OR_Size[tmp_or.MovingSymbol*2+1]-1)*_Symbol_Vyska;
    end;
    2:begin
       Result.Pos1.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.Time.X)*_Symbol_Sirka;
       Result.Pos1.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.Time.Y)*_Symbol_Vyska;
       Result.Pos2.X := (Self.ORs.Data[tmp_or.MovingOR].Poss.Time.X+_OR_Size[tmp_or.MovingSymbol*2]-1)*_Symbol_Sirka;
       Result.Pos2.Y := (Self.ORs.Data[tmp_or.MovingOR].Poss.Time.Y+_OR_Size[tmp_or.MovingSymbol*2+1]-1)*_Symbol_Vyska;
    end;
   end;//case
  end else begin
   Result.Color := 2;

   Result.Pos1.X := CursorPos.X*_Symbol_Sirka;
   Result.Pos1.Y := CursorPos.Y*_Symbol_Vyska;

   Result.Pos2.X := (CursorPos.X+_OR_Size[Self.ORMove.MovingSymbol*2]-1)*_Symbol_Sirka;
   Result.Pos2.Y := (CursorPos.Y+_OR_Size[Self.ORMove.MovingSymbol*2+1]-1)*_Symbol_Vyska;
  end;
end;//function

function TRelief.ORMouseUp(Position:TPoint; Button:TMouseButton):Byte;
var tmp_or:TORGraf;
begin
 Result := 0;

 if (Self.Mode = dmBloky) then
  begin
   // tohleto umoznuje pripradit OR bloku tim, ze kliknu na prislusny baracek
   if (Button <> mbLeft) then Exit;

   tmp_or := Self.GetORGraf(Position);
   if (tmp_or.MovingOR < 0) then Exit;   
   if (tmp_or.MovingSymbol <> 0) then Exit;
   Self.PanelObjects.SetOR(tmp_or.MovingOR);
   if (Assigned(Self.FOnBlokEdit)) then Self.FOnBlokEdit(Self, Self.PanelObjects.selected_obj);
   Result := 1;
  end;//dmBloky

 if (Self.Mode = dmBitmap) then
  begin
   if (Button = mbLeft) then
    begin
     if (Self.ORMove.MovingOR > -1) then
      begin
       //ukonceni pohybu
       Self.ORMove.MovingOR     := -1;
       Self.ORMove.MovingSymbol := 0;

       Self.FMove := false;
       Self.Show(Self.LastPos);
       Self.FMove := true;
       Sleep(50);
       Self.Show(Self.LastPos);
      end else begin
       tmp_or := Self.GetORGraf(Position);
       if (tmp_or.MovingOR > -1) then
         Self.MessageEvent(Self, 'OØ : '+ORs.Data[tmp_or.MovingOR].Name+' (id = '+ORs.Data[tmp_or.MovingOR].id+')');

       //zacatek pohybu
       if (Self.FMove) then
         Self.ORMove := tmp_or;
      end;
    end;

   if (Button = mbRight) then
    begin
     Self.ORClick := Self.GetORGraf(Position);
     if (Self.ORClick.MovingOR < 0) then Exit;
     case (Self.ORClick.MovingSymbol) of
      0:Self.DK_Menu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
     end;//case
    end;
  end;//dmBitmap
end;//function

//vraci OR na dane pozici
function TRelief.GetORGraf(pos:TPoint):TORGraf;
var i:Integer;
begin
 Result.MovingOR     := -1;
 Result.MovingSymbol := 0;

 for i := 0 to Self.ORs.Cnt-1 do
  begin
   if ((pos.X >= Self.ORs.Data[i].Poss.DK.X) and (pos.Y >= Self.ORs.Data[i].Poss.DK.Y)
      and (Self.ORs.Data[i].Poss.DK.X+_OR_Size[0] > pos.X) and (Self.ORs.Data[i].Poss.DK.Y+_OR_Size[1] > pos.Y)) then
    begin
     Result.MovingOR     := i;
     Result.MovingSymbol := 0;
     Exit;
    end;
   if ((pos.X >= Self.ORs.Data[i].Poss.Queue.X) and (pos.Y >= Self.ORs.Data[i].Poss.Queue.Y)
      and (Self.ORs.Data[i].Poss.Queue.X+_OR_Size[2] > pos.X) and (Self.ORs.Data[i].Poss.Queue.Y+_OR_Size[3] > pos.Y)) then
    begin
     Result.MovingOR     := i;
     Result.MovingSymbol := 1;
     Exit;
    end;
   if ((pos.X >= Self.ORs.Data[i].Poss.Time.X) and (pos.Y >= Self.ORs.Data[i].Poss.Time.Y)
      and (Self.ORs.Data[i].Poss.Time.X+_OR_Size[4] > pos.X) and (Self.ORs.Data[i].Poss.Time.Y+_OR_Size[5] > pos.Y)) then
    begin
     Result.MovingOR     := i;
     Result.MovingSymbol := 2;
     Exit;
    end;
  end;//for i
end;//function

procedure TRelief.DKMenuInit();
var MI:TMenuItem;
begin
 //initialize DK menu
 Self.DK_Menu := TPopupMenu.Create(Self.DrawObject);

 MI := TMenuItem.Create(Self.DK_Menu);
 MI.Caption := 'Vlastnosti';
 MI.OnClick := Self.DKPropClick;
 Self.DK_Menu.Items.Add(MI);

 MI := TMenuItem.Create(Self.DK_Menu);
 MI.Caption := 'Smazat OØ';
 MI.OnClick := Self.DKDeleteClick;
 Self.DK_Menu.Items.Add(MI);
end;//procedure

procedure TRelief.DKPropClick(Sender:TObject);
begin
 F_OREdit.OpenForm(Self.ORClick.MovingOR);
end;//procedure

procedure TRelief.DKDeleteClick(Sender:TObject);
begin
 if (Self.ORs.Cnt <= 1) then
  begin
   Application.MessageBox('Poslední OØ nelze smazat!','Nelze pokraèovat',MB_OK OR MB_ICONSTOP);
   Exit;
  end;

 if (Application.MessageBox('Opravdu smazat?','Otázka',MB_YESNO OR MB_ICONQUESTION) = mrYes) then
  begin
   Self.DeleteOR(Self.ORClick.MovingOR);
  end;
end;//procedure

//na kazdem radku je ulozena jedna oblast rizeni ve formatu:
//  nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
function TRelief.ORSave:string;
var i,j:Integer;
begin
 Result := '  ';    //2 byty jsou vyhrazene pro delku zpravy
 for i := 0 to Self.ORs.Cnt-1 do
  begin
   Result := Result + Self.ORs.Data[i].Name+';'+Self.ORs.Data[i].ShortName+';'+Self.ORs.Data[i].id+';'+
             IntToStr(Self.ORs.Data[i].Lichy)+';'+IntToStr(Self.ORs.Data[i].Poss.DKOr)+';'+
             BoolToStr(Self.ORs.Data[i].Rights.ModCasStart)+';'+BoolToStr(Self.ORs.Data[i].Rights.ModCasStop)+';'+BoolToStr(Self.ORs.Data[i].Rights.ModCasSet)+';'+
             IntToStr(Self.ORs.Data[i].Poss.DK.X)+';'+IntToStr(Self.ORs.Data[i].Poss.DK.Y)+';'+
             IntToStr(Self.ORs.Data[i].Poss.Queue.X)+';'+IntToStr(Self.ORs.Data[i].Poss.Queue.Y)+';'+
             IntToStr(Self.ORs.Data[i].Poss.Time.X)+';'+IntToStr(Self.ORs.Data[i].Poss.Time.Y)+';';

   for j := 0 to Self.ORs.Data[i].Osvetleni.Cnt-1 do
    Result := Result + IntToStr(Self.ORs.Data[i].Osvetleni.Data[j].board)+'#'+IntToStr(Self.ORs.Data[i].Osvetleni.Data[j].port)+'#'+Self.ORs.Data[i].Osvetleni.Data[j].name+'|';

   Result := Result + #13;
  end;//for i

 Result := Result + #13;
 Result[1] := chr(hi(Length(Result)-2));
 Result[2] := chr(lo(Length(Result)-2));
end;//function

//na kazdem radku je ulozena jedna oblast rizeni ve formatu:
//  nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;osv_mtb|osv_port|osv_name;
function TRelief.ORLoad(data:string):Byte;
var lines,data_main,data_osv,data_osv2:TStrings;
    i,j:Integer;
begin
 Self.ORs.Cnt := 0;

 lines := TStringList.Create();
 data_main := TStringList.Create();
 data_osv  := TStringList.Create();
 data_osv2 := TStringList.Create();

 if (RightStr(data,2) <> #13#13) then
  begin
   Result := 1;
   Exit;
  end;

 ExtractStrings([#13],[],PChar(LeftStr(data,Length(data)-2)),lines);

 for i := 0 to Lines.Count-1 do
  begin
   data_main.Clear();
   ExtractStrings([';'],[],PChar(lines[i]),data_main);

   if (data_main.Count < 14) then
    begin
     Result := 2;
     Exit;
    end;

   Self.ORs.Cnt := Self.ORs.Cnt + 1;
   Self.ORs.Data[Self.ORs.Cnt-1].Name       := data_main[0];
   Self.ORs.Data[Self.ORs.Cnt-1].ShortName  := data_main[1];
   Self.ORs.Data[Self.ORs.Cnt-1].id         := data_main[2];
   Self.ORs.Data[Self.ORs.Cnt-1].Lichy      := StrToInt(data_main[3]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DKOr  := StrToInt(data_main[4]);

   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasStart := StrToBool(data_main[5]);
   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasStop  := StrToBool(data_main[6]);
   Self.ORs.Data[Self.ORs.Cnt-1].Rights.ModCasSet   := StrToBool(data_main[7]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DK.X := StrToInt(data_main[8]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.DK.Y := StrToInt(data_main[9]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Queue.X := StrToInt(data_main[10]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Queue.Y := StrToInt(data_main[11]);

   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Time.X := StrToInt(data_main[12]);
   Self.ORs.Data[Self.ORs.Cnt-1].Poss.Time.Y := StrToInt(data_main[13]);

   Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt := 0;
   data_osv.Clear();
   if (data_main.Count < 15) then continue;
   
   ExtractStrings(['|'],[],PChar(data_main[14]),data_osv);
   for j := 0 to data_osv.Count-1 do
    begin
     data_osv2.Clear();
     ExtractStrings(['#'],[],PChar(data_osv[j]),data_osv2);

     if (data_osv2.Count < 2) then
      begin
       Result := 3;
       Exit;
      end;

     Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt := Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt + 1;
     Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Data[Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt-1].board := StrToInt(data_osv2[0]);
     Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Data[Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt-1].port  := StrToInt(data_osv2[1]);
     if (data_osv2.Count > 2) then Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Data[Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt-1].name := data_osv2[2] else
         Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Data[Self.ORs.Data[Self.ORs.Cnt-1].Osvetleni.Cnt-1].name := '';
    end;//for j
  end;//for i


 FreeAndNil(lines);
 FreeAndNil(data_main);
 FreeAndNil(data_osv);
 FreeAndNil(data_osv2);
 Result := 0;
end;//procedure


//konec operaci s oblastmi rizeni
////////////////////////////////////////////////////////////////////////////////

function TRelief.CheckValid(var error_cnt:Byte):TStrings;
begin
 if (Integer(Self.Mode) < 2) then
  begin
   Result := nil;
   Exit;
  end;
 Result := Self.PanelObjects.CheckValid(error_cnt);
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.AssignBitmapEvents();
begin
 Self.PanelBitmap.OnShow     := Self.ShowEvent;
 Self.PanelBitmap.OnTextEdit := Self.OnChangeText;
 Self.PanelBitmap.OnORAsk    := Self.IsOREvent;
end;//procedure

procedure TRelief.AssignObjectEvents();
begin
 Self.PanelObjects.OnBlokEdit      := Self.BlokEditEvent;
 Self.PanelObjects.OnMsg           := Self.MessageEvent;
 Self.PanelObjects.OnShow          := Self.ShowEvent;
 Self.PanelObjects.OnFormBlkClose  := Self.BlkFormCloseEvent;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

// vola se z bitmapoveho modu
// vraci true, pokud je na dane pozici DK, zasobnik, ci mereni casu
function TRelief.IsOREvent(Pos:TPoint):boolean;
var i:Integer;
begin
 for i := 0 to Self.ORs.Cnt-1 do
  begin
   if ((Pos.X >= Self.ORs.Data[i].Poss.DK.X) and (Pos.X <= Self.ORs.Data[i].Poss.DK.X+4)
    and (Pos.Y >= Self.ORs.Data[i].Poss.DK.Y) and (Pos.Y <= Self.ORs.Data[i].Poss.DK.Y+2)) then
    begin
     Result := true;
     Exit;
    end;

   if ((Pos.X >= Self.ORs.Data[i].Poss.Queue.X) and (Pos.X <= Self.ORs.Data[i].Poss.Queue.X+13)
    and (Pos.Y = Self.ORs.Data[i].Poss.Queue.Y)) then
    begin
     Result := true;
     Exit;
    end;

   if ((Pos.X >= Self.ORs.Data[i].Poss.Time.X) and (Pos.X <= Self.ORs.Data[i].Poss.Time.X+15)
    and (Pos.Y = Self.ORs.Data[i].Poss.Time.Y)) then
    begin
     Result := true;
     Exit;
    end;

  end;

 Result := false;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlkFormCloseEvent(Sender:TObject);
begin
 if (Assigned(Self.FFormBlkClose)) then Self.FFormBlkClose(Self);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.HideMouse();
begin
 if (Self.LastPos.X > 0) then
  begin
   Self.Show(Point(-1,-1));

   LastPos.X := -1;
   LastPos.Y := -1;
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TRelief.BlokEditEvent(Sender:TObject; Blok:TGraphBlok);
begin
 if (Assigned(Self.FOnBlokEdit)) then Self.FOnBlokEdit(Self, Blok);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

end.//unit

