unit PGraphics;

// panel graphics
//  je trida, ktera obsahuje vykreslovaci nastroje dostupne pro vsechny podtridy panelu

interface

uses DXDraws, Graphics, Classes, Types, SysUtils, StrUtils, Controls, Global,
     symbolHelper;

type
  TPanelGraphics = class
    private
      DrawObject:TDXDraw;
      IL_Text:TImageList;

    public

     blik:boolean;                          // pokud neco ma blikat, tady je globalne ulozen jeho stav
                                            // true = sviti, false = zhasnuto
                                            // obsluhu promenne zajistuje timer

      constructor Create(drawObject:TDXDraw; IL_Text:TImageList);

      procedure TextOutputC(Pos:TPoint;Text:string;Popredi,Pozadi:TColor);
      procedure TextOutputI(Pos:TPoint;Text:string;Popredi:ShortInt;Pozadi:TColor);

      function GetColorIndex(Color:TColor):integer;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////

constructor TPanelGraphics.Create(drawObject:TDXDraw; IL_Text:TImageList);
begin
 inherited Create();
 Self.DrawObject := drawObject;
 Self.IL_Text    := IL_Text;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TPanelGraphics.TextOutputC(Pos:TPoint;Text:string;Popredi,Pozadi:TColor);
begin
 Self.TextOutputI(Pos, Text, Self.GetColorIndex(Popredi), Pozadi);
end;//procedure

procedure TPanelGraphics.TextOutputI(Pos:TPoint;Text:string;Popredi:ShortInt;Pozadi:TColor);
var j:Integer;
    TextIndex:Integer;
begin
 for j := 0 to Length(Text)-1 do
  begin
   //prevedeni textu na indexy v ImageListu
   //texty v image listu jsou ilozeny v ASCII, coz tyto Delphi evidentne neumi zchroustat
   // proto tato silenost....
   case (Text[j+1]) of
    #32..#90:  TextIndex := ord(Text[j+1])-32;
    #97..#122: TextIndex := ord(Text[j+1])-97+59;
    'š' : TextIndex := 90;
    '' : TextIndex := 91;
    'ž' : TextIndex := 92;
    'á' : TextIndex := 93;
    'è' : TextIndex := 94;
    'é' : TextIndex := 95;
    'ì' : TextIndex := 96;
    'í' : TextIndex := 97;
    'ï' : TextIndex := 98;
    'ò' : TextIndex := 99;
    'ó' : TextIndex := 100;
    'ø' : TextIndex := 101;
    'ù' : TextIndex := 102;
    'ú' : TextIndex := 103;
    'ý' : TextIndex := 104;

    'Š' : TextIndex := 105;
    '' : TextIndex := 106;
    'Ž' : TextIndex := 107;
    'Á' : TextIndex := 108;
    'È' : TextIndex := 109;
    'É' : TextIndex := 110;
    'Ì' : TextIndex := 111;
    'Í' : TextIndex := 112;
    'Ï' : TextIndex := 113;
    'Ò' : TextIndex := 114;
    'Ó' : TextIndex := 115;
    'Ø' : TextIndex := 116;
    'Ù' : TextIndex := 117;
    'Ú' : TextIndex := 118;
    'Ý' : TextIndex := 119;
   else
    TextIndex := 0;
   end;

   Self.IL_Text.BkColor := Pozadi;
   Self.IL_Text.Draw(Self.DrawObject.Surface.Canvas,Pos.X*_Symbol_Sirka+(j*_Symbol_Sirka),Pos.Y*_Symbol_Vyska,(TextIndex*10)+Popredi);
  end;//for j
end;//procedure

////////////////////////////////////////////////////////////////////////////////

//TColor -> color index
function TPanelGraphics.GetColorIndex(Color:TColor):integer;
var i:Integer;
begin
 Result := 0;
 for i := 0 to _Symbol_ColorsCount-1 do
  begin
   if (_Symbol_Colors[i] = Color) then
    begin
     Result := i;
     Break;
    end;
  end;//for i
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
