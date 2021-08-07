unit PGraphics;

// panel graphics
// je trida, ktera obsahuje vykreslovaci nastroje dostupne pro vsechny podtridy panelu

interface

uses DXDraws, Graphics, Classes, Types, SysUtils, StrUtils, Controls, Global,
  symbolHelper;

type
  TPanelGraphics = class
  private
    DrawObject: TDXDraw;
    IL_Text: TImageList;

  public

    blik: boolean; // pokud neco ma blikat, tady je globalne ulozen jeho stav
    // true = sviti, false = zhasnuto
    // obsluhu promenne zajistuje timer

    constructor Create(DrawObject: TDXDraw; IL_Text: TImageList);

    procedure TextOutputI(Pos: TPoint; Text: string; Popredi: SymbolColor; Pozadi: TColor; underline: boolean = false;
      transparent: boolean = false);
  end;

implementation

/// /////////////////////////////////////////////////////////////////////////////

constructor TPanelGraphics.Create(DrawObject: TDXDraw; IL_Text: TImageList);
begin
  inherited Create();
  Self.DrawObject := DrawObject;
  Self.IL_Text := IL_Text;
end; // ctor

/// /////////////////////////////////////////////////////////////////////////////

procedure TPanelGraphics.TextOutputI(Pos: TPoint; Text: string; Popredi: SymbolColor; Pozadi: TColor;
  underline: boolean = false; transparent: boolean = false);
begin
  // transparent is faster

  if (not transparent) then
  begin
    Self.DrawObject.Surface.Canvas.Pen.Color := Pozadi;
    Self.DrawObject.Surface.Canvas.Brush.Color := Pozadi;
    Self.DrawObject.Surface.Canvas.Rectangle(Pos.X * _Symbol_Sirka, Pos.Y * _Symbol_Vyska,
      (Pos.X + Length(Text)) * _Symbol_Sirka, (Pos.Y + 1) * _Symbol_Vyska);
  end;

  for var j := 0 to Length(Text) - 1 do
  begin
    // prevedeni textu na indexy v ImageListu
    // texty v image listu jsou ilozeny v ASCII, coz tyto Delphi evidentne neumi zchroustat
    // proto tato silenost....
    var TextIndex: Integer;
    case (Text[j + 1]) of
      #32 .. #90:
        TextIndex := ord(Text[j + 1]) - 32;
      #97 .. #122:
        TextIndex := ord(Text[j + 1]) - 97 + 59;
      'š':
        TextIndex := 90;
      'ť':
        TextIndex := 91;
      'ž':
        TextIndex := 92;
      'á':
        TextIndex := 93;
      'č':
        TextIndex := 94;
      'é':
        TextIndex := 95;
      'ě':
        TextIndex := 96;
      'í':
        TextIndex := 97;
      'ď':
        TextIndex := 98;
      'ň':
        TextIndex := 99;
      'ó':
        TextIndex := 100;
      'ř':
        TextIndex := 101;
      'ů':
        TextIndex := 102;
      'ú':
        TextIndex := 103;
      'ý':
        TextIndex := 104;

      'Š':
        TextIndex := 105;
      'Ť':
        TextIndex := 106;
      'Ž':
        TextIndex := 107;
      'Á':
        TextIndex := 108;
      'Č':
        TextIndex := 109;
      'É':
        TextIndex := 110;
      'Ě':
        TextIndex := 111;
      'Í':
        TextIndex := 112;
      'Ď':
        TextIndex := 113;
      'Ň':
        TextIndex := 114;
      'Ó':
        TextIndex := 115;
      'Ř':
        TextIndex := 116;
      'Ů':
        TextIndex := 117;
      'Ú':
        TextIndex := 118;
      'Ý':
        TextIndex := 119;
    else
      TextIndex := 0;
    end;

    Self.IL_Text.Draw(Self.DrawObject.Surface.Canvas, Pos.X * _Symbol_Sirka + (j * _Symbol_Sirka),
      Pos.Y * _Symbol_Vyska, (TextIndex * 10) + Integer(Popredi))
  end; // for j

  if (underline) then
  begin
    Self.DrawObject.Surface.Canvas.Pen.Color := _Symbol_Colors[Integer(Popredi)];
    Self.DrawObject.Surface.Canvas.Rectangle(Pos.X * _Symbol_Sirka, (Pos.Y + 1) * _Symbol_Vyska - 1,
      (Pos.X + Length(Text)) * _Symbol_Sirka, (Pos.Y + 1) * _Symbol_Vyska);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

end.// unit
