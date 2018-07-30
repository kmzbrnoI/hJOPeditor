unit RPConst;

interface

uses Graphics;

const
 _Symbol_Sirka = 8;
 _Symbol_Vyska = 12;

 //zde je definovan pocet barev
 _Symbol_ColorsCount = 10;
 //zde jsou definovany jednotluve barvy
 _Symbol_Colors: array [0..(_Symbol_ColorsCount-1)] of TColor = ($FF00FF,$A0A0A0,$0000FF,$00FF00,$FFFFFF,$FFFF00,$FF0000,$00FFFF,$800080,$000000);

 _Uvazka_Spr_Sirka = 9;

 _BLK_VYH     = 0;
 _BLK_USEK    = 1;
 _BLK_IR      = 2;
 _BLK_SCOM    = 3;
 _BLK_PREJEZD = 4;
 _BLK_TRAT    = 5;
 _BLK_UVAZKA  = 6;

 _MAX_BLK     = 1023;

implementation

end.//unit
