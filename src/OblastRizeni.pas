unit OblastRizeni;

//delkarace struktur Oblasti rizeni

interface

uses Types;

const
  _MAX_OR  = 8;
  _MAX_OSV = 8;

type
  //1 osvetleni
  TOsv = record
   board:Byte;
   port:Byte;
   name:string;  //max 5 znaku
  end;

 //vsechna osvetleni
 TOsvs=record
  Cnt:Byte;
  Data:array [0.._MAX_OSV-1] of TOsv;
 end;

 //prava
 TORRights=record
  ModCasStart:Boolean;
  ModCasStop:Boolean;
  ModCasSet:Boolean;
 end;

 //pozice symbolu OR
 TPoss=record
  DK:TPoint;
  DKOr:byte;  //orientace DK (0,1)
  Queue:TPoint;
  Time:TPoint;
 end;

 //1 oR
 TOR=record
  Name:string;
  ShortName:string;
  id:string;
  Lichy:Byte;
  Rights:TORRights;
  Osvetleni:TOsvs;
  Poss:TPoss;
 end;

 //pouzivao pri presunech OR apod.
 TORGraf=record
  MovingOR:Integer;   //index v poli
  MovingSymbol:Byte;  //DK, queue, cas
 end;


implementation

end.//unit
