unit ObjToZT;
// unita definujici prechodovou funkci mezi objektovymi daty a daty "zaverove tabulky"
// zde dochazi zejmena ke zpracovani korenu useku

interface

uses ReliefObjects, ReliefZT;

type
 TObjToZT=class
  private const

  private

  public
    function ObjToZT(ObjectData:TPanelObjects; ZT:TPanelZT):Byte;
 end;//TConvert


implementation

////////////////////////////////////////////////////////////////////////////////

function TObjToZT.ObjToZT(ObjectData:TPanelObjects; ZT:TPanelZT):Byte;
begin


 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

end.//unit
