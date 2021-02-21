unit vetev;

interface

uses symbolHelper;

type
  // po rozdeleni useku je na vetve je usek reprezentovan takto:

  // ukoncovaci element vetve = vyhybka
  TVetevEnd = record
    vyh: Integer; // pokud usek nema vyhybky -> vyh1 = -1, vyh2 = -1 (nastava u useku bez vyhybky a u koncovych vetvi)
    // referuje na index v poli vyhybek (nikoliv na technologicke ID vyhybky!)
    // kazda vetev je ukoncena maximalne 2-ma vyhybkama - koren muze byt ukoncen 2-ma vyhybkama, pak jen jedna
    ref_plus, ref_minus: Integer; // reference  na vetev, kterou se pokracuje, pokud je vyh v poloze + resp. poloze -
    // posledni vetev resp. usek bez vyhybky ma obe reference = -1
  end;

  // vetev useku
  TVetev = record // vetev useku

    node1: TVetevEnd; // reference na 1. vyhybku, ktera ukoncuje tuto vetev
    node2: TVetevEnd; // reference na 2. vyhybku, ktera ukoncuje tuto vetev

    Symbols: array of TReliefSym;
    // s timto dynamicky alokovanym polem je potreba zachazet opradu opatrne
    // realokace trva strasne dlouho !
    // presto si myslim, ze se jedna o vyhodne reseni: pole se bude plnit jen jednou
    // pokud je v useku vykolejka, jednoduse sem ulozime symbol vykolejky
  end;

function DefaultVetev(): TVetev;

implementation

function DefaultVetev(): TVetev;
begin
  Result.node1.vyh := -1;
  Result.node1.ref_plus := -1;
  Result.node1.ref_minus := -1;

  Result.node2.vyh := -1;
  Result.node2.ref_plus := -1;
  Result.node2.ref_minus := -1;
end;

end.
