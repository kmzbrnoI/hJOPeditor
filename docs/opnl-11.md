# Specifikace formátu souboru opnl v1.1

Soubor `.bpnl` je textový ini soubor, který definuje reliéf. Definuje logické
spojení symbolů do bloků a umožňuje blokům na panelu přiřazovat technologické
bloky.

Skládá se z několika sekcí.

## Globální sekce `[G]`
 - ver=1.1 (verze souboru)

## Specifikace panelu `[P]`
 - W=(šířka panelu)
 - H=(výška panelu)
 - U=(počet úseků)
 - N=(počet návěstidel)
 - T=(počet textů)
 - P=(počet pomocných objektů)
 - V=(počet výhybek)
 - Uv=(počet úvazek)
 - UvS=(počet textových polí souprav k úvazkám)
 - Z=(počet zámků)
 - Vyk=(počet výkolejek)
 - R=(počet rozpojovačů)
 - vc=0/1 (flag napočítaných větví)

## Úseky `[U0]`-`[Un-1]`
 - B=(id technologického bloku)
 - S=(seznam symbolů, které tvoří úsek)
    Seznam obsahuje variabilní počet polí délky 8 znaků (bytů), přičemž v každém
    poli je uloženo na konkrétních pozicích.
     - 0-2: souřadnice X
     - 3-5: souřadnice Y
     - 6-7: číslo symbolu

 - C=(seznam polí, které se zobrazují při výběru konce jízdní cesty na úseku)
    Seznam obsahuje variabilní počet polí délky 6 znaků (bytů), přičemž v každém
    poli je uloženo na konkrétních pozicích.
     - 0-2: souřadnice X
     - 3-5: souřadnice Y

 - P=(seznam polí, na kterých se zobrazí číslo koleje)
    Obdobně, jako předchozí.

 - N=(název koleje jako text)
 - OR=(index oblasti řízení)
 - R=(pozice kořene pro barvení)
   - 0-2: souřadnice X
   - 3-5: souřadnice Y

 -- Následujici data reprezentují rozdělení úseku na větve. Né každý úsek musí
    být rozdělen na větve, v takovém případě se vykresluje na základě toho, co
    je uloženo v `S=`. Pokud je uložen úsek včetně větví, `S=` se stále ukládá. --

 - VC=(počet vetví)
 - V0...V(N-1)=(konkrétní větev)
    Formát větve: konkrétní hodnoty na konkrétních índexech:

     - 0-2: index 1. výhybky
     - 3-4: 1. výhybka: index další větve pro polohu "vodorovně"
     - 5-6: 1. výhybka: index dalši vetve pro polohu "šikmo"

     - 7-9: index 2. výhybky
     - 10-11: 2. výhybka: index další větve pro polohu "vodorovně"
     - 12-13: 2. výhybka: index dalši vetve pro polohu "šikmo"

     Následuje seznam symbolů ve větvi. To je seznam, ve kterém má každý prvek
     tento formát:

      - 7-9: souřadnice X
      - 10-12: souřadnice Y
      - 13-15: číslo symbolu

 Čtěte také poznámky níže o dvojité kolejové spojce.

## Návěstidla `[N0]`-`[Nn-1]`
 - B= [asociovany blok technologie]
 - X= [pozice X]
 - Y= [pozice Y]
 - S= [symbol] 0..3 (4 typy navestidel)
 - OR= [oblast rizeni] integer 0-n

## Pomocné bloky `[P0]`-`[Pn-1]`
 - P= [pozice] - 3*X;3*Y;... (bez stredniku - pevne delky)
 - S= [symbol]

## Text `[T0]`-`[Tn-1]`
 - T= [text]
 - X= [pozice X]
 - Y= [pozice Y]
 - C= [barva]
 - B= [blok]
 - OR= [oblast rizeni] integer 0-n

## Výhybky `[V0]`-`[Vn-1]`
 -  B= [asociovany blok technologie]
 - S= [symbol]
 - P= [poloha plus]
 - X= [pozice X]
 - Y= [pozice Y]
 - O= [objekt, kteremu vyhybka patri]
 - OR= [oblast rizeni] integer 0-n

## Přejezdy `[PRJ0]`-`[PRJn-1]`
 - B= [asociovany blok technologie]
 - BP= [blik_pozice] - 3*X;3*Y;3*U... = X,T, tech_usek (bez stredniku - pevne delky)
 - SP= [static_pozice] - 3*X;3*Y;... (bez stredniku - pevne delky)
 - OR= [oblast rizeni] integer 0-n
 - U= [technologicky usek] - blok typu usek, ktery prejezd zobrazuje (prostredni usek prejezdu)

## Úvazky `[Uv0]`-`[Uvn-1]`
 - B= [asociovany blok technologie]
 - D= [zakladni smer]
 - X= [pozice X]
 - Y= [pozice Y]
 - OR= [oblast rizeni] integer 0-n

## Úvazky soupravy `[UvS0]`-`[UvSn-1]`
 - B= [asociovany blok technologie]
 - VD= [vertikalni smer]
 - X= [pozice X]
 - Y= [pozice Y]
 - C= [pocet souprav]
 -  OR= [oblast rizeni] integer 0-n

## Zámky `[Z0]`-`[Zn-1]`
 - B= [asociovany blok technologie]
 - X= [pozice X]
 - Y= [pozice Y]
 - OR= [oblast rizeni] integer 0-n

## Výkolejky `[Vyk0]`-`[Vykn-1]`
 - B= [asociovany blok technologie]
 - X= [pozice X]
 - Y= [pozice Y]
 - T= typ vykolejky (otoceni symbolu)
 - OR= [oblast rizeni] integer 0-n
 - O= [objekt, kteremu vykolejka patri]
 - V= [vetev] vislo vetve, ve kterem je vykolejka (v useku pod ni)

## Rozpojovače `[R0]`-`[Rn-1]`
 - B= [asociovany blok technologie]
 - X= [pozice X]
 - Y= [pozice Y]
 - OR= [oblast rizeni] integer 0-n

---

## Dvojitá kolejová spojka

Dvojitá kolejová spojka je největším nepřitelem při vykreslování reliéfu.
Proto je pro ni definován speciální způsob uložení dat.

DKS je tvořena dvěma úseky, jeden v horní části DKS a jeden ve sdpodní části DSK.
Tyto úseky jsou autonomní, zabývejme se tedy jen způsobem uložení dolní (resp.
horní) poloviny DKS.

Kořen kreslení se do DKS umisťuje na kříž, na tomto úseku jsou pevně definované
větve:

 0. větev: obsahuje část od kříže (bez kříže) až po nejbližší levou výhybku.
    1. výhybka v této větvi je levá výhybka DKS.
 1. větev: obsahuje část od kříže (bez kříže) až po nejbližší pravou výhybku.
    1. výhybka v této větvi je pravá výhybka DKS.
 2. větev obsahuje kolej spojující obě krajní výhybky DKS.
 3. větev obsahuje počátek kreslení od levé výhybky dál
 4. větev obsahuje počátek kreslení od pravé výhybky dál

Další sekce mohou obsahovat strom pro kreslení zbylých částí úseků (navazující
za jednou z krajních výhybek).
