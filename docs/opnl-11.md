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
 - B= [blok technologie, jemuz je symbol prirazen]
 - S= [symboly] ulozeno textove
      pevne delky:
       - 0-2..souradnice X
       - 3-5..souradnice Y
       - 6-7..symbol
 - C= [JCClick] ulozeno textove
      pevne delky:
       - 0-2: souradnice X
       - 3-5: souradnice Y
 - P= [KPopisek] ulozeno textove
      pevne delky:
       - 0-2: souradnice X
       - 3-5. souradnice Y
 - N= [nazev koleje] ulozeno textove
 - OR= [oblast rizeni] integer 0-n
 - R= [root, koren]
       - 0-2: souradnice X
       - 3-5: souradnice Y

  -- nasledujici data jsou v souboru ulozena jen, pokud jsou k dispozici vetve: --

  - VC= vetve count = [pocet vetvi]
  - V0...V(N-1)= [vetve] ulozeno textove
      pevne delky:

      - 0-2: 1. vyhybka - index v poli vyhybek (nikoliv technologicky blok!)
      - 3-4: 1. vyhybka : index dalsi vetve pro polohu "vodorovne"
      - 5-6: 1. vyhybka: index dalsi vetve pro polohu "sikmo"

      - 7-9: 2. vyhybka - index v poli vyhybek (nikoliv technologicky blok!)
      - 10-11: 2. vyhybka : index dalsi vetve pro polohu "vodorovne"
      - 12-13: 2. vyhybka: index dalsi vetve pro polohu "sikmo"

      Následující sekce se opakuje:
      - 7-9: souradnice X
      - 10-12: souradnice Y
      - 13-15: symbol

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
