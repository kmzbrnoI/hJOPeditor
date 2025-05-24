# Specifikace formátu souboru bpnl v4.2

Soubor `.bpnl` je binární soubor, který v sobě obsahuje "bitmapovou" podobu
reliéfu. Ke každému políčku přiřazuje symbol, avšak nijak nespojuje políčka
do větších logických celků, ani jim nepřiřazuje technologické bloky.

Oproti předchozí verzi (v4.1) se zvětšuje maximální šířka a výška z 255 na 65535 polí.

Soubor je nekompripovaný, avšak použitím komprese lze u velkých panelů dosáhnout
poměrně vysoké úspory.

Kódování vícebytových čísel: první byte (nejnižší offset v souboru) obsahuje bity
nejvyšších významů, poslední (nejvyšší offset v souboru) byte obsahuje bity
nejnižších významů.

## Sekce souboru

Soubor se skládá z několika sekcí, každá sekce je ukončena byty `0xFF, 0xFF`

 1. Hlavička
    Obsahuje několik prvků, z nichž každý má pevný offset a pevnou délku.

    * offset:0, délka:2 : identifikace souboru: znaky 'br'
    * offset:2, délka:1 : verze souboru
      - 4 bity zleva = major
      - 4 bity zprava = minor
    * offset:3, délka:2 : šířka reliéfu
    * offset:5, délka:2 : výška reliéfu

 2. Bitmapová data reliéfu
    Vlastní symboly reliéfu. Obsahuje čísla symbolů uspořádaná po řádcích.
    Každý řádek je ukončen dvojicí znaků CR,LF (#13,#10)

 3. Texty
    Formát: 0. a 1. byte je délka bloku v bytech.

    Každý blok dat obsahuje:
     * 0. byte: Hi(X),
     * 1. byte: Lo(X),
     * 2. byte: Hi(Y),
     * 3. byte: Lo(Y),
     * 4. byte: barva,
     * 5. byte: délka UTF-8 zakódovaného textu v bytech
     * 6.-k. byte: UTF-8 text zakódovaný v bytech o délce udané v 5. bytu

    Pokud má první byte řetězce hodnotu `17` (dec), jedná se o popisek bloku.
    V takovém případě se číslo `17` ignoruje a text začíná až od dalšího znaku.
    Délka textu je včetně bytu s hodnotou `17`.

 4. Vertikální oddělovače
    Formát: 0. a 1. byte je počet oddělovačů. Pak následují pozice ve formátu:
    [Hi(X), Lo(X), Hi(Y), Lo(Y)]. Každá dvojice zabírá 4 byty, jedna souřadnice zabírá 2 byty.

 5. Horizontální oddělovače
    Formát stejný jako formát vertikálních oddělovačů.

 6. Symboly popisků kolejí
    Formát stejný jako formát oddělovačů.

 7. Symboly konce jízdní cesty
    Formát stejný jako formát oddělovačů.

 8. Symboly zobrazení souprav na koleji
    Formát stejný jako formát oddělovačů.

 9. Oblasti řízení

    První 2 byty udávají počet oblastí řízení. Pak následují samotné oblasti
    řízení.

    Oblast řízení je řetězec formátu:

    ```
    nazev;nazev_zkratka;id;lichy_smer(0,1);orientace_DK(0,1);ModCasStart(0,1);
    ModCasStop(0,1);ModCasSet(0,1);dkposx;dkposy;qposx;qposy;timeposx;timeposy;
    osv_mtb|osv_port|osv_name;
    ```

    Každá oblast řízení je ukočená znakem LF (#13).

    Blok je ukočen dalším znakem LF (#13), tedy v praxi jsou na konci bloku
    dva znaky LF.
