# Specifikace formátu souboru bpnl v3.2

Soubor `.bpnl` je binární soubor, který v sobě obsahuje "bitmapovou" podobu
reliéfu. Ke každému políčku přiřazuje symbol, avšak nijak nespojuje políčka
do větších logických celků, ani jim nepřiřazuje technologické bloky.

Oproti předchozí verzi umožňuje ukládání vertikálních oddělovačů.

Soubor je nekompripovaný, avšak použitím komprese lze u velkých panelů dosáhnout
poměrně vysoké úspory.

## Sekce souboru

Soubor se skládá z několika sekcí, každá sekce je ukončena byty `0xFF, 0xFF`

 1. Hlavička
    Obsahuje několik prvků, z nichž každý má pevný offset a pevnou délku.

    * offset:0, délka:2 : identifikace souboru: znaky 'br'
    * offset:2, délka:1 : verze souboru
      - 4 bity zleva = major
      - 4 bity zprava = minor
    * offset:3, délka:1 : šířka reliéfu
    * offset:4, délka:1 : výška reliéfu

 2. Bitmapová data reliéfu
    Vlastní symbly reliéfu. Obsahuje čísla symbolů uspořádaná po řádcích.
    Každý řádek je ukončen dvojicí znaků CR,LF (#13,#10)

 3. Popisky
    Formát: 1. a 2. byte je délka bloku v bytech, první byte obsahuje bity
    vyšších významu.

    Každý blok dat obsahuje:
     * 1. byte: posize X,
     * 2. byte: pozice Y,
     * 3. byte: barva,
     * 4. byte: délka UTF-8 zakódovaného textu v bytech
     * 5.-k. byte: UTF-8 text zakódovaný v bytech o délce udané ve 4. bytu

 4. Vertikální oddělovače
    Formát: 1. byte je počet oddělovačů, pak následují dvojice pozic [X, Y]
    Každá dvojice zabíra 2 byty, jedna souřadnice tedy zabírá jeden byte.

 5. Horizontální oddělovače

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
