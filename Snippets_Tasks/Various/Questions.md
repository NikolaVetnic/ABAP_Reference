# Various Questions

## `BOOLEAN` u ABAPu pitanje

ABAP ne podrzava i verovatno nikad nece podrzavati eksplicitne boolean varijable kao sto je to slucaj sa vecinom ostalih PJ. Iz tog razloga, istorijski se uvrezila praksa koriscenja literala `X` za `TRUE`, ` ` za `FALSE` i `-` za `undefined`. Ovi literali se srecu u legacy kodu ali se njihova upotreba ne preporucuje.

Alternativa je abap_bool tip iz grupe tipova abap koja je zapravo char duzine 1 (u cemu odgovara `X`, ` ` i `-` literalima) i koja ima vrednosti abap_true i abap_false. Ovaj tip je bolja varijanta od literala buduci da ne dozvoljava drugacije vrednosti osim abap_true i abap_false.

## Mylyn Plugin for Eclipse

Read full article here: [http://nikolapacekvetnic.rs/?p=1685](http://nikolapacekvetnic.rs/?p=1685)
