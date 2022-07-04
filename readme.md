# 2048

## Popis hry:

Hra 2048 spočívá v postupném spojování čísel stejné hodnoty, dokud nevznikne číslo 2048. Na haracím poli 4x4 začíná hráč se dvěma čísly hodnoty 2 na náhodné pozici. Při každém tahu hráč zvolí jeden ze 4 směrů a všechna čísla se posunou v daném směru o maximální možnou vzdálenost. Následně se na náhodnou volnou pozici vloží další číslo s hodnotou 2 nebo 4. Pokud do sebe narazí dvě stejná čísla, dojde k jejich spojení. Hra končí pokud dojde k dosažení hodnoty 2048, nebo jsou plně obsazeny všechny pozice a nelze se již dále pohnout.

## Specifikace programu:

- konzolová aplikace
- po spuštění se vykreslí hláška s možnostmi obtížností
  - číslo které je potřeba složit
- po zvolení obtížnosti se vykreslí začáteční hrací pole
- následně bude hráč volit tahy pomocí <kbd>W</kbd><kbd>A</kbd><kbd>S</kbd><kbd>D</kbd> a po každém tahu se hrací plocha překreslí na novou
- při dosasžení potřebné hodnoty nebo zaplnění celé hrací plochy (bez možnosti dalšího pohybu) se vypíše win/lose hláška a aplikace se ukončí
