# Metai
Automatische Versmaßanalyse des Langgedichts *Metai* ('Jahreszeiten') von [Kristijonas Donelaitis](https://de.wikipedia.org/wiki/Kristijonas_Donelaitis), unternommen im Rahmen des Seminars [*Altlitauisch*](https://agnes.hu-berlin.de/lupo/rds?state=verpublish&status=init&vmfile=no&publishid=188201&moduleCall=webInfo&publishConfFile=webInfo&publishSubDir=veranstaltung) im Wintersemester 2021/2022 bei Anna-Helene Feulner (Humboldt-Universität zu Berlin).

Der auf dem [*Corpus Donelaitis*](https://titus.fkidg1.uni-frankfurt.de/cordon/menu/de/texte.html) (CorDon) basierende Ausgangstext steht [hier](https://github.com/kmein/metai/releases/download/latest/metai.csv) als CSV-Tabelle zur Verfügung.

Eine CSV-Tabelle mit den Ergebnissen der Analyse stehen [hier](https://github.com/kmein/metai/releases/download/latest/metai-scansion.csv) zum Download bereit. Der Quellcode des Programms, das die Analyse durchführt, findet sich im Unterverzeichnis [`scansion/`](/scansion).

Die Ergebnis-Tabelle enthält für jeden Vers folgende Informationen:

- `book`, `verse` ist die Position des Verses im Gedicht.
- `text` ist der normalisierte Text des Verses.
- `words` ist die Anzahl der Wörter im Vers.
- `syllables` ist die Anzahl der Silben im Vers.
- `scansion` ist die als beste ausgewählte Analyse, d. h. Abfolge von Daktylen (`D`) und Spondeen (`S`). Wenn uneindeutig, werden die einzelnen Möglichkeiten mit '`|`' voneinander abgetrennt.
- `3h`, `5h`, `ktt`, `7h`, `pqt`, `bd` enthält `Just Marked` wenn der jeweilige Einschnitt im Vers vorhanden und durch ein Satzzeichen markiert ist, `Just Unmarked`, wenn an der entsprechenden Stelle eine Wortgrenze vorhanden ist, aber kein Satzzeichen, und `Nothing`, wenn der jeweilige Einschnitt im Vers nicht vorliegt.
- `metre`, `metreConflict` ist das aus Donelaitis' metrischen Markierungen erschlossene Schema sowie die Anzahl der Konflikte zu `scansion`.
- `stress`, `stressConflict` ist das aus den notierten Wortakzenten erschlossene Schema sowie die Anzahl der Konflikte zu `scansion`.
- `weight`, `weightConflict` ist das aus der Vokallänge und Silbenstruktur erschlossene Schema sowie die Anzahl der Konflikte zu `scansion`.

Auf den Ergebnissen baut eine Datenanalyse mittels [Pandas](https://pandas.pydata.org/) auf. Das dazugehörige Jupyter-Notebook ist [hier](/statistics.ipynb) einsehbar. Die entstehenden Tabellen und Grafiken sind [hier](https://github.com/kmein/metai/releases/download/latest/assets.zip) gesammelt als ZIP-Archiv zum Download verfügbar.

Um die Tabellen und Assets selbst zu generieren, muss [Nix](https://nixos.org/) installiert sein und im Root-Verzeichnis dieses Repositorys `nix build .#metai-scansion-csv -o metai-scansion.csv` bzw. `nix build .#metai-assets -o metai-assets` ausgeführt werden.
