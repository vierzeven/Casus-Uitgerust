       IDENTIFICATION DIVISION.
       PROGRAM-ID. 03--ReservingenVandaagNaarBoekingen.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 30-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ReserveringenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Reserveringen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.
       SELECT Werkbestand ASSIGN TO "C:\COBOL\DATA\tmp.tmp"
           FILE STATUS IS IOStatus.
       SELECT ReserveringenBestandSorted ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\ReserveringenSorted.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       SELECT BoekingenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Boekingen.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BB-Werkpleknummer
           ALTERNATE RECORD KEY IS BB-Naam WITH DUPLICATES
           FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       *> Werkbestand, alleen voor het sorteren (tmp.tmp)
       SD Werkbestand.
       01 WerkRecord.
         02 FILLER PIC X(24).
         02 Datum PIC 9(8).
       *> Reserveringenbestand (wordt gebruikt als input voor het sorteren)
       FD ReserveringenBestand.
       01 ReserveringenRecord.
         02 RB-Naam PIC X(24).
         02 RB-Datum.
           03 RB-Jaar PIC 9(4).
           03 RB-Maand PIC 99.
           03 RB-Dag PIC 99.
       *> Reservingenbestand gesorteerd (wordt gebruikt als output voor het sorteren)
       FD ReserveringenBestandSorted.
       01 ReserveringenRecordSorted.
         02 RBS-Naam PIC X(24).
         02 RBS-Datum PIC 9(8).
       *> Boekingenbestand (wordt overschreven met de reserveringen van vandaag)
       FD BoekingenBestand.
       01 BoekingenRecord.
         02 BB-Naam PIC X(24).
         02 BB-Werkpleknummer.
           03 BB-Huisnummer PIC 99.
           03 BB-Kamernummer PIC 9.
         02 BB-SleutelUitgereiktVlag PIC 9 VALUE ZERO.
           88 BB-SleutelUitgereikt VALUE 1.
           88 BB-SleutelNietUitgereikt VALUE 0.

       WORKING-STORAGE SECTION.
       *> CONSTANTEN
       78 AantalHuisjes VALUE 10.
       *> IO STATUS VLAG
       01 IOStatus PIC XX.
         88 IO-OK VALUE ZEROES.
        *> END OF DATA VLAG
       01 EndOfDataFlag PIC 9 VALUE ZERO.
         88 EndOfData VALUE 1.
         88 NotEndOfData VALUE 0.
       *> RESERVERING
       01 Reservering.
         02 ReserveringsNaam PIC X(24).
         02 ReserveringsDatum PIC 9(8).
       *> BOEKING
       01 Boeking.
         02 BoekingsNaam PIC X(24).
         02 BoekingsHuisnummer PIC 9(2).
         02 BoekingsKamernummer PIC 9.
         02 SleutelUitgereiktVlag PIC 9 VALUE 0.
           88 SleutelUitgereikt VALUE 1.
           88 SleutelNietUitgereikt VALUE 0.
       01 DatumVandaag PIC 9(8).
       *> WERKPLEK
       01 Werkplek OCCURS 1 TO 100 TIMES DEPENDING ON AantalWerkplekken.
         02 WP-Huisnummer PIC 9(2).
         02 WP-Kamernummer PIC 9.
       01 AantalWerkplekken PIC 99 VALUE ZERO.
       01 WerkplekkenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       *> WONING
       01 Woning OCCURS 10 TIMES.
         02 WO-Huisnummer PIC 9(2).
         02 WO-Type PIC X.
         02 WO-AantalKamers PIC 9.
       01 AantalWoningen PIC 99 VALUE ZERO.
       01 WoningenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       01 DatumVerschil PIC S99 VALUE ZERO.
       01 AantalReserveringenVandaag PIC 99 VALUE ZERO.
       01 DatumAlsInteger PIC 9(8).
       01 BoekingenTeller PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.
       BeginProgram.
           *> Ter voorbereiding
           PERFORM InitialseerWoningen
           PERFORM InitialiseerWerkplekken
           PERFORM GetDatumVandaag
           *> Reserveringen sorteren
           PERFORM SorteerReserveringen
           *> Overzichten printen
           PERFORM ToonReserveringen
           *> Reserveringen voor vandaag inboeken
           PERFORM BoekReserveringenVandaag
           PERFORM ToonDagomzet
           *> Ter afsluiting
           DISPLAY SPACE
           STOP RUN.

       InitialseerWoningen.
           PERFORM VARYING WoningenTeller FROM 1 BY 1 UNTIL WoningenTeller > AantalHuisjes
               ADD 1 TO AantalWoningen
               MOVE WoningenTeller TO WO-Huisnummer(WoningenTeller)
               EVALUATE TRUE
                   WHEN WO-Huisnummer(WoningenTeller) EQUALS 2 OR 3 OR 7 OR 8
                       MOVE "L" TO WO-Type(WoningenTeller)
                       MOVE 5 TO WO-AantalKamers(WoningenTeller)
                   WHEN OTHER
                       MOVE "S" TO WO-Type(WoningenTeller)
                       MOVE 3 TO WO-AantalKamers(WoningenTeller)
               END-EVALUATE
           END-PERFORM.

       ToonWoningen.
           DISPLAY SPACE
           DISPLAY "WONINGEN"
           DISPLAY "========"
           PERFORM VARYING WoningenTeller FROM 1 BY 1 UNTIL WoningenTeller > AantalWoningen
               DISPLAY "Huisnummer: " WO-Huisnummer(WoningenTeller) " | " WITH NO ADVANCING
               DISPLAY "Type: " WO-Type(WoningenTeller) " | " WITH NO ADVANCING
               DISPLAY "Aantal kamers: " WO-AantalKamers(WoningenTeller)
           END-PERFORM.

       InitialiseerWerkplekken.
           PERFORM VARYING WoningenTeller FROM 1 BY 1 UNTIL WoningenTeller > AantalWoningen
               PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > WO-AantalKamers(WoningenTeller)
                   ADD 1 TO AantalWerkplekken
                   MOVE WoningenTeller TO WP-Huisnummer(AantalWerkplekken)
                   MOVE WerkplekkenTeller TO WP-Kamernummer(AantalWerkplekken)
               END-PERFORM
           END-PERFORM.

       ToonWerkplekken.
           DISPLAY SPACE
           DISPLAY "WERKPLEKKEN"
           DISPLAY "==========="
           PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > AantalWerkplekken
               DISPLAY WerkplekkenTeller ". Huisnummer: " WP-Huisnummer(WerkplekkenTeller) " | Kamernummer: " WP-Kamernummer(WerkplekkenTeller)
           END-PERFORM.

       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8) TO DatumVandaag.

       SorteerReserveringen.
           SORT WerkBestand
           ON ASCENDING KEY Datum USING ReserveringenBestand
                                 GIVING ReserveringenBestandSorted.

       BoekReserveringenVandaag.
           *> Werkplekkenteller initialiseren
           MOVE 1 TO WerkplekkenTeller
           *> Bestanden openenen
           OPEN INPUT ReserveringenBestandSorted
           OPEN OUTPUT ReserveringenBestand
           *> Als alle bestanden goed geopend zijn...
           IF (IO-OK)
               SET NotEndOfData TO TRUE
               *> Eerste reservering uitlezen...
               READ ReserveringenBestandSorted
                   AT END
                       SET EndOfData TO TRUE
               END-READ
               *> ...en voor elke reservering...
               PERFORM UNTIL EndOfData
                   *> ...indien het een reservering voor vandaag betreft...
                   IF (DatumVandaag EQUALS RBS-Datum AND AantalWerkplekken > 0)
                       *> BoekingenBestand pas openen als de eerste reservering gevonden is
                       IF (WerkplekkenTeller EQUALS 1)
                           DISPLAY SPACE
                           OPEN OUTPUT BoekingenBestand
                       END-IF
                       *> Data klaarzetten in working storage (boeking)
                       MOVE RBS-Naam TO BoekingsNaam
                       MOVE WP-Huisnummer(WerkplekkenTeller) TO BoekingsHuisnummer
                       MOVE WP-Kamernummer(WerkplekkenTeller) TO BoekingsKamernummer
                       SET SleutelNietUitgereikt TO TRUE
                       *> Record wegschrijven
                       WRITE BoekingenRecord FROM Boeking
                           INVALID KEY
                               DISPLAY ">>> NIET GELUKT. FileStatus: " IOStatus
                               DISPLAY SPACE
                       END-WRITE
                       *> Verslag uitbrengen
      *                DISPLAY "Huisnummer: " BoekingsHuisnummer " | Kamernummer: " BoekingsKamernummer " gaat naar " BoekingsNaam
                       *> Tellers updaten
                       ADD 1 TO WerkplekkenTeller
                       SUBTRACT 1 FROM AantalWerkplekken
                   ELSE
                       *> Overige reserveringen terugschrijven naar het reserveringenbestand
                       WRITE ReserveringenRecord FROM ReserveringenRecordSorted
                   END-IF
                   *> Volgende reservering uitlezen...
                   READ ReserveringenBestandSorted
                       AT END
                           SET EndOfData TO TRUE
                   END-READ
               END-PERFORM
               CLOSE ReserveringenBestandSorted, BoekingenBestand, ReserveringenBestand
               IF (WerkplekkenTeller EQUALS 1)
                   DISPLAY SPACE
                   DISPLAY "Er waren geen reserveringen voor vandaag."
               ELSE
                   *> Overzichten printen
      *            PERFORM ToonReserveringen
                   PERFORM ToonBoekingen
               END-IF
           ELSE
               DISPLAY "Er is iets mis gegaan. IO-Status: " IOStatus
           END-IF.

       ToonReserveringen.
           DISPLAY SPACE
           DISPLAY "RESERVERINGEN"
           DISPLAY "============="
           OPEN INPUT ReserveringenBestand
           *> Als alle bestanden goed geopend zijn...
           IF (IO-OK)
               SET NotEndOfData TO TRUE
               *> Eerste reservering uitlezen...
               READ ReserveringenBestand
                   AT END
                       SET EndOfData TO TRUE
               END-READ
               *> ...en voor elke reservering...
               PERFORM UNTIL EndOfData
                   MOVE RB-Datum TO DatumAlsInteger
                   COMPUTE DatumVerschil = FUNCTION INTEGER-OF-DATE (DatumAlsInteger) - FUNCTION INTEGER-OF-DATE (DatumVandaag)
                   IF (DatumVerschil EQUALS ZERO)
                       ADD 1 TO AantalReserveringenVandaag
                       DISPLAY ">>> | " RB-Dag "-" RB-Maand "-" RB-Jaar " | " RB-Naam
                   ELSE
                       DISPLAY "    | " RB-Dag "-" RB-Maand "-" RB-Jaar " | " RB-Naam
                   END-IF
                   *> Volgende reservering uitlezen...
                   READ ReserveringenBestand
                       AT END
                           SET EndOfData TO TRUE
                   END-READ
               END-PERFORM
               CLOSE ReserveringenBestand
               DISPLAY SPACE
               DISPLAY "Aantal reserveringen voor vandaag: " AantalReserveringenVandaag
           ELSE
               DISPLAY "Er is iets mis gegaan. IO-Status: " IOStatus
           END-IF.

       ToonBoekingen.
           DISPLAY SPACE
           DISPLAY "BOEKINGEN"
           DISPLAY "========="
           OPEN INPUT BoekingenBestand
           *> Als alle bestanden goed geopend zijn...
           IF (IO-OK)
               SET NotEndOfData TO TRUE
               *> Eerste boeking uitlezen...
               READ BoekingenBestand NEXT RECORD
                   AT END
                       SET EndOfData TO TRUE
               END-READ
               *> ...en voor elke boeking...
               PERFORM UNTIL EndOfData
                   ADD 1 TO BoekingenTeller
                   DISPLAY BoekingenTeller ". Huisnummer: " BB-Huisnummer WITH NO ADVANCING
                   DISPLAY " | Kamernummer: " BB-Kamernummer WITH NO ADVANCING
                   DISPLAY " | Naam: " BB-Naam WITH NO ADVANCING
                   IF (BB-SleutelUitgereikt)
                       DISPLAY " | Sleutel uitgereikt"
                   ELSE
                       DISPLAY " | Sleutel nog niet uitgereikt"
                   END-IF
                   *> Volgende boeking uitlezen...
                   READ BoekingenBestand NEXT RECORD
                       AT END
                           SET EndOfData TO TRUE
                   END-READ
               END-PERFORM
               CLOSE BoekingenBestand
           ELSE
               DISPLAY "Er is iets mis gegaan. IO-Status: " IOStatus
           END-IF
           .

       ToonDagomzet.
           DISPLAY SPACE
           DISPLAY "Dagomzet: " (AantalReserveringenVandaag * 15) " euro."
           .