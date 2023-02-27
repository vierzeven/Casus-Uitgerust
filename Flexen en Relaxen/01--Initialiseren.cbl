       IDENTIFICATION DIVISION.
       PROGRAM-ID. 01--Initialiseren.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 30-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ReserveringenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Reserveringen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.
       SELECT BoekingenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Boekingen.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BB-Werkpleknummer
           ALTERNATE RECORD KEY IS BB-Naam WITH DUPLICATES
           FILE STATUS IS IOStatus.

       DATA DIVISION.
       FILE SECTION.
       FD ReserveringenBestand.
       01 ReserveringenRecord.
         02 RB-Naam PIC X(24).
         02 RB-Datum PIC 9(8).
       FD BoekingenBestand.
       01 BoekingenRecord.
         02 BB-Naam PIC X(24).
         02 BB-Datum PIC 9(8).
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
       *> RESERVERING
       01 Reservering.
         02 ReserveringsNaam PIC X(24).
         02 ReserveringsDatum PIC 9(8).
       *> BOEKING
       01 Boeking.
         02 BoekingsNaam PIC X(24).
         02 BoekingsDatum PIC 9(8).
         02 BoekingsHuisnummer PIC 9(2).
         02 BoekingsKamernummer PIC 9.
         02 SleutelUitgereiktVlag PIC X VALUE "N".
           88 SleutelUitgereikt VALUE "J".
           88 SleutelNietUitgereikt VALUE "N".
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

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM InitialseerWoningen.
           PERFORM ToonWoningen.
           PERFORM InitialiseerWerkplekken.
           PERFORM ToonWerkplekken.

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

           *> TODO: Reserveringen maken (met datumcontrole)
           *> TODO: Overzetten reserveringen naar boekingen (met sortering bestand en verwijdering geboekte reserveringen)
           *> Uitgifte sleutel op naam
