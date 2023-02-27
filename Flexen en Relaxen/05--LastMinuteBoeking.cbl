       IDENTIFICATION DIVISION.
       PROGRAM-ID. 05--LastMinuteBoeking.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 31-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT BoekingenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Boekingen.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BB-Werkpleknummer
           ALTERNATE RECORD KEY IS BB-Naam WITH DUPLICATES
           FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       *> Boekingenbestand (wordt overschreven met de reserveringen van vandaag)
       FD BoekingenBestand.
       COPY Boeking REPLACING ==(pf)== BY ==BB==.

       WORKING-STORAGE SECTION.
       *> CONSTANTEN
       78 AantalHuisjes VALUE 6.
       *> IO STATUS VLAG
       01 IOStatus PIC XX.
         88 IO-OK VALUE ZEROES.
         88 ErIsNogEenRecord VALUE "02".
       *> END OF DATA VLAG
       01 EndOfDataFlag PIC 9 VALUE ZERO.
         88 EndOfData VALUE 1.
         88 NotEndOfData VALUE 0.
       *> NAAM VALIDATIE VLAG
       01 NameValidationFlag PIC 9 VALUE ZERO.
         88 NameFound VALUE 1.
         88 NameNotFound VALUE 0.
       *> MUTATIE VOTLTOOID VLAG
       01 MutationDoneFlag PIC 9 VALUE ZERO.
         88 MutatieVoltooid VALUE 1.
         88 MutatieNogNietVoltooid VALUE 0.
       *> STOP PROGRAMMA VLAG
       01 StopProgramFlag PIC 9 VALUE ZERO.
         88 UserWantsToStopTheProgram VALUE 1.
         88 ContinueProgram VALUE 0.
       *> KLAAR MET ZOEKEN VLAG
       01 DoneSearchingFlag PIC 9 VALUE ZERO.
         88 WerkplekGevonden VALUE 1.
         88 BlijfZoeken VALUE 0.
       *> BOEKING
       COPY Boeking REPLACING ==(pf)== BY ==WS==.

       01 BoekingenTeller PIC 99 VALUE ZERO.
       01 TotaalAantalBoekingen PIC 99 VALUE ZERO.
       *> WERKPLEK
       01 Werkplek OCCURS 1 TO 100 TIMES DEPENDING ON TotaalAantalWerkplekken.
         02 WP-Huisnummer PIC 9(2).
         02 WP-Kamernummer PIC 9.
         02 WP-Bezet PIC X(15) VALUE "---".
       01 TotaalAantalWerkplekken PIC 99 VALUE ZERO.
       01 AantalWerkplekkenBezet PIC 99 VALUE ZERO.
       01 WerkplekkenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       *> WONING
       01 Woning OCCURS 10 TIMES.
         02 WO-Huisnummer PIC 9(2).
         02 WO-Type PIC X.
         02 WO-AantalKamers PIC 9.
       01 AantalWoningen PIC 99 VALUE ZERO.
       01 WoningenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       *> OVERIG
       01 Iteraties PIC 99 VALUE ZERO.
       01 JaOfNee PIC X VALUE "n".

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM InitialiseerWoningen
           PERFORM InitialiseerWerkplekken
           PERFORM UNTIL UserWantsToStopTheProgram
               PERFORM ToonBoekingen
               PERFORM ToonWerkplekken
               PERFORM GetReserveringsNaam
               IF (WS-Naam NOT EQUALS "xxx")
                   MOVE ZERO TO Iteraties
                   PERFORM GeefSleutelUit
               ELSE
                   SET UserWantsToStopTheProgram TO TRUE
               END-IF
           END-PERFORM
           DISPLAY SPACE
           STOP RUN.

       GetReserveringsNaam.
           DISPLAY SPACE
           DISPLAY "SLEUTEL UITGIFTE"
           DISPLAY "================"
           DISPLAY "Naam: " WITH NO ADVANCING
           ACCEPT WS-Naam
           MOVE WS-Naam TO BB-Naam.

       GeefSleutelUit.
           *> Initialiseer continueringsvariabelen
           SET NotEndOfData TO TRUE
           SET MutatieNogNietVoltooid TO TRUE
           *> Open het bestand
           OPEN I-O BoekingenBestand
           *> Plaats de pointer bij het eerste record met deze boekingsnaam
           START BoekingenBestand
             KEY IS >= BB-Naam
               INVALID KEY
                   SET EndOfData TO TRUE
               NOT INVALID KEY
                   *> Lees het eerste record
                   PERFORM LeesBoeking
           END-START
           *> Voor elke gelezen record...
           PERFORM UNTIL EndOfData OR MutatieVoltooid
               *> ...checken of de naam klopt
               IF (BB-Naam EQUALS WS-Naam)
                   SET NameFound TO TRUE
                   IF (BB-SleutelNietUitgereikt)
                       PERFORM ZetWerkplekOpBezet
                       *> Sleutel uitreiken
                       SET BB-SleutelUitgereikt TO TRUE
                       SET MutatieVoltooid TO TRUE
                       REWRITE BB-Boeking
                           INVALID KEY
                               DISPLAY "Er ging iets fout. File status: " IOStatus
                       END-REWRITE
                   END-IF
               ELSE
                   SET EndOfData TO TRUE
               END-IF
               *> Volgende boeking uitlezen...
               IF (NotEndOfData AND MutatieNogNietVoltooid)
                   PERFORM LeesBoeking
               END-IF
           END-PERFORM
           IF (NameFound AND MutatieNogNietVoltooid OR NameNotFound)
               DISPLAY SPACE
               DISPLAY ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
               DISPLAY "Helaas. We hebben geen werkplek beschikbaar voor " WS-Naam
               DISPLAY "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
               DISPLAY SPACE
               IF (TotaalAantalBoekingen < TotaalAantalWerkplekken)
                   DISPLAY "Wilt u een last minute boeking doen? (j/n) " WITH NO ADVANCING
                   ACCEPT JaOfNee
                   IF (JaOfNee EQUALS "j")
                       PERFORM ZoekWerkplek
                   END-IF
               ELSE
                   DISPLAY "Bovendien zijn alle werkplekken nu bezet."
               END-IF
           END-IF
           CLOSE BoekingenBestand.

       ToonBoekingen.
           MOVE ZERO TO BoekingenTeller
           MOVE ZERO TO AantalWerkplekkenBezet
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
                       ADD 1 TO AantalWerkplekkenBezet
                       PERFORM ZetWerkplekOpBezet
                   ELSE
                       DISPLAY " | ---"
                       PERFORM ZetWerkplekOpGereserveerd
                   END-IF
                   *> Volgende boeking uitlezen...
                   READ BoekingenBestand NEXT RECORD
                       AT END
                           SET EndOfData TO TRUE
                   END-READ
               END-PERFORM
               CLOSE BoekingenBestand
           ELSE
               DISPLAY "Er is iets mis gegaan 2. IO-Status: " IOStatus
           END-IF
           MOVE BoekingenTeller TO TotaalAantalBoekingen
           DISPLAY SPACE
           DISPLAY "============================================="
           DISPLAY "Totaal aantal werkplekken: " TotaalAantalWerkplekken
           DISPLAY "Totaal aantal boekingen: " TotaalAantalBoekingen
           DISPLAY "Aantal werkplekken vrij: "(TotaalAantalWerkplekken - TotaalAantalBoekingen)
      *    DISPLAY "Aantal sleutels uitgereikt: " AantalWerkplekkenBezet
      *    DISPLAY "Aantal werkplekken gereserveerd: " (TotaalAantalBoekingen - AantalWerkplekkenBezet)
           DISPLAY "=============================================".

       LeesBoeking.
           READ BoekingenBestand NEXT RECORD
               AT END
                   SET EndOfData TO TRUE
               NOT AT END
                   ADD 1 TO Iteraties
           END-READ.

       InitialiseerWoningen.
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
                   ADD 1 TO TotaalAantalWerkplekken
                   MOVE WoningenTeller TO WP-Huisnummer(TotaalAantalWerkplekken)
                   MOVE WerkplekkenTeller TO WP-Kamernummer(TotaalAantalWerkplekken)
                   MOVE "---" TO WP-Bezet(TotaalAantalWerkplekken)
               END-PERFORM
           END-PERFORM.

       ToonWerkplekken.
           DISPLAY SPACE
           DISPLAY "WERKPLEKKEN"
           DISPLAY "==========="
           PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > TotaalAantalWerkplekken
               DISPLAY WerkplekkenTeller ". " WITH NO ADVANCING
               DISPLAY "Huisnummer: " WP-Huisnummer(WerkplekkenTeller) " | " WITH NO ADVANCING
               DISPLAY "Kamernummer: " WP-Kamernummer(WerkplekkenTeller) " | " WITH NO ADVANCING
               DISPLAY "Bezet: " WP-Bezet(WerkplekkenTeller)
           END-PERFORM.

       ZoekWerkplek.
           PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > TotaalAantalWerkplekken OR WerkplekGevonden
               IF (WP-Bezet(WerkplekkenTeller) EQUALS "---")
                   SET WerkplekGevonden TO TRUE
                   DISPLAY "Kamer " WP-Huisnummer(WerkplekkenTeller) "|" WP-Kamernummer(WerkplekkenTeller) " is nog vrij."
                   DISPLAY "Zullen we die voor u inboeken? (j/n) " WITH NO ADVANCING
                   ACCEPT JaOfNee
                   IF (JaOfNee EQUALS "j")
                       MOVE WP-Huisnummer(WerkplekkenTeller) TO WS-Huisnummer
                       MOVE WP-Kamernummer(WerkplekkenTeller) TO WS-Kamernummer
                       SET WS-SleutelUitgereikt TO TRUE
                       OPEN I-O BoekingenBestand
                       WRITE BB-Boeking FROM WS-Boeking
                       CLOSE BoekingenBestand
                       ADD 1 TO AantalWerkplekkenBezet
                       MOVE WS-Naam TO WP-Bezet(WerkplekkenTeller)
                   END-IF
               END-IF
           END-PERFORM
           SET BlijfZoeken TO TRUE.

       ZetWerkplekOpBezet.
           PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > TotaalAantalWerkplekken
               IF (WP-Huisnummer(WerkplekkenTeller) EQUALS BB-Huisnummer)
                 AND (WP-Kamernummer(WerkplekkenTeller) EQUALS BB-Kamernummer)
                   MOVE BB-Naam TO WP-Bezet(WerkplekkenTeller)
               END-IF
           END-PERFORM.

       ZetWerkplekOpGereserveerd.
           PERFORM VARYING WerkplekkenTeller FROM 1 BY 1 UNTIL WerkplekkenTeller > TotaalAantalWerkplekken
               IF (WP-Huisnummer(WerkplekkenTeller) EQUALS BB-Huisnummer)
                 AND (WP-Kamernummer(WerkplekkenTeller) EQUALS BB-Kamernummer)
                   MOVE "Gereserveerd" TO WP-Bezet(WerkplekkenTeller)
               END-IF
           END-PERFORM.
