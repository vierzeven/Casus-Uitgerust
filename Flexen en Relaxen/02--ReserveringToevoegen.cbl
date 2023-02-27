       IDENTIFICATION DIVISION.
       PROGRAM-ID. 02--ReserveringToevoegen.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 30-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ReserveringenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Reserveringen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.

       DATA DIVISION.
       FILE SECTION.
       FD ReserveringenBestand.
       01 ReserveringenRecord.
         02 RB-Naam PIC X(24).
         02 RB-Datum PIC 9(8).

       WORKING-STORAGE SECTION.
       01 MaxAantalDagenInToekomst PIC 99 VALUE 10.
       01 AantalReserveringenMaken PIC 99 VALUE ZERO.
       *> IO STATUS VLAG
       01 IOStatus PIC XX.
         88 IO-OK VALUE ZEROES.
       *> RESERVERING
       01 Reservering.
         02 ReserveringsNaam PIC X(24).
         02 ReserveringsDatum.
           03 Jaar PIC 9(4).
           03 Maand PIC 99.
             88 LangeMaand VALUE 1,3,5,7,8,10,12.
             88 KorteMaand VALUE 4,6,9,11.
             88 Februari VALUE 2.
           03 Dag PIC 99.
       *> DATUMCONTROLE
       01 CorrecteDatumVlag PIC 9 VALUE 1.
         88 DatumCorrect VALUE 1.
         88 DatumIncorrect VALUE 0.
       01 Foutmelding PIC X(100) VALUE SPACES.
       01 LeapYear PIC 9 VALUE 0.
         88 IsLeapYear VALUE 1.
         88 IsNotLeapYear VALUE 0.
       01 MaxAantalDagen PIC 99 VALUE ZEROES.
       01 RestDelingDoor4 PIC 999 VALUE ZEROES PACKED-DECIMAL.
       01 RestDelingDoor100 PIC 999 VALUE ZEROES PACKED-DECIMAL.
       01 RestDelingDoor400 PIC 999 VALUE ZEROES PACKED-DECIMAL.
       01 Maandnamen.
         03 FILLER PIC X(9) VALUE "januari".
         03 FILLER PIC X(9) VALUE "februari".
         03 FILLER PIC X(9) VALUE "maart".
         03 FILLER PIC X(9) VALUE "april".
         03 FILLER PIC X(9) VALUE "mei".
         03 FILLER PIC X(9) VALUE "juni".
         03 FILLER PIC X(9) VALUE "juli".
         03 FILLER PIC X(9) VALUE "augustus".
         03 FILLER PIC X(9) VALUE "september".
         03 FILLER PIC X(9) VALUE "oktober".
         03 FILLER PIC X(9) VALUE "november".
         03 FILLER PIC X(9) VALUE "december".
       01 FILLER REDEFINES Maandnamen.
         03 Maandnaam OCCURS 12 PIC X(9).
       01 DatumVandaag PIC 9(8).
       01 DatumAlsInteger PIC 9(8).

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM VraagNaam
           DISPLAY "Hoeveel reserveringen wilt u maken? " WITH NO ADVANCING
           ACCEPT AantalReserveringenMaken
           PERFORM AantalReserveringenMaken TIMES
               PERFORM VraagDatum
               PERFORM ValideerDatum
               IF (DatumCorrect)
                   PERFORM SchrijfReserveringWeg
               ELSE
                   DISPLAY Foutmelding
               END-IF
           END-PERFORM
           DISPLAY SPACE
           DISPLAY ">>> Einde programma <<<"
           DISPLAY SPACE
           STOP RUN.

       VraagNaam.
           DISPLAY "Naam (max 25 tekens): " WITH NO ADVANCING.
           ACCEPT ReserveringsNaam.

       VraagDatum.
           DISPLAY SPACE
           DISPLAY "Datum (yyyymmdd): " WITH NO ADVANCING
           ACCEPT ReserveringsDatum.

       SchrijfReserveringWeg.
           OPEN EXTEND ReserveringenBestand
           IF (IOStatus EQUALS 00 OR 05)
               WRITE ReserveringenRecord FROM Reservering
               CLOSE ReserveringenBestand
               DISPLAY SPACE
               DISPLAY "OPGESLAGEN RESERVERING"
               DISPLAY "======================"
               DISPLAY "Naam: " ReserveringsNaam
               DISPLAY "Datum:" ReserveringsDatum
           ELSE
               DISPLAY "Er heeft zich een fout voorgedaan. IO-Status: " IOStatus
           END-IF.

       ValideerDatum.
           PERFORM CheckMonth
           PERFORM CheckForLeapyear
           PERFORM CalculateMaxNumberOfDays
           PERFORM CheckDay
           PERFORM GetDatumVandaag
           IF (DatumCorrect)
               PERFORM CheckIfDateIsInThePast
               PERFORM CheckIfDateIsTooFarInTheFuture
           END-IF

           .

       CheckMonth.
           IF (Maand < 1)
               SET DatumIncorrect TO TRUE
               MOVE "Een correcte datum kan geen maand hebben die lager is dan 1." TO Foutmelding
           END-IF
           IF (Maand > 12)
               SET DatumIncorrect TO TRUE
               MOVE "Een correcte datum kan geen maand hebben die hoger is dan 12." TO Foutmelding
           END-IF.

       CheckForLeapyear.
           COMPUTE RestDelingDoor4 = (FUNCTION MOD (Jaar, 4))
           COMPUTE RestDelingDoor100 = (FUNCTION MOD (Jaar, 100))
           COMPUTE RestDelingDoor400 = (FUNCTION MOD (Jaar, 400))

           IF (RestDelingDoor4 = 0 AND RestDelingDoor100 <> 0 OR RestDelingDoor400 = 0)
               SET IsLeapYear TO TRUE
           ELSE
               SET IsNotLeapYear TO TRUE
           END-IF.

       CalculateMaxNumberOfDays.
           EVALUATE TRUE ALSO TRUE
               WHEN IsLeapYear ALSO Februari
                   MOVE 29 TO MaxAantalDagen
               WHEN IsNotLeapYear ALSO Februari
                   MOVE 28 TO MaxAantalDagen
               WHEN ANY ALSO LangeMaand
                   MOVE 31 TO MaxAantalDagen
               WHEN ANY ALSO KorteMaand
                   MOVE 30 TO MaxAantalDagen
               WHEN OTHER
                   MOVE 999 TO MaxAantalDagen
           END-EVALUATE.

       CheckDay.
           EVALUATE TRUE
               WHEN (Dag < 1)
                   SET DatumIncorrect TO TRUE
                   MOVE "Een correcte datum kan geen dag hebben die lager is dan 1." TO Foutmelding
               WHEN (Dag > MaxAantalDagen)
                   SET DatumIncorrect TO TRUE
                   IF (Februari)
                       STRING Foutmelding DELIMITED BY SPACES "De maand " FUNCTION TRIM (Maandnaam(Maand)) " van het jaar " Jaar " heeft maar " MaxAantalDagen " dagen." INTO Foutmelding
                   ELSE
                       STRING Foutmelding DELIMITED BY SPACES "De maand " FUNCTION TRIM (Maandnaam(Maand)) " heeft maar " MaxAantalDagen " dagen." INTO Foutmelding
                   END-IF
               WHEN OTHER
                   SET DatumCorrect TO TRUE
           END-EVALUATE.

       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8) TO DatumVandaag.

       CheckIfDateIsInThePast.
           MOVE ReserveringsDatum TO DatumAlsInteger
           IF (FUNCTION INTEGER-OF-DATE (DatumVandaag) - FUNCTION INTEGER-OF-DATE (DatumAlsInteger) IS POSITIVE)
               SET DatumIncorrect TO TRUE
               MOVE "Datum in het verleden!" TO Foutmelding
           END-IF
           .

       CheckIfDateIsTooFarInTheFuture.
           MOVE ReserveringsDatum TO DatumAlsInteger
           IF (FUNCTION INTEGER-OF-DATE (DatumAlsInteger) - FUNCTION INTEGER-OF-DATE (DatumVandaag) > MaxAantalDagenInToekomst)
               SET DatumIncorrect TO TRUE
               STRING "U kunt maximaal " MaxAantalDagenInToekomst " dagen in de toekomst reserveren!" INTO Foutmelding
           END-IF
           .

           *> Overzichten printen
           *> Opbrengst berekenen

