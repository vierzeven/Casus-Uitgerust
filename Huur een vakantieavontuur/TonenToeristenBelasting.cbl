       IDENTIFICATION DIVISION.
       PROGRAM-ID. TonenToeristenBelasting.
       AUTHOR. Max en Ani.
       DATE-WRITTEN. 02-03-2023.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       Currency Sign "E" with Picture Symbol '$'.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ReserveringenBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\ReserveringenMetAltKey.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-R-Reserveringsnummer
           ALTERNATE KEY IS FS-R-Woningnummer
           WITH DUPLICATES
           FILE STATUS IS IOStatus.

       SELECT BewonersBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Bewoners.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-B-BewonersID
           FILE STATUS IS IOStatus.

       SELECT DatumWeekBestand ASSIGN TO "C:\COBOL\DATA\HUUR\DatumWeek.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.

       DATA DIVISION.
       FILE SECTION.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.
       FD BewonersBestand.
       01 Bewonersrecord.
       COPY Bewoner REPLACING ==(pf)== BY ==FS-B==.
       FD DatumWeekBestand.
       01 DatumWeekRecord.
         03 WeekNummer PIC 99.
         03 WeekDatum PIC 9(8).

       WORKING-STORAGE SECTION.
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 EOFReserveringenVlag PIC 99 VALUE 1.
         88 EOFReserveringen VALUE 0.
         88 NotEOFReserveringen VALUE 1.
       01 EOFBewonersVlag PIC 99 VALUE 1.
         88 EOFBewoners VALUE 0.
         88 NotEOFBewoners VALUE 1.
       01 EOFDatumWeekVlag PIC 99 VALUE 1.
         88 EOFDatumWeek VALUE 0.
         88 NotEOFDatumWeek VALUE 1.

       01 Week PIC 9(2).
       01 BeginDatumWeek PIC 9(8).
       01 EindDatumWeek PIC 9(8).
       01 WS-WeekDatum PIC 9(8).
       01 WS-INT-WeekDatum PIC 9(8).

       01 BeginDatumReservering PIC 9(8).
       01 EindDatumReservering PIC 9(8).

       01 WS-Geboortedatum PIC 9(8).
       01 WS-INT-Geboortedatum PIC 9(8).
       01 WS-INT-Leeftijd PIC 999.

       01 ToeristenBelasting PIC 999V99.
       01 ToeristenBelastingTotaal PIC 999V99.
       01 DisplayToeristenBelastingTotaal PIC 9(6),9999.

       PROCEDURE DIVISION.

           DISPLAY "Voor welke week wil je de toeristen belasting zien (geef weeknummer): " WITH NO ADVANCING
           ACCEPT Week

           OPEN INPUT ReserveringenBestand
           IF NOT IO-OK
               DISPLAY "Sorry, het openen van het reserveringen bestand ging mis: " IOSTatus
           END-IF

           READ ReserveringenBestand NEXT RECORD
               AT END
                   SET EOFReserveringen TO TRUE
           END-READ

           PERFORM UNTIL EOFReserveringen
               IF (Week EQUALS FS-R-Weeknummer AND FS-R-ReserveringsType EQUALS "B")

                   OPEN INPUT BewonersBestand
                   IF NOT IO-OK
                       DISPLAY "Sorry, het openen van het bewoners bestand ging mis: " IOSTatus
                   END-IF

                   READ BewonersBestand NEXT RECORD
                       AT END
                           SET EOFBewoners TO TRUE
                   END-READ

                   PERFORM UNTIL EOFBewoners EQUALS TRUE
                       IF (FS-B-Reserveringsnummer EQUALS FS-R-Reserveringsnummer)
                           MOVE FS-B-Geboortedatum TO WS-Geboortedatum
                           MOVE FUNCTION INTEGER-OF-DATE (WS-Geboortedatum) TO WS-INT-Geboortedatum

                           OPEN INPUT DatumWeekBestand
                           READ DatumWeekBestand
                               AT END
                                   SET EOFDatumWeek TO TRUE
                           END-READ

                           SET NotEOFDatumWeek TO TRUE
                           PERFORM UNTIL EOFDatumWeek EQUALS TRUE

                               IF FS-R-Weeknummer EQUALS WeekNummer
                                   MOVE WeekDatum TO WS-WeekDatum
                                   MOVE FUNCTION INTEGER-OF-DATE (WS-WeekDatum) TO WS-INT-WeekDatum
                                   SET EOFDatumWeek TO TRUE
                                   COMPUTE WS-INT-Leeftijd EQUALS (WS-INT-WeekDatum - WS-INT-Geboortedatum) / 365.25
                                   DISPLAY "Leeftijd is: " WS-INT-Leeftijd

                                   PERFORM ToeristenBelastingBerekenen

                               END-IF

                               READ DatumWeekBestand
                                   AT END
                                       SET EOFDatumWeek TO TRUE
                               END-READ

                           END-PERFORM
                           CLOSE DatumWeekBestand
                       END-IF

                       READ BewonersBestand NEXT RECORD
                           AT END
                               SET EOFBewoners TO TRUE
                       END-READ

                   END-PERFORM
                   CLOSE BewonersBestand
               END-IF

               READ ReserveringenBestand NEXT RECORD
                   AT END
                       SET EOFReserveringen TO TRUE
               END-READ

           END-PERFORM

           CLOSE ReserveringenBestand
             
           DISPLAY "De totale toeristenbelasting voor week " Week " is " DisplayToeristenBelastingTotaal " euro."
       EXIT PROGRAM.

       ToeristenBelastingBerekenen.
           IF WS-INT-Leeftijd >= 18
               COMPUTE ToeristenBelasting EQUALS FS-R-AantalWeken * 7 * 1.5
           END-IF
           IF WS-INT-Leeftijd <= 5
                   COMPUTE ToeristenBelasting EQUALS FS-R-AantalWeken * 7 * 0
           END-IF
           IF WS-INT-Leeftijd >5 AND WS-INT-Leeftijd < 18
                   COMPUTE ToeristenBelasting EQUALS FS-R-AantalWeken * 7 * 1
           END-IF

               ADD ToeristenBelasting TO ToeristenBelastingTotaal
           MOVE ToeristenBelastingTotaal TO DisplayToeristenBelastingTotaal.
       

