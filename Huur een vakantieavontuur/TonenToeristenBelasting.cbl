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

       DATA DIVISION.
       FILE SECTION.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.
       FD BewonersBestand.
       01 Bewonersrecord.
       COPY Bewoner REPLACING ==(pf)== BY ==FS-B==.

       WORKING-STORAGE SECTION.
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 EOF PIC 99 VALUE 0.
         88 NotEOF VALUE 1.
       
       01 Maand PIC 9(2).
       01 BeginDatumMaand PIC 9(8).
       01 EindDatumMaand PIC 9(8).

       01 BeginDatumReservering PIC 9(8).
       01 EindDatumReservering PIC 9(8).

       PROCEDURE DIVISION.

           DISPLAY "Over welke maand wil je de toeristen belasting zien (geef maandnummer): " WITH NO ADVANCING
           ACCEPT Maand

           *>COMPUTE FUNCTION INTEGER-OF-DATE(BeginDatumMaand) FUNCTION INTEGER-OF-DATE(EindDatumMaand)

           *>COMPUTE AantalDagenMaand = FUNCTION INTEGER-OF-DATE(BeginDatumMaand) - FUNCTION INTEGER-OF-DATE(EindDatumMaand)

           OPEN INPUT ReserveringenBestand
           IF NOT IO-OK
               DISPLAY "Sorry, het openen van het reserveringen bestand ging mis: " IOSTatus
           END-IF
           OPEN INPUT BewonersBestand
           IF NOT IO-OK
               DISPLAY "Sorry, het openen van het bewoners bestand ging mis: " IOSTatus
           END-IF

           READ ReserveringenBestand NEXT RECORD

           *>Vergelijking reservering met begin en einddatum maand

               AT END
                   SET EOF TO 0
           END-READ.

           



       EXIT PROGRAM.
