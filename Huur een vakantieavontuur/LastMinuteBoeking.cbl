       IDENTIFICATION DIVISION.
       PROGRAM-ID. LastMinuteBoeking.
       AUTHOR. Ani en Michael.
       DATE-WRITTEN. 28-02-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ReserveringenBestand ASSIGN TO "C:\COBOL\DATA\HUUR\ReserveringenMetAltKey.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FS-R-Reserveringsnummer
                  ALTERNATE KEY IS FS-R-Woningnummer
                  WITH DUPLICATES
                  FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.

       WORKING-STORAGE SECTION.
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.

       PROCEDURE DIVISION.

           CALL "ToevoegenReservering" USING BY REFERENCE FS-R-Reserveringsnummer

           OPEN I-O ReserveringenBestand
           READ ReserveringenBestand 
               KEY IS FS-R-Reserveringsnummer
           MOVE FS-R-DatumCreatie TO FS-R-DatumBetaling
           REWRITE Reserveringsrecord 
           CLOSE ReserveringenBestand
           DISPLAY "De last minute boeking is gelukt."
           CALL "BezettingsOverzicht"
       END PROGRAM LastMinuteBoeking.
       
