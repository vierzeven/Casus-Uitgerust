       IDENTIFICATION DIVISION.
       PROGRAM-ID. PlanOnderhoud.
       AUTHOR. Joey Schmitz en Michaël Koning.
       DATE-WRITTEN. 02-03-2023.
       ENVIRONMENT DIVISION.
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
       SELECT WoningenBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Woningen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.
       SELECT SysteemkengetallenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Systeemkengetallen.dat"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       FD SysteemkengetallenBestand.
       01 Systeemkengetallenrecord.
         03 HoogsteKlantennummer PIC 9(8) VALUE ZERO.
         03 HoogsteReserveringsnummer PIC 9(8) VALUE ZERO.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.
       FD WoningenBestand.
       01 Woningrecord.
         03 Woningnummer PIC 99 VALUE ZERO.
         03 Woningtype PIC X VALUE "S".

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 EndOfDataVlag PIC 9 VALUE ZERO.
         88 NotEOD VALUE ZERO.
         88 EOD VALUE 1.
       01 WoningnummerInput PIC 99.
       01 ValidatiefoutVlag PIC 9 VALUE ZERO.
         88 ValidatieGeslaagd VALUE ZERO.
         88 ValidatieFout VALUE 1.
       01 RedenValidatieFout PIC X(100) VALUE SPACES.
       01 Weeknummer PIC 99 VALUE ZERO.
       01 AantalWeken PIC 99 VALUE ZERO.
       01 DatumVandaag PIC 9(8) VALUE ZERO.


       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM GetDatumVandaag

           SET NotEOD TO TRUE

           *> Vraag 1: Welke woning?
           DISPLAY "Voor welke woning wilt u onderhoud plannen?"
           ACCEPT WoningnummerInput

           *> Validatie 1: bestaat de woning?
           OPEN INPUT WoningenBestand
           SET ValidatieFout TO TRUE
           MOVE "De gekozen woning komt niet voor in het woningenbestand." TO RedenValidatieFout
           READ WoningenBestand
               AT END
                   SET EOD TO TRUE
           END-READ
           PERFORM WITH TEST BEFORE UNTIL EOD OR ValidatieGeslaagd
               IF WoningnummerInput EQUALS Woningnummer
                   SET ValidatieGeslaagd TO TRUE
                   MOVE SPACES TO RedenValidatieFout
               END-IF
               READ WoningenBestand
                   AT END
                       SET EOD TO TRUE
               END-READ
           END-PERFORM
           CLOSE WoningenBestand
           IF ValidatieFout
               PERFORM ValidatieNietGeslaagd
           END-IF

           *> Vraag 2: welke week?
           DISPLAY "In welke week begint het onderhoud?"
           ACCEPT Weeknummer

           *> Validatie 2: week in seizoen?
           SET ValidatieFout TO TRUE
           MOVE "Onderhoud kan alleen gepland worden in week 18 t/m 37." TO RedenValidatieFout
           IF Weeknummer >= 18 AND Weeknummer <= 37
               SET ValidatieGeslaagd TO TRUE
           END-IF
           IF ValidatieFout
               PERFORM ValidatieNietGeslaagd
           END-IF
           MOVE SPACES TO RedenValidatieFout

           *> Vraag 3: hoeveel weken?
           DISPLAY "Hoeveel weken duurt het onderhoud?"
           ACCEPT AantalWeken

           *> TODO: Validaties implementeren
           *> Validatie 3: woning bezet?
           *> Validatie 4: past onderhoud in seizoen?
           *> Validatie 5: is woning niet verkocht?


           *> Systeemkengetallen bestand updaten
           OPEN I-O SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand
           ADD 1 TO HoogsteReserveringsnummer
           REWRITE Systeemkengetallenrecord
           CLOSE SysteemkengetallenBestand

           *> Reservering wegschrijven
           OPEN I-O ReserveringenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van ReserveringenBestand.dat: " IOStatus
           END-IF
           MOVE HoogsteReserveringsnummer TO FS-R-Reserveringsnummer
           MOVE WoningnummerInput TO FS-R-Woningnummer
           MOVE DatumVandaag TO FS-R-DatumCreatie
           MOVE "O" TO FS-R-ReserveringsType
           MOVE 2023 TO FS-R-Jaar
           MOVE Weeknummer TO FS-R-Weeknummer
           MOVE AantalWeken TO FS-R-AantalWeken
           WRITE Reserveringsrecord
           CLOSE ReserveringenBestand
           DISPLAY "Het onderhoud is gepland."
           CALL "BezettingsOverzicht"
           EXIT PROGRAM.

       ValidatieNietGeslaagd.
           DISPLAY "Helaas. De validatie is niet geslaagd. " RedenValidatieFout
           EXIT PROGRAM
           .

       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8)
             TO DatumVandaag.

