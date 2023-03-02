       IDENTIFICATION DIVISION.
       PROGRAM-ID. Main.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 28-02-2023.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 KeuzeVlag PIC 9 VALUE 1.
         88 VerlaatHetProgramma VALUE ZERO.
         88 BlijfInHetMenu VALUE 1.
       01 Keuze PIC 99 VALUE ZERO.

       01 DatumVandaag PIC 9(8) VALUE ZERO.


       PROCEDURE DIVISION.
       BeginProgram.
           *>>>>>> TODO: Weghalen zodra in productie
           CALL "Bestandsinitialisatie"
           *>>>>>> END TODO
           PERFORM GetDatumVandaag
           PERFORM UNTIL VerlaatHetProgramma
               DISPLAY SPACE
               DISPLAY "1. Bestandsinitialisatie"
               DISPLAY "2. Toevoegen Klant"
               DISPLAY "3. Toevoegen Reservering"
               DISPLAY "4. Bezettingsoverzicht"
               DISPLAY "5. Berekening Huuromzet"
               DISPLAY "6. Annuleer Boeking"
               DISPLAY "7. Annuleer Reservering"
               DISPLAY "8. Betaal Reservering"
               DISPLAY "9. Verwerk mutaties"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 1
                       CALL "Bestandsinitialisatie"
                   WHEN 2
                       CALL "ToevoegenKlant"
                   WHEN 3
                       CALL "ToevoegenReservering"
                   WHEN 4
                       CALL "BezettingsOverzicht"
                   WHEN 5
                       CALL "BerekeningHuuromzet"
                   WHEN 6
                       CALL "AnnuleerBoeking"
                   WHEN 7
                       CALL "AnnuleerReservering"
                   WHEN 8
                       CALL "BetaalReservering"
                   WHEN 9
                       CALL "VerwerkMutatie"
                   WHEN 47
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                         TO TRUE
               END-EVALUATE
           END-PERFORM

           STOP RUN.

       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8)
             TO DatumVandaag.


           

