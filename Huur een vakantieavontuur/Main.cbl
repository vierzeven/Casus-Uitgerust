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
       01 Reserveringsnummer PIC 9(8) VALUE ZEROES.

       PROCEDURE DIVISION.
       BeginProgram.
      *>> >>>> TODO: Weghalen zodra in productie
           CALL "Bestandsinitialisatie"
             *>> >>>> END TODO
           CALL "BezettingsOverzicht"
           PERFORM UNTIL VerlaatHetProgramma
               DISPLAY SPACE
               DISPLAY "0. Bestandsinitialisatie"
               DISPLAY "1. Toevoegen Klant"
               DISPLAY "2. Toevoegen Reservering"
               DISPLAY "3. Betaal Reservering (*)"
               DISPLAY "4. Last Minute Boeking"
               DISPLAY "5. Bezettingsoverzicht"
               DISPLAY "6. Berekening Huuromzet"
               DISPLAY "7. Annuleer Reservering (*)"
               DISPLAY "8. Annuleer Boeking"
               DISPLAY "9. Verwerk mutaties"
               DISPLAY "10. Plan onderhoud woning"
               DISPLAY "11. Plan verkoop woning"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 0
                       CALL "Bestandsinitialisatie"
                   WHEN 1
                       CALL "ToevoegenKlant"
                   WHEN 2
                       CALL "ToevoegenReservering" USING BY REFERENCE Reserveringsnummer
                       DISPLAY "De reservering is opgeslagen."
                       CALL "BezettingsOverzicht"
                   WHEN 3
                       CALL "BetaalReservering"
                   WHEN 4
                       CALL "LastMinuteBoeking"
                   WHEN 5
                       CALL "BezettingsOverzicht"
                   WHEN 6
                       CALL "BerekeningHuuromzet"
                   WHEN 7
                       CALL "AnnuleerReservering"
                   WHEN 8
                       CALL "AnnuleerBoeking"
                   WHEN 9
                       CALL "VerwerkMutatie"
                   WHEN 10
                       CALL "PlanOnderhoud"
                   WHEN 11
                       CALL "PlanVerkoop"
                   WHEN 47
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                         TO TRUE
               END-EVALUATE
           END-PERFORM

           STOP RUN.


           

