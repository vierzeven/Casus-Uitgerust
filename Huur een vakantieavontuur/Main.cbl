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
               DISPLAY "00. Bestandsinitialisatie"
               DISPLAY "01. Toevoegen Klant"
               DISPLAY "02. Wijzig Klant"
               DISPLAY "03. Toevoegen Reservering"
               DISPLAY "04. Betaal Reservering (*)"
               DISPLAY "05. Last Minute Boeking"
               DISPLAY "06. Bezettingsoverzicht"
               DISPLAY "07. Berekening Huuromzet"
               DISPLAY "08. Annuleer Reservering"
               DISPLAY "09. Annuleer Boeking"
               DISPLAY "10. Verwerk mutaties"
               DISPLAY "11. Plan onderhoud woning"
               DISPLAY "12. Plan verkoop woning"
               DISPLAY "13. Tonen toeristenbelasting"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 0
                       CALL "Bestandsinitialisatie"
                       DISPLAY SPACES
                       DISPLAY "De bestanden zijn opnieuw geinitialiseerd met testdata."
                       CALL "BezettingsOverzicht"
                   WHEN 1
                       CALL "ToevoegenKlant"
                   WHEN 2
                       CALL "WijzigKlantgegevens"
                   WHEN 3
                       CALL "ToevoegenReservering" USING BY REFERENCE Reserveringsnummer
                       DISPLAY "De reservering is opgeslagen."
                       CALL "BezettingsOverzicht"
                   WHEN 4
                       PERFORM NogNietGebouwd
                   WHEN 5
                       CALL "LastMinuteBoeking"
                   WHEN 6
                       CALL "BezettingsOverzicht"
                   WHEN 7
                       CALL "BerekeningHuuromzet"
                   WHEN 8
                       CALL "AnnuleerBoeking"
                       CALL "BezettingsOverzicht"
                   WHEN 9
                       CALL "AnnuleerBoeking"
                       CALL "BezettingsOverzicht"
                   WHEN 10
                       CALL "VerwerkMutatie"
                   WHEN 11
                       CALL "PlanOnderhoud"
                   WHEN 12
                       CALL "PlanVerkoop"
                   WHEN 13
                       CALL "TonenToeristenBelasting"
                   WHEN 47
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                         TO TRUE
               END-EVALUATE
           END-PERFORM

           STOP RUN.

       NogNietGebouwd.
           DISPLAY SPACE
           ">>> Excuus. Deze functionaliteit is nog niet gerealiseerd."
           DISPLAY SPACE
           .


           

