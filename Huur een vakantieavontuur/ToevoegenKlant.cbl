       PROGRAM-ID. Program2 as "Huur_een_vakantieavontuur.ToevoegenKlant".          

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       Currency Sign "E" with Picture Symbol '$'.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       SELECT SysteemkengetallenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Systeemkengetallen.dat"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS IOStatus.

       SELECT KlantenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Klanten.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FS-K-Klantnummer
                  FILE STATUS IS IOStatus.

       DATA DIVISION.
       FILE SECTION.

       FD SysteemkengetallenBestand.
       01 Systeemkengetallenrecord.
         03 HoogsteKlantennummer PIC 9(8) VALUE ZERO.
         03 HoogsteReserveringsnummer PIC 9(8) VALUE ZERO.

       FD KlantenBestand.
       01 Klantrecord.
       COPY Klant REPLACING ==(pf)== BY ==FS-K==.

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.

       PROCEDURE DIVISION.

           DISPLAY SPACE
           OPEN I-O SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
           OPEN I-O KlantenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Klanten.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand
           ADD 1 TO HoogsteKlantennummer
           MOVE HoogsteKlantennummer
             TO FS-K-Klantnummer
           DISPLAY "Achternaam: " WITH NO ADVANCING
           ACCEPT FS-K-Naam
           DISPLAY "Voornaam: " WITH NO ADVANCING
           ACCEPT FS-K-Voornaam
           DISPLAY "Straat: " WITH NO ADVANCING
           ACCEPT FS-K-Straat
           DISPLAY "Huisnummer: " WITH NO ADVANCING
           ACCEPT FS-K-Huisnummer
           DISPLAY "Postcode: " WITH NO ADVANCING
           ACCEPT FS-K-Postcode
           DISPLAY "Woonplaats: " WITH NO ADVANCING
           ACCEPT FS-K-Woonplaats
           DISPLAY "Geboortedatum: " WITH NO ADVANCING
           ACCEPT FS-K-Geboortedatum
           DISPLAY "Telefoonnummer: " WITH NO ADVANCING
           ACCEPT FS-K-Telefoonnummer
           DISPLAY "E-mail: " WITH NO ADVANCING
           ACCEPT FS-K-Emailadres
           WRITE Klantrecord
           REWRITE Systeemkengetallenrecord
           CLOSE SysteemkengetallenBestand
           CLOSE KlantenBestand.
           DISPLAY SPACES
           DISPLAY FUNCTION TRIM(FS-K-Voornaam) " " FUNCTION TRIM(FS-K-Naam) " is toegevoegd aan het klantenbestand."
       EXIT PROGRAM.
