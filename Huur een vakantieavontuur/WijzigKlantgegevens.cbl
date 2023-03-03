       IDENTIFICATION DIVISION.
       PROGRAM-ID. WijzigKlantgegevens.
       AUTHOR. Joey Schmitz en Michaël Koning.
       DATE-WRITTEN. 03-03-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT KlantenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Klanten.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FS-K-Klantnummer
                  FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       FD KlantenBestand.
       01 Klantrecord.
       COPY Klant REPLACING ==(pf)== BY ==FS-K==.

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 RedenValidatieFout PIC X(100) VALUE SPACES.
       01 GeboorteDatum.
         03 Jaar PIC 9(4).
         03 Maand PIC 99.
         03 Dag PIC 99.
       01 TelefoonnummerFormat.
         03 Landcode PIC 9(4).
         03 Telefoonnummer PIC 9(10).
       01 KeuzeVlag PIC 9 VALUE 1.
         88 VerlaatHetProgramma VALUE ZERO.
         88 BlijfInHetMenu VALUE 1.
       01 Keuze PIC 99 VALUE ZERO.
       01 Invoerklantnummer PIC 9(8) VALUE ZERO.


       PROCEDURE DIVISION.
       BeginProgram.
           *> Vraag 1: welke klant?
           DISPLAY SPACES
           DISPLAY "Van welke klant wilt u de gegevens wijzigen? (Klantnummer) " WITH NO ADVANCING
           ACCEPT InvoerKlantnummer
           *> Validatie 1: bestaat klant?
           OPEN I-O KlantenBestand.
           MOVE InvoerKlantnummer TO FS-K-Klantnummer
           READ KlantenBestand KEY IS FS-K-Klantnummer
               INVALID KEY
                   MOVE "Dit klantnummer bestaat niet." TO RedenValidatieFout
                   PERFORM ValidatieNietGeslaagd
           END-READ
           *> Display klantgegevens
           DISPLAY SPACES
           DISPLAY "Klantnummer: " FS-K-Klantnummer
           DISPLAY "Klantnaam: " FUNCTION TRIM(FS-K-Voornaam) " " FUNCTION TRIM(FS-K-Naam)
           DISPLAY "Adres: " FUNCTION TRIM(FS-K-Straat) FS-K-Huisnummer ", " WITH NO ADVANCING
           DISPLAY FUNCTION TRIM(FS-K-Postcode) " " FUNCTION TRIM(FS-K-Woonplaats)
           MOVE FS-K-Geboortedatum TO GeboorteDatum
           DISPLAY "Geboortedatum: " Dag "-" Maand "-" Jaar
           DISPLAY "Telefoonnummer: " FS-K-Telefoonnummer
           DISPLAY "E-mail: " FS-K-Emailadres
           *> Vraag 2: Juiste klant?
           PERFORM UNTIL VerlaatHetProgramma
               DISPLAY SPACE
               DISPLAY "1. Klantgegevens wijzigen"
               DISPLAY "2. Ander klantnummer invoeren"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 1
                       PERFORM KlantgegevensWijzigen
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN 2
                       CALL "WijzigKlantgegevens"
                   WHEN 47
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                         TO TRUE
               END-EVALUATE
           END-PERFORM
           CLOSE KlantenBestand
           EXIT PROGRAM.

       ValidatieNietGeslaagd.
           DISPLAY "Helaas. De validatie is niet geslaagd. " RedenValidatieFout
           CLOSE KlantenBestand
           EXIT PROGRAM.

       KlantgegevensWijzigen.
           SET BlijfInHetMenu TO TRUE
           PERFORM UNTIL VerlaatHetProgramma
               DISPLAY SPACE
               DISPLAY "1. Wijzig telefoonnummer"
               DISPLAY "2. Wijzig e-mail"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 1
                       PERFORM WijzigTelefoonnummer
                       SET VerlaatHetProgramma TO TRUE
                   WHEN 2
                       PERFORM WijzigEmail
                       SET VerlaatHetProgramma TO TRUE
                   WHEN 47
                       SET VerlaatHetProgramma
                         TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                         TO TRUE
               END-EVALUATE
           END-PERFORM
           CLOSE KlantenBestand
           EXIT PROGRAM.

       WijzigTelefoonnummer.
           DISPLAY "Wat wordt het nieuwe telefoonnummer? " WITH NO ADVANCING
           ACCEPT FS-K-Telefoonnummer
           REWRITE Klantrecord
           CLOSE KlantenBestand
           DISPLAY "De wijziging is doorgevoerd."
           DISPLAY SPACES
           EXIT PROGRAM.

       WijzigEmail.
           DISPLAY "Wat wordt het nieuwe mailadres? " WITH NO ADVANCING
           ACCEPT FS-K-Emailadres
           REWRITE Klantrecord
           CLOSE KlantenBestand
           DISPLAY "De wijziging is doorgevoerd."
           DISPLAY SPACES
           EXIT PROGRAM.


           

