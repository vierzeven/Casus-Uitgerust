       IDENTIFICATION DIVISION.
       PROGRAM-ID. ToevoegenReservering.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 27-02-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT KlantenBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Klanten.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-K-Klantnummer
           FILE STATUS IS IOStatus.
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
       SELECT MutatieBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Mutaties.dat"
           ORGANIZATION IS LINE SEQUENTIAL
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
       FD KlantenBestand.
       01 Klantrecord.
       COPY Klant REPLACING ==(pf)== BY ==FS-K==.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.
       FD BewonersBestand.
       01 Bewonersrecord.
       COPY Bewoner REPLACING ==(pf)== BY ==FS-B==.
       FD WoningenBestand.
       01 Woningrecord.
         03 Woningnummer PIC 99 VALUE ZERO.
         03 Woningtype PIC X VALUE "S".
       FD SysteemkengetallenBestand.
       01 Systeemkengetallenrecord.
         03 HoogsteKlantennummer PIC 9(8) VALUE ZERO.
         03 HoogsteReserveringsnummer PIC 9(8) VALUE ZERO.
                FD MutatieBestand.
       01 Mutatierecord.
       COPY Mutatie REPLACING ==(pf)== BY ==FS-M==.
       WORKING-STORAGE SECTION.
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 DatumVandaag PIC 9(8) VALUE ZERO.
       01 AantalBewoners PIC 9 VALUE ZERO.
       01 Teller PIC 99 VALUE ZERO.
       01 HuisjesTeller PIC 99 VALUE ZERO.

       *>LINKAGE SECTION.
       *>01 Reserveringsnummer PIC 9(8) VALUE ZEROES.

       LINKAGE SECTION.
       COPY Reserveringsnummer REPLACING ==(pf)== BY ==LS==.

       PROCEDURE DIVISION USING LS-Reserveringsnummer.
         *>USING Reserveringsnummer.
       BeginProgram.
           DISPLAY SPACE
           OPEN I-O SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
           OPEN I-O ReserveringenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Reserveringen.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand
           ADD 1 TO HoogsteReserveringsnummer
           MOVE HoogsteReserveringsnummer
             TO FS-R-Reserveringsnummer, LS-Reserveringsnummer
           DISPLAY "Klantnummer: " WITH NO ADVANCING
           ACCEPT FS-R-Klantnummer
           DISPLAY "Woningnummer: " WITH NO ADVANCING
           ACCEPT FS-R-Woningnummer
           DISPLAY "Jaar: " WITH NO ADVANCING
           ACCEPT FS-R-Jaar
           IF FS-R-Jaar = ZERO
               MOVE DatumVandaag(1:4)
                 TO FS-R-Jaar
           END-IF
           DISPLAY "Weeknummer: " WITH NO ADVANCING
           ACCEPT FS-R-Weeknummer
           DISPLAY "Aantal weken: " WITH NO ADVANCING
           ACCEPT FS-R-AantalWeken
           DISPLAY "Datum creatie: " WITH NO ADVANCING
           ACCEPT FS-R-DatumCreatie
           IF FS-R-DatumCreatie = SPACE
               MOVE DatumVandaag
                 TO FS-R-DatumCreatie
           END-IF
           DISPLAY "Aantal bewoners: " WITH NO ADVANCING
           ACCEPT AantalBewoners
           PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > AantalBewoners
               OPEN I-O BewonersBestand
               MOVE FS-R-Reserveringsnummer
                 TO FS-B-Reserveringsnummer
               MOVE Teller TO FS-B-Volgnummer
               DISPLAY "Initialen gast " Teller ": " WITH NO ADVANCING
               ACCEPT FS-B-Initialen
               DISPLAY "Geboortedatum gast " Teller ": " WITH NO ADVANCING
               ACCEPT FS-B-Geboortedatum
               WRITE Bewonersrecord
               CLOSE BewonersBestand
           END-PERFORM
           WRITE Reserveringsrecord
           DISPLAY ">>> Writing Reservering. IOStatus: " IOStatus
           REWRITE Systeemkengetallenrecord
           DISPLAY ">>> Rewriting Systeemkengetallen. IOStatus: " IOStatus
           CLOSE SysteemkengetallenBestand
           DISPLAY ">>> Closing SysteemkengetallenBestand. IOStatus: " IOStatus
           CLOSE ReserveringenBestand
           DISPLAY ">>> Closing ReserveringenBestand. IOStatus: " IOStatus
           EXIT PROGRAM.
