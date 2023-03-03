       PROGRAM-ID. Program2 as "Huur_een_vakantieavontuur.Bestandsinitialisatie".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
              Currency Sign "E" with Picture Symbol '$'.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT WoningenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Woningen.dat"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS IOStatus.
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
       FD KlantenBestand.
       01 Klantrecord.
              COPY Klant REPLACING ==(pf)== BY ==FS-K==.
       FD ReserveringenBestand.
       01 Reserveringsrecord.
              COPY Reservering REPLACING ==(pf)== BY ==FS-R==.

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.

       *> Tabellen
       01 FILLER VALUE "10S11S12L13L14S15S16S17L18L19S".
         03 HuisjeInitieel PIC X(3) OCCURS 10.

       *> Tellers
       01 Huisjesteller PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.

           PERFORM ResetBewoners
           PERFORM VulWoningen
           PERFORM VulSysteemkengetallen
           PERFORM VulKlanten
           PERFORM VulReserveringen

           STOP RUN.

       ResetBewoners.
           OPEN OUTPUT BewonersBestand
           CLOSE BewonersBestand
           .

       VulWoningen.
           OPEN OUTPUT WoningenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Woningen.dat: " IOStatus
           END-IF
           PERFORM VARYING Huisjesteller FROM 1 BY 1 UNTIL Huisjesteller > 10
               MOVE HuisjeInitieel(Huisjesteller)
                 TO Woningrecord
               WRITE Woningrecord
           END-PERFORM
           CLOSE WoningenBestand.

       VulSysteemkengetallen.
           OPEN OUTPUT SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
           MOVE ZERO TO HoogsteKlantennummer
           MOVE ZERO TO HoogsteReserveringsnummer
           WRITE Systeemkengetallenrecord

           CLOSE SysteemkengetallenBestand.

       VulKlanten.
           OPEN OUTPUT KlantenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Klanten.dat: " IOStatus
           END-IF
           MOVE 1 TO FS-K-Klantnummer, HoogsteKlantennummer
           MOVE "Schmitz" TO FS-K-Naam
           MOVE "Joey" TO FS-K-Voornaam
           MOVE "Kempenaar 17" TO FS-K-Straat
           MOVE "81" TO FS-K-Huisnummer
           MOVE "8231EN" TO FS-K-Postcode
           MOVE "Lelystad" TO FS-K-Woonplaats
           MOVE "19790319" TO FS-K-Geboortedatum
           MOVE "00310624812030"
             TO FS-K-Telefoonnummer
           MOVE "4plus7@gmail.com"
             TO FS-K-Emailadres
           WRITE Klantrecord
           CLOSE KlantenBestand
           OPEN OUTPUT SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Systeemkengetallen.dat: " IOStatus
           END-IF
           MOVE 1 TO HoogsteKlantennummer
           WRITE Systeemkengetallenrecord
           CLOSE SysteemkengetallenBestand.

       VulReserveringen.
           OPEN I-O SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand

           OPEN OUTPUT ReserveringenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Reserveringen.dat: " IOStatus
           END-IF
           *> Woning 15, week 32, 1 week, geannuleerd
           MOVE "000000010000000115202332120230203"
             TO Reserveringsrecord
           MOVE "20230210" TO FS-R-DatumAnnulering
           WRITE Reserveringsrecord
           *> Woning 10, week 20, 3 weken, verlopen
           MOVE "000000020000000110202320320230204"
             TO Reserveringsrecord
           MOVE "20230210" TO FS-R-DatumVerlopen
           WRITE Reserveringsrecord
           *> Woning 17, week 18, 5 weken
           MOVE "000000030000000217202318520230205"
             TO Reserveringsrecord
           WRITE Reserveringsrecord


           *> Woning 13, week 22, 7 weken
           MOVE "000000040000000313202322720230206"
             TO Reserveringsrecord
           WRITE Reserveringsrecord
           *> Woning 19, week 36, 2 weken
           MOVE "000000050000000419202336220230208"
             TO Reserveringsrecord
           MOVE "20230210" TO FS-R-DatumBetaling
           WRITE Reserveringsrecord
           *> Bewoners toevoegen: 1 reservering, 1 woning, 3 personen (15, 3, 30)
           OPEN I-O BewonersBestand
           *> Bewoner 1
           MOVE 5 TO FS-B-Reserveringsnummer
           MOVE 1 TO FS-B-Volgnummer
           MOVE "JS" TO FS-B-Initialen
           MOVE "20200101" TO FS-B-Geboortedatum
           WRITE Bewonersrecord
           *> Bewoner 2
           MOVE 5 TO FS-B-Reserveringsnummer
           MOVE 2 TO FS-B-Volgnummer
           MOVE "AB" TO FS-B-Initialen
           MOVE "20080707" TO FS-B-Geboortedatum
           WRITE Bewonersrecord
           *> Bewoner 3
           MOVE 5 TO FS-B-Reserveringsnummer
           MOVE 3 TO FS-B-Volgnummer
           MOVE "MK" TO FS-B-Initialen
           MOVE "19931005" TO FS-B-Geboortedatum
           WRITE Bewonersrecord
           CLOSE BewonersBestand

           *> Woning 12, onderhoud in week 18 en 19
           MOVE SPACES TO Reserveringsrecord
           MOVE 6 TO FS-R-Reserveringsnummer
           MOVE 12 TO FS-R-Woningnummer
           MOVE "20230209" TO FS-R-DatumCreatie
           MOVE "O" TO FS-R-ReserveringsType
           MOVE 2023 TO FS-R-Jaar
           MOVE 18 TO FS-R-Weeknummer
           MOVE 2 TO FS-R-AantalWeken
           WRITE Reserveringsrecord

           *> Woning 11, verkocht vanaf 01-06-2023
           MOVE SPACES TO Reserveringsrecord
           MOVE 7 TO FS-R-Reserveringsnummer
           MOVE 11 TO FS-R-Woningnummer
           MOVE "20230210" TO FS-R-DatumCreatie
           MOVE "V" TO FS-R-ReserveringsType
           MOVE 2023 TO FS-R-Jaar
           MOVE 22 TO FS-R-Weeknummer
           WRITE Reserveringsrecord

           *> Woning 12, onderhoud in week 31 t/m 34
           MOVE SPACES TO Reserveringsrecord
           MOVE 8 TO FS-R-Reserveringsnummer
           MOVE 12 TO FS-R-Woningnummer
           MOVE "20230209" TO FS-R-DatumCreatie
           MOVE "O" TO FS-R-ReserveringsType
           MOVE 2023 TO FS-R-Jaar
           MOVE 31 TO FS-R-Weeknummer
           MOVE 4 TO FS-R-AantalWeken
           WRITE Reserveringsrecord

           *> Update systeemkengetallen
           MOVE 8 TO HoogsteReserveringsnummer
           REWRITE Systeemkengetallenrecord
           *> Sluit bestanden
           CLOSE SysteemkengetallenBestand
           CLOSE ReserveringenBestand.

           EXIT PROGRAM.

       