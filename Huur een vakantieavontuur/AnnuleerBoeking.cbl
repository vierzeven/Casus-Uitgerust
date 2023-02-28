       PROGRAM-ID. Program2 as "Huur_een_vakantieavontuur.AnnuleerBoeking".          

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

       

       DATA DIVISION.
       FILE SECTION.

       FD ReserveringenBestand.
       01 Reserveringsrecord.
       COPY Reservering REPLACING ==(pf)== BY ==FS-R==.

       

       

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.

       01 WijzigingVlag PIC 9 VALUE 0.
         88 WijzigingTeVerwerken VALUE 1.
         88 GeenWijziging VALUE 0.
       01 ReserveringVerwerktVlag PIC 9 VALUE 0.
         88 ReserveringVerwerkt VALUE 1.
         88 GeenReserveringVerwerkt VALUE 0.

       01 KeuzeVlag PIC 9 VALUE 1.
         88 VerlaatHetProgramma VALUE ZERO.
         88 BlijfInHetMenu VALUE 1.
       01 Keuze PIC 99 VALUE ZERO.

       01 Reserveringnr-WS PIC 9(8).
       01 DatumVandaag PIC 9(8) VALUE ZERO.
       


       *> Andere Variabelen
       

       01 Reserveringsdatum PIC 9(8) VALUE ZERO.
       

       PROCEDURE DIVISION.

           DISPLAY "SUBPROGRAM - ANNULEER BOEKING"
             
           PERFORM OpvragenResNummer

           
           PERFORM ToonReservering
           
           STOP RUN.

       OpvragenResNummer.

           DISPLAY "Reserveringnummer: " WITH NO ADVANCING
           ACCEPT Reserveringnr-WS
           DISPLAY "Annuleringsdatum: " WITH NO ADVANCING
           ACCEPT DatumVandaag

           .

       ToonReservering.
           OPEN I-O ReserveringenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van het reserveringen bestand: " IOStatus
           END-IF

           MOVE Reserveringnr-WS TO FS-R-RESERVERINGSNUMMER
           READ ReserveringenBestand KEY IS FS-R-Reserveringsnummer
               INVALID KEY
                   DISPLAY "Reservering niet gevonden - " IOStatus
           END-READ

           IF IO-OK THEN
               DISPLAY "Reservering nummer: " FS-R-Reserveringsnummer
               DISPLAY "Klantnummer: " FS-R-Klantnummer
               DISPLAY "Woning nummer: " FS-R-Woningnummer
               DISPLAY "Woning jaar - week: " FS-R-JaarWeek
               DISPLAY "Aantal weken: " FS-R-AantalWeken
               DISPLAY ""
                 *>>> bevestigin
                 *>>
               PERFORM UNTIL VerlaatHetProgramma
                   DISPLAY SPACE
                   DISPLAY "1. Boeking annuleren"
                   DISPLAY "2. Ander Reservering invoeren"
                   DISPLAY "47. EXIT"
                   DISPLAY SPACE
                   DISPLAY "Uw keuze: " WITH NO ADVANCING
                   ACCEPT Keuze
                   EVALUATE Keuze
                       WHEN 1
                           PERFORM AnnuleringBoeking
                           SET VerlaatHetProgramma
                             TO TRUE
                       WHEN 2
                           CLOSE ReserveringenBestand
                           PERFORM OpvragenResNummer
                           PERFORM ToonReservering
                       WHEN 47
                           SET VerlaatHetProgramma
                             TO TRUE
                           
                       WHEN OTHER
                           SET BlijfInHetMenu
                             TO TRUE
                   END-EVALUATE
               END-PERFORM
               
               
           END-IF.

       AnnuleringBoeking.
           MOVE DatumVandaag TO FS-R-DatumAnnulering
           REWRITE Reserveringsrecord
               INVALID KEY
                   DISPLAY "File status - " IOStatus
           END-REWRITE
           DISPLAY SPACE
           DISPLAY "De boeking met reserveringsnummer " FS-R-Reserveringsnummer " is geannulleerd per " FS-R-DatumAnnulering
           DISPLAY SPACE
           CLOSE ReserveringenBestand.
