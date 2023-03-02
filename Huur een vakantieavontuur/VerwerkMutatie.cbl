       PROGRAM-ID. Program2 as "Huur_een_vakantieavontuur.VerwerkMutatie".          

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

       SELECT ReserveringenBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\ReserveringenMetAltKey.dat"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS FS-R-Reserveringsnummer
                  ALTERNATE KEY IS FS-R-Woningnummer
                  WITH DUPLICATES
                  FILE STATUS IS IOStatus.

       SELECT MutatieBestand
                  ASSIGN TO "C:\COBOL\DATA\HUUR\Mutaties.dat"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS IOStatus.

       DATA DIVISION.
       FILE SECTION.

       FD ReserveringenBestand.
       01 Reserveringsrecord.
                            COPY Reservering REPLACING ==(pf)== BY ==FS-R==.

       FD SysteemkengetallenBestand.
       01 Systeemkengetallenrecord.
         03 HoogsteKlantennummer PIC 9(8) VALUE ZERO.
         03 HoogsteReserveringsnummer PIC 9(8) VALUE ZERO.

       FD KlantenBestand.
       01 Klantrecord.
       COPY Klant REPLACING ==(pf)== BY ==FS-K==.

       FD MutatieBestand.
       01 Mutatierecord.
       COPY Mutatie REPLACING ==(pf)== BY ==FS-M==.

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

       *> SKM RESERVERING TBV MUTATIES
       01 SKM-RM.
         05 SKM-RM-tmReserveringsnummer.
           07 SKM-RM-EOFVlag PIC 9 VALUE ZERO.
             88 SKM-RM-EOF VALUE 1.
           07 SKM-RM-Reserveringsnummer PIC 9(8) VALUE ZERO.

       *> SKM MUTATIE
       01 SKM-M.
         05 SKM-M-tmReserveringsnummer.
           07 SKM-M-EOFVlag PIC 9 VALUE ZERO.
             88 SKM-M-EOF VALUE 1.
           07 SKM-M-Reserveringsnummer PIC 9(8) VALUE ZERO.


       *> KSK
       01 KSK.
         05 KSK-tmReserveringsnummer.
           07 KSK-EOFVlag PIC 9 VALUE ZERO.
             88 KSK-AllEOF VALUE 1.
             88 KSK-NotAllEOF VALUE ZERO.
           07 KSK-Reserveringsnummer PIC 9(8) VALUE ZERO.


       *> Andere Variabelen
       

       01 Reserveringsdatum PIC 9(8) VALUE ZERO.
       01 DatumVandaag PIC 9(8) VALUE ZERO.

       PROCEDURE DIVISION.

           DISPLAY "SUBPROGRAM - VERWERKMUTATIE"
           PERFORM RM90-InitialiseerProgramma
           IF KSK-NotAllEOF
               PERFORM RM80-InitialiseerVerwerking
               PERFORM WITH TEST BEFORE UNTIL KSK-AllEOF
                   PERFORM RM70-InitialiseerReserveringsnummer
                   IF SKM-RM EQUALS KSK
                       PERFORM RM60-VerwerkReserveringOrigineel
                   END-IF
                   PERFORM RM71-TussenInit
                   IF SKM-M EQUALS KSK
                       PERFORM RM62-VerwerkMutatie
                       PERFORM RM63-LeesVolgendeMutatie
                   END-IF
                   PERFORM RM72-TussenInit
                   IF WijzigingTeVerwerken
                       PERFORM RM64-BouwReserveringGemuteerd
                       PERFORM RM65-SchrijfReserveringGemuteerd
                   END-IF
                   IF SKM-RM EQUALS KSK
                       PERFORM RM61-LeesVolgendeReserveringOrigineel
                   END-IF
                   PERFORM RM73-AfsluitenReserveringsnummer
                   PERFORM RM79-ZetIteratieVoorwaarde
               END-PERFORM
               PERFORM RM89-AfsluitenVerwerking
           END-IF
           PERFORM RM99-AfsluitenProgramma.

       RM90-InitialiseerProgramma.
           PERFORM GetDatumVandaag
           SET KSK-NotAllEOF TO TRUE
           OPEN I-O ReserveringenBestand
           DISPLAY ">>> Opening ReserveringenBestand. IOStatus: " IOStatus
           OPEN INPUT MutatieBestand
           DISPLAY ">>> Opening MutatieBestand. IOStatus: " IOStatus
           PERFORM RM61-LeesVolgendeReserveringOrigineel
           PERFORM RM63-LeesVolgendeMutatie
           PERFORM RM79-ZetIteratieVoorwaarde.

       RM80-InitialiseerVerwerking.
          
       RM70-InitialiseerReserveringsnummer.
           SET GeenWijziging TO TRUE
           SET GeenReserveringVerwerkt
             TO TRUE
           MOVE FS-R-DatumCreatie
             TO Reserveringsdatum.
       RM60-VerwerkReserveringOrigineel.
           SET ReserveringVerwerkt
             TO TRUE.

       RM61-LeesVolgendeReserveringOrigineel.
           IF (ReserveringVerwerkt)
               READ ReserveringenBestand NEXT RECORD
                   AT END
                       SET SKM-RM-EOF
                         TO TRUE
                   NOT AT END
                       MOVE FS-R-Reserveringsnummer
                         TO SKM-RM-Reserveringsnummer
               END-READ
           END-IF.
       RM71-TussenInit.
           .
       RM62-VerwerkMutatie.
           IF (GeenReserveringVerwerkt)
               DISPLAY "Dit is een mutatie zonder reservering, dus we doen niks."
           ELSE
               SET WijzigingTeVerwerken
                 TO TRUE
               EVALUATE FS-M-Mutatietype
                   WHEN "AR"
                       MOVE FS-M-Mutatiedatum
                         TO FS-R-DatumAnnulering
                   WHEN "BR"
                       MOVE FS-M-Mutatiedatum
                         TO FS-R-DatumBetaling
                   WHEN "AB"
                       MOVE FS-M-Mutatiedatum
                         TO FS-R-DatumAnnulering
               END-EVALUATE
           END-IF.
       RM63-LeesVolgendeMutatie.
           READ MutatieBestand NEXT RECORD
               AT END
                   SET SKM-M-EOF
                     TO TRUE
               NOT AT END
                   MOVE FS-M-Reserveringsnummer
                     TO SKM-M-Reserveringsnummer
           END-READ.
       RM72-TussenInit.
           IF (DatumVandaag - Reserveringsdatum > 5)
               MOVE DatumVandaag
                 TO FS-R-DatumVerlopen
               SET WijzigingTeVerwerken
                 TO TRUE
           END-IF.
       RM64-BouwReserveringGemuteerd.
           .
       RM65-SchrijfReserveringGemuteerd.
           REWRITE Reserveringsrecord.
       RM73-AfsluitenReserveringsnummer.
           .
       RM79-ZetIteratieVoorwaarde.
           IF (SKM-RM < SKM-M)
               MOVE SKM-RM TO KSK
           ELSE
               MOVE SKM-M TO KSK
           END-IF.
       RM89-AfsluitenVerwerking.
           .
       RM99-AfsluitenProgramma.
           CLOSE ReserveringenBestand
           CLOSE MutatieBestand.

       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8)
             TO DatumVandaag.

       end program program2.
