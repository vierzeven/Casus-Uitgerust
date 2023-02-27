       IDENTIFICATION DIVISION.
       PROGRAM-ID. 04--UitgifteSleutelOpNaam.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 31-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT BoekingenBestand ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Boekingen.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BB-Werkpleknummer
           ALTERNATE RECORD KEY IS BB-Naam WITH DUPLICATES
           FILE STATUS IS IOStatus.
       DATA DIVISION.
       FILE SECTION.
       *> Boekingenbestand (wordt overschreven met de reserveringen van vandaag)
       FD BoekingenBestand.
       01 BoekingenRecord.
         02 BB-Naam PIC X(24).
         02 BB-Werkpleknummer.
           03 BB-Huisnummer PIC 99.
           03 BB-Kamernummer PIC 9.
         02 BB-SleutelUitgereiktVlag PIC 9 VALUE ZERO.
           88 BB-SleutelUitgereikt VALUE 1.
           88 BB-SleutelNietUitgereikt VALUE 0.

       WORKING-STORAGE SECTION.
       *> IO STATUS VLAG
       01 IOStatus PIC XX.
         88 IO-OK VALUE ZEROES.
       *> END OF DATA VLAG
       01 EndOfDataFlag PIC 9 VALUE ZERO.
         88 EndOfData VALUE 1.
         88 NotEndOfData VALUE 0.
       *> NAAM VALIDATIE VLAG
       01 NameValidationFlag PIC 9 VALUE ZERO.
         88 NameFound VALUE 1.
         88 NameNotFound VALUE 0.
       *> MUTATIE VOTLTOOID VLAG
       01 MutationDoneFlag PIC 9 VALUE ZERO.
         88 MutatieVoltooid VALUE 1.
         88 MutatieNogNietVoltooid VALUE 0.
       *> STOP PROGRAMMA VLAG
       01 StopProgramFlag PIC 9 VALUE ZERO.
         88 StopProgram VALUE 1.
         88 ContinueProgram VALUE 0.
       *> BOEKING
       01 Boeking.
         02 BoekingsNaam PIC X(24).
         02 BoekingsHuisnummer PIC 9(2).
         02 BoekingsKamernummer PIC 9.
         02 SleutelUitgereiktVlag PIC 9 VALUE 0.
           88 SleutelUitgereikt VALUE 1.
           88 SleutelNietUitgereikt VALUE 0.
       01 DatumVandaag PIC 9(8).
       *> WERKPLEK
       01 Werkplek OCCURS 1 TO 100 TIMES DEPENDING ON AantalWerkplekken.
         02 WP-Huisnummer PIC 9(2).
         02 WP-Kamernummer PIC 9.
       01 AantalWerkplekken PIC 99 VALUE ZERO.
       01 WerkplekkenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       *> WONING
       01 Woning OCCURS 10 TIMES.
         02 WO-Huisnummer PIC 9(2).
         02 WO-Type PIC X.
         02 WO-AantalKamers PIC 9.
       01 AantalWoningen PIC 99 VALUE ZERO.
       01 WoningenTeller PIC 99 VALUE ZERO PACKED-DECIMAL.
       01 DatumVerschil PIC S99 VALUE ZERO.
       01 AantalReserveringenVandaag PIC 99 VALUE ZERO.
       01 DatumAlsInteger PIC 9(8).
       01 BoekingenTeller PIC 99 VALUE ZERO.
       01 Iteraties PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM UNTIL StopProgram
               PERFORM ToonBoekingen
               PERFORM GetReserveringsNaam
               IF (Boekingsnaam NOT EQUALS "xxx")
                   MOVE ZERO TO Iteraties
                   PERFORM DoeHet
                   DISPLAY "Aantal records gelezen om dit te bereiken: " Iteraties
               ELSE
                   SET StopProgram TO TRUE
               END-IF
           END-PERFORM
           DISPLAY SPACE
           STOP RUN.

       GetReserveringsNaam.
           DISPLAY SPACE
           DISPLAY "SLEUTEL UITGIFTE"
           DISPLAY "================"
           DISPLAY "Naam: " WITH NO ADVANCING
           ACCEPT BoekingsNaam.
       DoeHet.
           OPEN I-O BoekingenBestand
           *> Als alle bestanden goed geopend zijn...
           IF (IO-OK)
               SET NotEndOfData TO TRUE
               SET MutatieNogNietVoltooid TO TRUE
               *> Eerste boeking uitlezen...
               PERFORM LeesBoeking
               *> ...en voor elke boeking...
               PERFORM UNTIL EndOfData OR MutatieVoltooid
                   IF (BB-Naam EQUALS BoekingsNaam)
                       SET NameFound TO TRUE
                       IF (BB-SleutelNietUitgereikt)
                           SET BB-SleutelUitgereikt TO TRUE
                           SET MutatieVoltooid TO TRUE
                           REWRITE BoekingenRecord
                               INVALID KEY
                                   DISPLAY "Er ging iets fout. File status: " IOStatus
                           END-REWRITE 
                       END-IF
                   END-IF
                   *> Volgende boeking uitlezen...
                   IF (NotEndOfData AND MutatieNogNietVoltooid)
                       PERFORM LeesBoeking
                   END-IF
               END-PERFORM
               IF (NameFound AND MutatieNogNietVoltooid)
                   DISPLAY SPACE
                   DISPLAY ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                   DISPLAY "Helaas. We hebben geen werkplek beschikbaar voor " BoekingsNaam
                   DISPLAY "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<"
                   DISPLAY SPACE
               END-IF
               CLOSE BoekingenBestand
           ELSE
               DISPLAY "Er is iets mis gegaan. IO-Status: " IOStatus
           END-IF.

       GeefSleutelUit.
           OPEN I-O BoekingenBestand
           SET BB-SleutelUitgereikt TO TRUE
           MOVE BoekingsNaam TO BB-Naam
           REWRITE BoekingenRecord
               INVALID KEY
                   DISPLAY "Er ging iets fout. File status: " IOStatus
           END-REWRITE
           CLOSE BoekingenBestand.
       ToonBoekingen.
           MOVE ZERO TO BoekingenTeller
           DISPLAY SPACE
           DISPLAY "BOEKINGEN"
           DISPLAY "========="
           OPEN INPUT BoekingenBestand
           *> Als alle bestanden goed geopend zijn...
           IF (IO-OK)
               SET NotEndOfData TO TRUE
               *> Eerste boeking uitlezen...
               READ BoekingenBestand NEXT RECORD
                   AT END
                       SET EndOfData TO TRUE
               END-READ
               *> ...en voor elke boeking...
               PERFORM UNTIL EndOfData
                   ADD 1 TO BoekingenTeller
                   DISPLAY BoekingenTeller ". Huisnummer: " BB-Huisnummer WITH NO ADVANCING
                   DISPLAY " | Kamernummer: " BB-Kamernummer WITH NO ADVANCING
                   DISPLAY " | Naam: " BB-Naam WITH NO ADVANCING
                   IF (BB-SleutelUitgereikt)
                       DISPLAY " | Sleutel uitgereikt"
                   ELSE
                       DISPLAY " | Sleutel nog niet uitgereikt"
                   END-IF
                   *> Volgende boeking uitlezen...
                   READ BoekingenBestand NEXT RECORD
                       AT END
                           SET EndOfData TO TRUE
                   END-READ
               END-PERFORM
               CLOSE BoekingenBestand
           ELSE
               DISPLAY "Er is iets mis gegaan. IO-Status: " IOStatus
           END-IF.

       LeesBoeking.
           READ BoekingenBestand NEXT RECORD
               AT END
                   SET EndOfData TO TRUE
               NOT AT END
                   ADD 1 TO Iteraties
           END-READ.