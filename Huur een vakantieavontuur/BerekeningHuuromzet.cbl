       PROGRAM-ID. Program2 as "Huur_een_vakantieavontuur.BerekeningHuuromzet".
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

       *> Tellers
       01 Teller PIC 99 VALUE ZERO.
       01 Wekenteller PIC 99 VALUE ZERO.

       *> HumanReadable variabelen
       01 HRBedrag PIC $ZZZ9 VALUE ZERO.

       *> Tabellen
       01 FILLER.
         03 Weekomzetten PIC 99999 VALUE ZERO OCCURS 20 TIMES.

       *> SKM RESERVERING NIEUW
       01 SKM-ReserveringIngelezen.
         03 SKM-R-N-tmJaarWeek.
           05 SKM-R-N-tmWoningnummer.
             07 SKM-R-N-EOF PIC 9 VALUE ZERO.
               88 R-EOF VALUE 1.
               88 R-NotEOF VALUE ZERO.
             07 SKM-R-N-Woningnummer PIC 99 VALUE ZERO.
           05 SKM-R-N-Jaarweek PIC 9(6) VALUE ZERO.
       *> SKM RESERVERING OUD
       01 SKM-ReserveringBewaard.
         03 SKM-R-O-tmJaarWeek.
           05 SKM-R-O-tmWoningnummer.
             07 SKM-R-O-EOF PIC 9 VALUE ZERO.
             07 SKM-R-O-Woningnummer PIC 99 VALUE ZERO.
           05 SKM-R-O-Jaarweek PIC 9(6) VALUE ZERO.

       PROCEDURE DIVISION.

       BeginProgram.
           PERFORM RB90-InitProgramma
           IF NOT R-EOF
               PERFORM RH80-InitVerwerking
               PERFORM UNTIL R-EOF
                   PERFORM RB70-InitWoning
                   PERFORM UNTIL SKM-ReserveringIngelezen NOT EQUALS SKM-ReserveringBewaard
                       PERFORM RH60-VerwerkReservering
                       PERFORM RB61-LeesReservering
                   END-PERFORM
                   *> Hier gebeurt niks:
                   PERFORM RB71-BouwWoning
                   PERFORM RB72-SchrijfWoning
                   PERFORM RB79-AfsluitenWoning
               END-PERFORM
               *> Deze routines zijn overbodig: RB81, RB82, RB89
           END-IF
           PERFORM RH99-AfsluitenProgramma
           EXIT PROGRAM.
       
       RH60-VerwerkReservering.
           *> Dit is de laatste subroutine waar we directe toegang hebben tot een gelezen record.
           *> Daarom vind hier de verwerking plaats. In dit geval: het ophogen van de weekomzet.
           IF (FS-R-Jaar EQUALS 2023)
               PERFORM VARYING Teller FROM 0 BY 1 UNTIL Teller EQUALS FS-R-AantalWeken
                   ADD 10 TO Weekomzetten(FS-R-Weeknummer - 17 + Teller)
               END-PERFORM
           END-IF.
       
       RB61-LeesReservering.
           MOVE SKM-ReserveringIngelezen
             TO SKM-ReserveringBewaard
           READ ReserveringenBestand NEXT RECORD
               AT END
                   SET R-EOF TO TRUE
               NOT AT END
                   MOVE FS-R-Woningnummer
                     TO SKM-R-N-Woningnummer
                   MOVE FS-R-JaarWeek
                     TO SKM-R-N-Jaarweek
           END-READ.
       RB70-InitWoning.
           MOVE SKM-ReserveringIngelezen
             TO SKM-ReserveringBewaard.
       RB71-BouwWoning.
           .
       RB72-SchrijfWoning.
           .
       RB79-AfsluitenWoning.
           .
       RH80-InitVerwerking.
           MOVE SKM-ReserveringIngelezen
             TO SKM-ReserveringBewaard
           *> Alle weekomzetten weer op nul zetten
           PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > 20
               MOVE ZERO TO Weekomzetten(Teller)
           END-PERFORM.
       RB90-InitProgramma.
           SET R-NotEOF TO TRUE
           OPEN INPUT ReserveringenBestand
           MOVE ZERO TO FS-R-Woningnummer
           START ReserveringenBestand
             KEY > FS-R-Woningnummer
           END-START
           PERFORM RB61-LeesReservering.
       RH99-AfsluitenProgramma.
           PERFORM RenderOmzettentabel
           CLOSE ReserveringenBestand.

       RenderOmzettentabel.
           DISPLAY SPACE
           DISPLAY "HUUROMZETTEN"
           DISPLAY "============"
           *> Kopregel
           DISPLAY "weeknr" SPACE WITH NO ADVANCING
           PERFORM VARYING Wekenteller FROM 1 BY 1 UNTIL Wekenteller > 20
               DISPLAY "|   "(Wekenteller + 17) SPACE WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           *> Omzetten
           DISPLAY "omzet " SPACE WITH NO ADVANCING
           PERFORM VARYING Wekenteller FROM 1 BY 1 UNTIL Wekenteller > 20
               MOVE Weekomzetten(Wekenteller)
                 TO HRBedrag
               DISPLAY "|" HRBedrag SPACE WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE.

       end program Program2.
