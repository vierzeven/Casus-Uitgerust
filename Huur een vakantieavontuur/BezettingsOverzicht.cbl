       IDENTIFICATION DIVISION.
       PROGRAM-ID. BezettingsOverzicht.
       AUTHOR. Michael Koning.
       DATE-WRITTEN. 27-02-2023.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       Currency Sign "E" with Picture Symbol '$'.
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
       SELECT WoningenBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Woningen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.
       SELECT SysteemkengetallenBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Systeemkengetallen.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS IOStatus.
       SELECT MutatieBestand
           ASSIGN TO "C:\COBOL\DATA\HUUR\Mutaties.dat"
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
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 KeuzeVlag PIC 9 VALUE 1.
         88 VerlaatHetProgramma VALUE ZERO.
         88 BlijfInHetMenu VALUE 1.
       01 WijzigingVlag PIC 9 VALUE 0.
         88 WijzigingTeVerwerken VALUE 1.
         88 GeenWijziging VALUE 0.
       01 ReserveringVerwerktVlag PIC 9 VALUE 0.
         88 ReserveringVerwerkt VALUE 1.
         88 GeenReserveringVerwerkt VALUE 0.
       *> SKM RESERVERING OUD
       01 SKM-ReserveringBewaard.
         03 SKM-R-O-tmJaarWeek.
           05 SKM-R-O-tmWoningnummer.
             07 SKM-R-O-EOF PIC 9 VALUE ZERO.
             07 SKM-R-O-Woningnummer PIC 99 VALUE ZERO.
           05 SKM-R-O-Jaarweek PIC 9(6) VALUE ZERO.
       *> SKM RESERVERING NIEUW
       01 SKM-ReserveringIngelezen.
         03 SKM-R-N-tmJaarWeek.
           05 SKM-R-N-tmWoningnummer.
             07 SKM-R-N-EOF PIC 9 VALUE ZERO.
               88 R-EOF VALUE 1.
               88 R-NotEOF VALUE ZERO.
             07 SKM-R-N-Woningnummer PIC 99 VALUE ZERO.
           05 SKM-R-N-Jaarweek PIC 9(6) VALUE ZERO.
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

       *> Tabellen
       01 FILLER.
         03 Huisjesbezetting OCCURS 10 TIMES.
           05 Weken PIC X(4) VALUE "   ." OCCURS 20 TIMES.
       01 FILLER.
         03 Weekomzetten PIC 99999 VALUE ZERO OCCURS 20 TIMES.
       01 FILLER VALUE "10S11S12L13L14S15S16S17L18L19S".
         03 HuisjeInitieel PIC X(3) OCCURS 10.

       *> Tellers
       01 Teller PIC 99 VALUE ZERO.
       01 Huisjesteller PIC 99 VALUE ZERO.
       01 Wekenteller PIC 99 VALUE ZERO.
       *> HumanReadable variabelen
       01 HRBedrag PIC $ZZZ9 VALUE ZERO.
       *> Overige variabelen
       01 Keuze PIC 99 VALUE ZERO.
       01 AantalBewoners PIC 9 VALUE ZERO.
       01 Reserveringsdatum PIC 9(8) VALUE ZERO.
       01 DatumVandaag PIC 9(8) VALUE ZERO.

       PROCEDURE DIVISION.
       BeginProgram.
       BezettingsOverzicht.
           PERFORM RB90-InitProgramma
           IF NOT R-EOF
               PERFORM RB80-InitVerwerking
               PERFORM UNTIL R-EOF
                   PERFORM RB70-InitWoning
                   PERFORM UNTIL SKM-ReserveringIngelezen NOT EQUALS SKM-ReserveringBewaard
                       PERFORM RB60-VerwerkReservering
                       PERFORM RB61-LeesReservering
                   END-PERFORM
                   PERFORM RB71-BouwWoning
                   PERFORM RB72-SchrijfWoning
                   PERFORM RB79-AfsluitenWoning
               END-PERFORM
               *> Deze routines zijn overbodig: RB81, RB82, RB89
           END-IF
           PERFORM RB99-AfsluitenProgramma.

       RB90-InitProgramma.
           SET R-NotEOF TO TRUE
           OPEN INPUT ReserveringenBestand
           MOVE ZERO TO FS-R-Woningnummer
           START ReserveringenBestand
             KEY > FS-R-Woningnummer
           END-START
           PERFORM RB61-LeesReservering.
       RB99-AfsluitenProgramma.
           PERFORM RenderBezettingstabel
           CLOSE ReserveringenBestand.
       RB80-InitVerwerking.
           MOVE SKM-ReserveringIngelezen
             TO SKM-ReserveringBewaard
           *> Alle weekomzetten weer op nul zetten
           PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > 20
               MOVE ZERO TO Weekomzetten(Teller)
           END-PERFORM.
       RB70-InitWoning.
           MOVE SKM-ReserveringIngelezen
             TO SKM-ReserveringBewaard.
       RB71-BouwWoning.
           .

       RB72-SchrijfWoning.
           .
       RB79-AfsluitenWoning.
           .
       RB60-VerwerkReservering.
           *> Dit is de laatste subroutine waar we directe toegang hebben tot een gelezen record.
           *> Daarom vind hier de verwerking plaats. In dit geval: het plaatsen van de juiste letters R.
           *> Van belang zijn de velden FS-R-Woningnummer (voor de rij), FS-R-JaarWeek (de kolom),
           *> en FS-R-AantalWeken (voor de iteratie).
           IF (FS-R-Jaar EQUALS 2023)
               PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > FS-R-AantalWeken
                   *> Onderscheid maken tussen boeking, reservering en verlopen
                   MOVE "   R" TO Weken(FS-R-Woningnummer - 9, FS-R-Weeknummer - 18 + Teller)
                   IF FS-R-DatumBetaling > "        "
                       MOVE "   B"
                         TO Weken(FS-R-Woningnummer - 9, FS-R-Weeknummer - 18 + Teller)
                   END-IF

                   IF FS-R-DatumVerlopen > "        "
                       MOVE "   ."
                         TO Weken(FS-R-Woningnummer - 9, FS-R-Weeknummer - 18 + Teller)
                   END-IF
                   IF FS-R-DatumAnnulering > "        "
                       MOVE "   ."
                         TO Weken(FS-R-Woningnummer - 9, FS-R-Weeknummer - 18 + Teller)
                   END-IF
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
                   DISPLAY "Read gedaan op huisje " FS-R-Woningnummer
                   MOVE FS-R-JaarWeek
                     TO SKM-R-N-Jaarweek
           END-READ.

       RenderBezettingstabel.
           DISPLAY SPACE
           DISPLAY "BEZETTINGSOVERZICHT"
           DISPLAY "==================="
           *> Regel 1: weeknummers
           DISPLAY "weeknr =>" SPACE WITH NO ADVANCING
           PERFORM VARYING Wekenteller FROM 1 BY 1 UNTIL Wekenteller > 20
               DISPLAY SPACE (Wekenteller + 17) SPACE WITH NO ADVANCING
           END-PERFORM
           DISPLAY SPACE
           *> Regel 2: kolomkop huisnummers
           DISPLAY " huisnr"
           *> Tot slot: de rest van de tabel
           PERFORM VARYING Huisjesteller FROM 1 BY 1 UNTIL Huisjesteller > 10
               DISPLAY "   "(Huisjesteller + 9) "    " WITH NO ADVANCING
               PERFORM VARYING Wekenteller FROM 1 BY 1 UNTIL Wekenteller > 20
                   DISPLAY Weken(Huisjesteller, Wekenteller) WITH NO ADVANCING
               END-PERFORM
               DISPLAY SPACE
           END-PERFORM.
