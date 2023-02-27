       IDENTIFICATION DIVISION.
       PROGRAM-ID. Opdracht0506.
       AUTHOR. Joey Schmitz.
       DATE-WRITTEN. 02-2023.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       Currency Sign "E" with Picture Symbol '$'.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
<<<<<<< HEAD
       
      *    SELECT ReserveringenBestand
      *    ASSIGN TO "C:\COBOL\DATA\HUUR\Reserveringen.dat"
      *    ORGANIZATION IS INDEXED
      *    ACCESS MODE IS DYNAMIC
      *    RECORD KEY IS FS-R-Reserveringsnummer
      *    FILE STATUS IS IOStatus.

=======
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
>>>>>>> a8499aaefa5d252af5542058e2e11d431fe80c9c
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

       DATA DIVISION.
       FILE SECTION.

       FD BewonersBestand.
       01 Bewonersrecord.
       COPY Bewoner REPLACING ==(pf)== BY ==FS-B==.
       FD MutatieBestand.
       01 Mutatierecord.
       COPY Mutatie REPLACING ==(pf)== BY ==FS-M==.

       WORKING-STORAGE SECTION.
       *> Vlaggen
       01 IOStatus PIC 99 VALUE ZERO.
         88 IO-OK VALUE ZERO.
       01 KeuzeVlag PIC 9 VALUE 1.
         88 VerlaatHetProgramma VALUE ZERO.
         88 BlijfInHetMenu      VALUE 1.
       01 WijzigingVlag PIC 9 VALUE 0.
         88 WijzigingTeVerwerken VALUE 1.
         88 GeenWijziging        VALUE 0.
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
       

       *> Tellers
       01 Teller       PIC 99 VALUE ZERO.
       *>01 Huisjesteller PIC 99 VALUE ZERO.
       01 Wekenteller  PIC 99 VALUE ZERO.
       *> HumanReadable variabelen
       01 HRBedrag     PIC $ZZZ9 VALUE ZERO.
       *> Overige variabelen
       01 Keuze        PIC 99 VALUE ZERO.
       01 AantalBewoners PIC 9 VALUE ZERO.
       01 Reserveringsdatum PIC 9(8) VALUE ZERO.
       01 DatumVandaag PIC 9(8) VALUE ZERO.

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM DisplayMenu
           STOP RUN
           .
       DisplayMenu.
           PERFORM GetDatumVandaag
           PERFORM UNTIL VerlaatHetProgramma
               DISPLAY SPACE
               DISPLAY "1. Bestandsinitialisatie"
               DISPLAY "2. Toevoegen Klant"
               DISPLAY "3. Toevoegen Reservering"
               DISPLAY "4. Bezettingsoverzicht"
               DISPLAY "5. Berekening Huuromzet"
               DISPLAY "6. Annuleer Boeking"
               DISPLAY "7. Annuleer Reservering"
               DISPLAY "8. Betaal Reservering"
               DISPLAY "9. Verwerk mutaties"
               DISPLAY "47. EXIT"
               DISPLAY SPACE
               DISPLAY "Uw keuze: " WITH NO ADVANCING
               ACCEPT Keuze
               EVALUATE Keuze
                   WHEN 1
                       PERFORM Bestandsinitialisatie
                   WHEN 2
                       PERFORM ToevoegenKlant
                   WHEN 3
                       PERFORM ToevoegenReservering
                   WHEN 4
                       PERFORM BezettingsOverzicht
                   WHEN 5
                       PERFORM BerekeningHuuromzet
                   WHEN 6
                       PERFORM AnnuleerBoeking
                   WHEN 7
                       PERFORM AnnuleerReservering
                   WHEN 8
                       PERFORM BetaalReservering
                   WHEN 9
                       PERFORM VerwerkMutaties
                   WHEN 47
                       SET VerlaatHetProgramma
                               TO TRUE
                   WHEN OTHER
                       SET BlijfInHetMenu
                               TO TRUE
               END-EVALUATE
           END-PERFORM
           .
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
           END-PERFORM
           .
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
           DISPLAY SPACE
           .
       Bestandsinitialisatie.
           CALL "Bestandsinitialisatie".

       ToevoegenReservering.
           CALL "ToevoegenReservering".

       ToevoegenKlant.
<<<<<<< HEAD
           CALL "ToevoegenKlant".
           
       ToevoegenReservering.
=======
>>>>>>> a8499aaefa5d252af5542058e2e11d431fe80c9c
           DISPLAY SPACE
           OPEN I-O SysteemkengetallenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van SysteemkengetallenBestand.dat: " IOStatus
           END-IF
<<<<<<< HEAD
           OPEN I-O ReserveringenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Reserveringen.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand
           ADD 1               TO HoogsteReserveringsnummer
           MOVE HoogsteReserveringsnummer
                               TO FS-R-Reserveringsnummer
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
               MOVE Teller     TO FS-B-Volgnummer
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
           .

=======
           OPEN I-O KlantenBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Klanten.dat: " IOStatus
           END-IF
           READ SysteemkengetallenBestand
           ADD 1               TO HoogsteKlantennummer
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
           CLOSE KlantenBestand
           .
>>>>>>> a8499aaefa5d252af5542058e2e11d431fe80c9c
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
           PERFORM RB99-AfsluitenProgramma
           .
       BerekeningHuuromzet.
           PERFORM RB90-InitProgramma
           IF NOT R-EOF
               PERFORM RH80-InitVerwerking
               PERFORM UNTIL R-EOF
                   PERFORM RB70-InitWoning
                   PERFORM UNTIL SKM-ReserveringIngelezen NOT EQUALS SKM-ReserveringBewaard
                       PERFORM RH60-VerwerkReservering
                       PERFORM RB61-LeesReservering
                   END-PERFORM
                   PERFORM RB71-BouwWoning
                   PERFORM RB72-SchrijfWoning
                   PERFORM RB79-AfsluitenWoning
               END-PERFORM
               *> Deze routines zijn overbodig: RB81, RB82, RB89
           END-IF
           PERFORM RH99-AfsluitenProgramma
           .
       
       
       MaakMutatieBestand.
           OPEN OUTPUT MutatieBestand
           IF NOT IO-OK
               DISPLAY ">>> Fout bij het openen van Mutaties.dat: " IOStatus
           END-IF
           CLOSE MutatieBestand
           .
       
       RB90-InitProgramma.
           SET R-NotEOF        TO TRUE
           OPEN INPUT ReserveringenBestand
           MOVE ZERO           TO FS-R-Woningnummer
           START ReserveringenBestand
             KEY > FS-R-Woningnummer
           END-START
           PERFORM RB61-LeesReservering
           .
       RB99-AfsluitenProgramma.
           PERFORM RenderBezettingstabel
           CLOSE ReserveringenBestand
           .
       RB80-InitVerwerking.
           MOVE SKM-ReserveringIngelezen
                               TO SKM-ReserveringBewaard
           *> Alle weekomzetten weer op nul zetten
           PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > 20
               MOVE ZERO       TO Weekomzetten(Teller)
           END-PERFORM
           .
       RB70-InitWoning.
           MOVE SKM-ReserveringIngelezen
                               TO SKM-ReserveringBewaard
           .
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
           END-IF
           .
       RB61-LeesReservering.
           MOVE SKM-ReserveringIngelezen
                               TO SKM-ReserveringBewaard
           READ ReserveringenBestand NEXT RECORD
               AT END
                   SET R-EOF   TO TRUE
               NOT AT END
                   MOVE FS-R-Woningnummer
                               TO SKM-R-N-Woningnummer
                   DISPLAY "Read gedaan op huisje " FS-R-Woningnummer
                   MOVE FS-R-JaarWeek
                               TO SKM-R-N-Jaarweek
           END-READ
           .

       *> SUBROUTINES MET EEN AANGEPASTE INHOUD TBV BEREKENING HUUROMZETTEN
       RH99-AfsluitenProgramma.
           PERFORM RenderOmzettentabel
           CLOSE ReserveringenBestand
           .
       RH80-InitVerwerking.
           MOVE SKM-ReserveringIngelezen
                               TO SKM-ReserveringBewaard
           *> Alle weekomzetten weer op nul zetten
           PERFORM VARYING Teller FROM 1 BY 1 UNTIL Teller > 20
               MOVE ZERO       TO Weekomzetten(Teller)
           END-PERFORM
           .
       RH60-VerwerkReservering.
           *> Dit is de laatste subroutine waar we directe toegang hebben tot een gelezen record.
           *> Daarom vind hier de verwerking plaats. In dit geval: het ophogen van de weekomzet.
           IF (FS-R-Jaar EQUALS 2023)
               PERFORM VARYING Teller FROM 0 BY 1 UNTIL Teller EQUALS FS-R-AantalWeken
                   ADD 10      TO Weekomzetten(FS-R-Weeknummer - 17 + Teller)
               END-PERFORM
           END-IF
           .

       AnnuleerBoeking.
           .
       AnnuleerReservering.
           .
       BetaalReservering.
           .
       VerwerkMutaties.
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
                   PERFORM RM61-LeesVolgendeReserveringOrigineel
                   PERFORM RM73-AfsluitenReserveringsnummer
                   PERFORM RM79-ZetIteratieVoorwaarde
               END-PERFORM
               PERFORM RM89-AfsluitenVerwerking
           END-IF
           PERFORM RM99-AfsluitenProgramma
           .
       ToonAlleBoekingen.
           .

       RM90-InitialiseerProgramma.
           PERFORM GetDatumVandaag
           SET KSK-NotAllEOF   TO TRUE
           OPEN I-O ReserveringenBestand
           DISPLAY ">>> Opening ReserveringenBestand. IOStatus: " IOStatus
           OPEN INPUT MutatieBestand
           DISPLAY ">>> Opening MutatieBestand. IOStatus: " IOStatus
           PERFORM RM61-LeesVolgendeReserveringOrigineel
           PERFORM RM63-LeesVolgendeMutatie
           PERFORM RM79-ZetIteratieVoorwaarde
           .
       RM80-InitialiseerVerwerking.
           .
       RM70-InitialiseerReserveringsnummer.
           SET GeenWijziging   TO TRUE
           SET GeenReserveringVerwerkt
                               TO TRUE
           MOVE FS-R-DatumCreatie
                               TO Reserveringsdatum
           .
       RM60-VerwerkReserveringOrigineel.
           SET ReserveringVerwerkt
                               TO TRUE
           .
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
           END-IF
           .
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
           END-IF
           .
       RM63-LeesVolgendeMutatie.
           READ MutatieBestand NEXT RECORD
               AT END
                   SET SKM-M-EOF
                               TO TRUE
               NOT AT END
                   MOVE FS-M-Reserveringsnummer
                               TO SKM-M-Reserveringsnummer
           END-READ
           .
       RM72-TussenInit.
           IF (DatumVandaag - Reserveringsdatum > 5)
               MOVE DatumVandaag
                               TO FS-R-DatumVerlopen
               SET WijzigingTeVerwerken
                               TO TRUE
           END-IF
           .
       RM64-BouwReserveringGemuteerd.
           .
       RM65-SchrijfReserveringGemuteerd.
           REWRITE Reserveringsrecord
           .
       RM73-AfsluitenReserveringsnummer.
           .
       RM79-ZetIteratieVoorwaarde.
           IF (SKM-RM < SKM-M)
               MOVE SKM-RM     TO KSK
           ELSE
               MOVE SKM-M      TO KSK
           END-IF
           .
       RM89-AfsluitenVerwerking.
           .
       RM99-AfsluitenProgramma.
           CLOSE ReserveringenBestand
           CLOSE MutatieBestand
           .
       GetDatumVandaag.
           MOVE FUNCTION CURRENT-DATE (1:8)
                               TO DatumVandaag
           .
