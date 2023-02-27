       IDENTIFICATION DIVISION.
       PROGRAM-ID. 00b-AanmakenTestbestand.
       AUTHOR. Trainee StormPunt iTrack.
       DATE-WRITTEN. 18-01-2023.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ReserveringenBestand
               ASSIGN TO "C:\COBOL\DATA\CasusFlexenEnRelaxen\Reserveringen.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS IO-Status.

       DATA DIVISION.
       FILE SECTION.

       FD ReserveringenBestand.
       01 ReserveringRecord.
         02 R-Naam  PIC X(24).
         02 R-Datum PIC 9(8).

       WORKING-STORAGE SECTION.
       78 AantalHuisjes VALUE 10.

       01 IO-Status     PIC XX.
         88 IO-OK           VALUE ZEROES.
         88 IO-FileNotFound VALUE "35".

       01 Huisjes.
         02 HuisjesWaarden.
           03 FILLER PIC X(3) VALUE "01S".
           03 FILLER PIC X(3) VALUE "02L".
           03 FILLER PIC X(3) VALUE "03L".
           03 FILLER PIC X(3) VALUE "04S".
           03 FILLER PIC X(3) VALUE "05S".
           03 FILLER PIC X(3) VALUE "06S".
           03 FILLER PIC X(3) VALUE "07L".
           03 FILLER PIC X(3) VALUE "08L".
           03 FILLER PIC X(3) VALUE "09S".
           03 FILLER PIC X(3) VALUE "10S".
       01 FILLER REDEFINES Huisjes.
         02 Huisje OCCURS AantalHuisjes.
           03 H-Huisnummer PIC 99.
           03 H-Type       PIC X.
             88 H-Luxe      VALUE "L".
             88 H-Standaard VALUE "S".


       01 Datums.
         03 DatumVandaag     PIC X(8).
         03 DatumHuidig      PIC 9(8).
         03 DatumInt         PIC 9(8).

       PROCEDURE DIVISION.
       BeginProgram.
           PERFORM Initialiseren
           PERFORM TestbestandMaken
           STOP RUN
           .

       Initialiseren.
           *> ReserveringenBestand Checken
           OPEN INPUT ReserveringenBestand
           IF IO-FileNotFound
               DISPLAY "Reserveringen niet gevonden. Nieuw bestand aangemaakt. Filestatus: " IO-Status
               OPEN OUTPUT ReserveringenBestand
           END-IF
           CLOSE ReserveringenBestand
           .

       TestbestandMaken.
           OPEN OUTPUT ReserveringenBestand
           MOVE FUNCTION CURRENT-DATE TO DatumVandaag
           MOVE DatumVandaag          TO DatumHuidig
           COMPUTE DatumInt = FUNCTION INTEGER-OF-DATE (DatumHuidig)

           MOVE "James"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Willem"       TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 4)
           WRITE ReserveringRecord
           MOVE "Diana"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 3)
           WRITE ReserveringRecord
           MOVE "Eveline"      TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Henri"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 6)
           WRITE ReserveringRecord
           MOVE "Willem"       TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Henri"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 1)
           WRITE ReserveringRecord
           MOVE "Max"         TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 6)
           WRITE ReserveringRecord
           MOVE "James"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 4)
           WRITE ReserveringRecord
           MOVE "Willem"       TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Max"         TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Tjeerd"       TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt + 1)
           WRITE ReserveringRecord
           MOVE "James"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "Willem"       TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord
           MOVE "James"        TO R-Naam
           COMPUTE R-Datum = FUNCTION DATE-OF-INTEGER (DatumInt)
           WRITE ReserveringRecord

           CLOSE ReserveringenBestand
           DISPLAY "Testbestand Reserveringen gemaakt"
           DISPLAY SPACE
           .
