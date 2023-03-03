IDENTI ICATION DIVISION.
       PROGRAM-ID. TestTonenToeristenBelasting.
       AUTHOR. Ani en Max.
       DATE-WRITTEN. 03-03-2023.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VerwachteUitkomst PIC X(9).
       01 VerwachteReturnCode PIC XX.
         88 VerwachteRC-OK VALUE ZERO.
         88 VerwachteRC-IllegalParameter VALUE "99".
       01 AantalGeslaagdeTesten PIC S9(3) PACKED-DECIMAL VALUE ZERO.
       01 AantalMislukteTesten PIC S9(3) PACKED-DECIMAL VALUE ZERO.
       COPY ParametersToonToeristenBelasting REPLACING ==(pf)== BY ==WS==.
       
       01 (pf)-C-ParametersTonenToeristenBelasting.
         03 (pf)-C-WeekNummer PIC 99.
         03 (pf)-C-AantalWeken PIC 99.
         03 (pf)-C-Leeftijd PIC 99.
        *>By Reference
       01 (pf)-R-ParametersTonenToeristenBelasting.
         03 (pf)-R-ToeristenBelastingTonen PIC 9(5).
         03 (pf)-R-ReturnCode PIC 99.
           88 (pf)-R-OK VALUE ZEROES.
           88 (pf)-R-IllegalParameter VALUE 99.
       
       PROCEDURE DIVISION.
       BeginProgram.
           *>    DISPLAY "UnitTest Tonen Toeristen Belasting"
       
           PERFORM Test-Nul-Fout.
           PERFORM Test-Rand-Getal-Klein-Fout.
           PERFORM Test-Positief-Getal-Goed.
           PERFORM Test-Rand-Getal-Groot-Goed.
           PERFORM Test-Rand-Getal-Groot-Fout.
           PERFORM Test-Groot-Getal-Fout.
       
           PERFORM Test-Niet-Numeriek-Getal-Fout.
       
           DISPLAY "Aantal geslaagde testen: " AantalGeslaagdeTesten
           DISPLAY "Aantal mislukte testen:  " AantalMislukteTesten
           STOP RUN.
       
       Test-Nul-Fout.
           MOVE 0 TO WS-C-WeekNummer
           SET VerwachteRC-IllegalParameter TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Rand-Getal-Klein-Fout.
           MOVE 1 TO WS-C-WeekNummer
           SET VerwachteRC-IllegalParameter TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Positief-Getal-Goed.
           MOVE 18 TO WS-C-WeekNummer
           MOVE 18 TO WS-C-Leeftijd
           MOVE 2 TO WS-C-AantalWeken
           MOVE 21 TO VerwachteUitkomst
           SET VerwachteRC-OK TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Rand-Getal-Groot-Goed.
           MOVE 37 TO WS-C-WeekNummer
           MOVE 121 TO WS-C-Leeftijd
           MOVE 1 TO WS-C-AantalWeken
           MOVE 21 TO VerwachteUitkomst
           SET VerwachteRC-OK TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Rand-Getal-Groot-Fout.
           MOVE 37 TO WS-C-WeekNummer
           MOVE 121 TO WS-C-Leeftijd
           MOVE 2 TO WS-C-AantalWeken
           SET VerwachteRC-IllegalParameter TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Groot-Getal-Fout.
           MOVE 38 TO WS-C-WeekNummer
           MOVE 121 TO WS-C-Leeftijd
           MOVE 2 TO WS-C-AantalWeken
           SET VerwachteRC-IllegalParameter TO TRUE
           PERFORM UitvoerenTestCase.
       
       Test-Niet-Numeriek-Getal-Fout.
           MOVE SPACES TO WS-C-ParametersTonenToeristenBelasting
           SET VerwachteRC-IllegalParameter TO TRUE
           PERFORM UitvoerenTestCase.
       
       UitvoerenTestCase.
           CALL "TonenToeristenBelasting" USING BY CONTENT WS-C-ParameterstonenToeristenBelasting,
                                      BY REFERENCE WS-R-ParametersTonenToeristenBelasting
           IF WS-R-ReturnCode EQUALS VerwachteReturnCode
               IF WS-R-OK
                   IF WS-R-ToeristenBelastingTonen EQUALS VerwachteUitkomst
                       ADD 1 TO AantalGeslaagdeTesten
                   ELSE
                       DISPLAY "Test geeft niet de verwachte uitkomst: "
                       ADD 1 TO AantalMislukteTesten
                   END-IF
               ELSE
                   ADD 1 TO AantalGeslaagdeTesten
               END-IF
           ELSE
               DISPLAY "Test geeft niet de verwachte uitkomst "
                 " ReturnCode: " WS-R-ReturnCode
               ADD 1 TO AantalMislukteTesten
           END-IF.
       
