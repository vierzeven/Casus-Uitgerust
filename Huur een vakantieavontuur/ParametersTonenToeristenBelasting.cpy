      * Copybook1 
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