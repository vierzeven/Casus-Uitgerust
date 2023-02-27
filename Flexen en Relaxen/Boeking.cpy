      * Copybook Boeking
       01 (pf)-Boeking.
         02 (pf)-Naam PIC X(24).
         02 (pf)-Werkpleknummer.
           03 (pf)-Huisnummer PIC 99.
           03 (pf)-Kamernummer PIC 9.
         02 (pf)-SleutelUitgereiktVlag PIC 9 VALUE ZERO.
           88 (pf)-SleutelUitgereikt VALUE 1.
           88 (pf)-SleutelNietUitgereikt VALUE 0.

