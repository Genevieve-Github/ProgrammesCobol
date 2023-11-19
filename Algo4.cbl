       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOUCLE.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. VIRTUAL BOX.
       DATE-WRITTEN. 13-11-2023.
       DATE-COMPILED. 13-11-2023.
       SECURITY. NON-CONFIDENTIAL.
      ******************************************************************
      * OBJECTIF: LIRE LES 3 PREMIERS ENREGISTREMENTS DU FICHIER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE01 ASSIGN TO "ASSURANCES.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE01.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE01
           RECORDING MODE IS F.
       01  REC01.
           05  REC01-MUNACTX.
               10  REC01-NUMACT    PIC 9(08).
           05  REC01-IDENT         PIC X(14).
           05  REC01-LIBELLE       PIC X(101).
           05  REC01-DATEDEBUTX.
               10  REC01-DATEDEBUT PIC 9(08).
           05  REC01-DATEFINX.
               10  REC01-DATEFIN   PIC 9(08).

       WORKING-STORAGE SECTION.
       77  WS-STATUS-FILE01        PIC X(02) VALUE SPACES.
       77  WS-COMPTEUR-FILE01      PIC 9(02) VALUE ZEROES.

       PROCEDURE DIVISION.
       DEBUT-PROGRAMME.
      *******************************************************
      *  OUVERTURE SUR FICHIER
      *******************************************************
           OPEN INPUT FILE01.
         
           PERFORM LECTURE-DEBUT THRU LECTURE-FIN 3 TIMES.
         
           DISPLAY "NOMBRE D'ENREGISTREMENTS LUS: "
                   WS-COMPTEUR-FILE01.

           CLOSE FILE01.

       FIN-PROGRAMME.
           STOP RUN.



      *******************************************************
      *  BOUCLE DE LECTRE SUR LE FICHIER
      *******************************************************
       LECTURE-DEBUT.
         
           READ FILE01 NEXT.

           IF WS-STATUS-FILE01 = "00"
               ADD 1 TO WS-COMPTEUR-FILE01
               DISPLAY "RECORD " WS-COMPTEUR-FILE01 ": " REC01
           END-IF.

       LECTURE-FIN.
           EXIT.
