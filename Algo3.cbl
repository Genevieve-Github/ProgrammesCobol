       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATION.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. MAC.
       DATE-WRITTEN. 13-11-2023.
       DATE-COMPILED. 13-11-2023.
       SECURITY. NON-CONFIDENTIAL.
      ******************************************************************
      * OBJECTIF: OUVRIR UN FICHIER ET S'IL N'EXISTE PAS LE CREER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE01 ASSIGN TO "JOURNAL.txt"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE01.


       DATA DIVISION.
       FILE SECTION.
       FD  FILE01
           RECORDING MODE IS F.
       01  REC01.
           05  REC01-VALEUR1   PIC 9(04).
           05  FILLER          PIC X(02).
           05  REC01-VALEUR2   PIC 9(04).
           05  FILLER          PIC X(02).
           05  REC01-SOMME     PIC 9(04).
           05  FILLER          PIC X(02).


       WORKING-STORAGE SECTION.
       77  WS-STATUS-FILE01        PIC X(02) VALUE SPACES.
       77  WS-COMPTEUR-FILE01      PIC 9(02) VALUE ZEROES.


       PROCEDURE DIVISION.
       
       DEBUT-PROGRAMME.

      *******************************************************
      *  OUVERTURE SUR FICHIER
      *******************************************************
           OPEN EXTEND FILE01.
         
           IF WS-STATUS-FILE01 NOT = "00"
              OPEN OUTPUT FILE01
              IF WS-STATUS-FILE01 = "00"
                  DISPLAY "FICHIER CREE"
              else
                  DISPLAY "ERREUR CREATION FICHIER FILE01: "
                      WS-STATUS-FILE01.


           IF WS-STATUS-FILE01 = "00"
              MOVE LOW-VALUE TO REC01
              MOVE 10 TO REC01-VALEUR1
              MOVE 20 TO REC01-VALEUR2
              COMPUTE REC01-SOMME = REC01-VALEUR1 + REC01-VALEUR2
              WRITE REC01
           END-IF.
         
           CLOSE FILE01.


       FIN-PROGRAMME.
           STOP RUN.    
         
