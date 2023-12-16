       IDENTIFICATION DIVISION.
       PROGRAM-ID. ELEVES.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. PERSONNAL PC.
       DATE-WRITTEN. 22-11-2023.
       DATE-COMPILED. 22-11-2023.
       SECURITY. NON-CONFIDENTIAL.
       
      ******************************************************************
      * OBJECTIF: Créer un programme avec la déclaration d’un tableau en
      * deux dimensions pouvant accueillir les tables TablELEV, TablNOTE 
      * et TablDEV.
      * Le programme doit charger dans le tableau le fichier ELEVES.
      * Le programme doit décharger le tableau listant les élèves en sortie
      * DANS UN FICHIER SEQUENTIEL :
      * NOM PRENOM AGE MATIERE COEFICIENT NOTE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE-INPUT ASSIGN TO "ELEVES-INPUT.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-INPUT.
       
           SELECT FILE-OUTPUT ASSIGN TO "ELEVES-OUTPUT.txt"
      *    ORGANIZATION IS LINE SEQUENTIAL
      *    ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-OUTPUT.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD FILE-INPUT
           RECORDING MODE IS F.
       01  REC-INPUT PIC X(36).
       
       FD  FILE-OUTPUT
           RECORDING MODE IS F.
       01  REC-OUTPUT PIC X(120).  
       
       WORKING-STORAGE SECTION.
       01  WS-STATUS-FILE-INPUT PIC X(02).
           88 WS-STATUS-FILE-INPUT-OK VALUE "00".
           88 WS-STATUS-FILE-INPUT-EOF VALUE "10".
       
       01 WS-STATUS-FILE-OUTPUT PIC X(02).
           88 WS-STATUS-FILE-OUTPUT-OK VALUE "00".
       01  WS-COMPTEUR-FILE-INPUT PIC 9(02) VALUE ZEROES.
       01  WS-COMPTEUR-FILE-OUTPUT PIC 9(02) VALUE ZEROES.
       
      ***********************************************
      * DEFINTION ENREGISTREMENT DU FICHIER INPUT
      *********************************************** 
       01  WS-INPUT-REC.
           05  REC00-NUMERO PIC X(02).
               88 WS-REC01 VALUE "01".
               88 WS-REC02 VALUE "02".
           05  FILLER PIC X(34).
       01  WS-INPUT-REC01 REDEFINES WS-INPUT-REC.
           05 REC01-NUMERO PIC X(02).
           05 REC01-PRENOM PIC X(07).
           05 REC01-NOM    PIC X(07).
           05 REC01-AGE    PIC 9(02). 
           05 FILLER       PIC X(18).     
       01  WS-INPUT-REC02 REDEFINES WS-INPUT-REC.    
           05 REC02-NUMERO  PIC X(02).       
           05 REC02-MATIERE PIC X(26).     
           05 REC02-COEFF.  
               10 REC02-COEFF-ENTIER  PIC 9(01). 
               10 FILLER              PIC X(01). 
               10 REC02-COEFF-DECIMAL PIC 9(01).
           05 REC02-NOTE.
               10 REC02-NOTE-ENTIER  PIC 9(02).
               10 FILLER             PIC X(01).
               10 REC02-NOTE-DECIMAL PIC 9(02).

      ***********************************************
      * DEFINTION ENREGISTREMENT DU FICHIER OUTPUT
      ***********************************************
       01  ENTETE.
           05 FILLER PIC X(08) VALUE "NOM     ".
           05 FILLER PIC X(07) VALUE "PRENOM ".
           05 FILLER PIC X(04) VALUE "AGE ".
           05 FILLER PIC X(26) VALUE "MATIERE                   ".
           05 FILLER PIC X(07) VALUE "COEFF. ".
           05 FILLER PIC X(04) VALUE "NOTE".
       01  WS-REC-OUTPUT.
           05 OUTPUT-PRENOM PIC X(07).
           05 FILLER PIC X(01).
           05 OUTPUT-NOM PIC X(07).
           05 FILLER PIC X(01).
           05 OUTPUT-AGE PIC 9(02).
           05 FILLER PIC X(01).
           05 OUTPUT-MATIERE PIC X(26).
           05 FILLER PIC X(01).
           05 OUTPUT-COEFF.
               10 OUTPUT-COEFF-ENTIER PIC 9(01).
               10 FILLER PIC X(01).
               10 OUTPUT-COEFF-DECIMAL PIC 9(01).
           05 FILLER PIC X(02).
           05 OUTPUT-NOTE.
               10 OUTPUT-NOTE-ENTIER PIC 9(02).
               10 FILLER PIC X(01).
               10 OUTPUT-NOTE-DECIMAL PIC 9(02).
       01  WS-TAB-ELEVE.
           05 WS-TABLE-ELEVES OCCURS 9.
               10 WS-TAB-PRENOM PIC X(07).
               10 WS-TAB-NOM PIC X(07).
               10 WS-TAB-AGE PIC 9(02).
               10 WS-TAB-MATIERES OCCURS 7.
                   15 WS-TAB-MATIERE PIC X(26).
                   15 WS-TAB-COEFF.
                       20 WS-TAB-COEFF-ENTIER PIC 9(01).
                       20 FILLER PIC X(01).
                       20 WS-TAB-COEFF-DECIMAL PIC 9(01).
                   15 WS-TAB-NOTE.
                       20 WS-TAB-NOTE-ENTIER PIC 9(02).
                       20 FILLER PIC X(01).
                       20 WS-TAB-NATE-DECIMAL PIC 9(02).
       01  WS-MAX-ELEVE PIC 9(02) VALUE 9.
       01  WS-MAX-NOTE PIC 9(02) VALUE 7.
       01  WS-INDICE PIC 9(02) VALUE ZEROES.
       01  WS-INDICE2 PIC 9(02) VALUE ZEROES.
       
       PROCEDURE DIVISION.
       DEBUT-PROGRAMME.
   
      *******************************************************
      * PARAGRAPHE PRINCIPAL
      *******************************************************
           DISPLAY "DEBUT DE PROGRAMME".
           INITIALIZE WS-TAB-ELEVE.
           
           OPEN INPUT FILE-INPUT.
           
           IF WS-STATUS-FILE-INPUT-OK
               PERFORM TRAITEMENT-FICHIER-DEBUT THRU
                   TRAITEMENT-FICHIER-FIN
                   UNTIL NOT WS-STATUS-FILE-INPUT-OK
           ELSE
               DISPLAY "--> ERREUR OUVERTURE FICHIER INPUT STATUS: "
               WS-STATUS-FILE-OUTPUT.
           
           DISPLAY "NOMBRE D'ENREGISTREMENTS LUS: "
               WS-COMPTEUR-FILE-INPUT.
           
           CLOSE FILE-INPUT.
           
           OPEN OUTPUT FILE-OUTPUT.
           
           IF NOT WS-STATUS-FILE-OUTPUT-OK
               OPEN EXTEND FILE-OUTPUT.
           
           IF WS-STATUS-FILE-OUTPUT-OK
               PERFORM LECTURE-TABLE-DEBUT THRU
               LECTURE-TABLE-FIN
           ELSE
               DISPLAY "--> ERREUR OUVERTURE FICHIER OUTPUT STATUS: "
               WS-STATUS-FILE-OUTPUT.
           
           CLOSE FILE-OUTPUT.
           
           DISPLAY "NOMBRE D'ENREGISTREMENTS ECRITS: "
               WS-COMPTEUR-FILE-OUTPUT.
           DISPLAY "FIN DE PROGRAMME".

       FIN-PROGRAMME.
       
       STOP RUN.


       
      *******************************************************
      * TRAITEMENT DU FICHIER
      ******************************************************* 
       TRAITEMENT-FICHIER-DEBUT.
       
           PERFORM LECTURE-DEBUT THRU LECTURE-FIN
               UNTIL NOT WS-STATUS-FILE-INPUT-OK.
       
       TRAITEMENT-FICHIER-FIN.
           EXIT.
       


      *******************************************************
      * BOUCLE DE LECTRE SUR LE FICHIER INPUT
      *******************************************************
       LECTURE-DEBUT.
       
           READ FILE-INPUT NEXT RECORD INTO WS-INPUT-REC.
           
           IF WS-STATUS-FILE-INPUT-OK
               ADD 1 TO WS-COMPTEUR-FILE-INPUT
               PERFORM CHARGEMENT-TABLE-DEBUT THRU
                       CHARGEMENT-TABLE-FIN.
       LECTURE-FIN.
           EXIT.
       


      *******************************************************
      * CHARGEMENT DE LA TABLE
      *******************************************************
       CHARGEMENT-TABLE-DEBUT.

           IF WS-REC01
               IF WS-INDICE = WS-MAX-ELEVE
                   DISPLAY "--> ! PLUS DE " WS-MAX-ELEVE " ELEVES! "
               ELSE
                   ADD 1 TO WS-INDICE.
       
           IF WS-REC02
               IF WS-INDICE2 = WS-MAX-NOTE
                   DISPLAY "--> ! PLUS DE " WS-MAX-NOTE 
                   " MATIERES POUR "
                   REC01-PRENOM " "
                   REC01-NOM " "
                   REC01-AGE
               ELSE
                   ADD 1 TO WS-INDICE2.

           IF WS-REC01
               MOVE ZEROES TO WS-INDICE2
               MOVE REC01-PRENOM TO WS-TAB-PRENOM (WS-INDICE)
               MOVE REC01-NOM TO WS-TAB-NOM (WS-INDICE)
               MOVE REC01-AGE TO WS-TAB-AGE (WS-INDICE).
           
           IF WS-REC02
               MOVE REC02-MATIERE TO
               WS-TAB-MATIERE (WS-INDICE WS-INDICE2)
               MOVE REC02-COEFF TO WS-TAB-COEFF (WS-INDICE WS-INDICE2)
               MOVE REC02-NOTE TO WS-TAB-NOTE (WS-INDICE WS-INDICE2).
       
       CHARGEMENT-TABLE-FIN.
           EXIT.
       

      ***************************************************
      * GESTION DU FICHIER DE SORTIE
      ***************************************************
       LECTURE-TABLE-DEBUT.
       
           MOVE ZEROES TO WS-INDICE.

           PERFORM TRAIT-ELEVES-DEBUT THRU TRAIT-ELEVES-FIN
               VARYING WS-INDICE FROM 1 BY 1
               UNTIL WS-INDICE > WS-MAX-ELEVE.
       LECTURE-TABLE-FIN.
           EXIT.


      ***************************************************
      * TRAITEMENT DES ELEVES
      ***************************************************
       TRAIT-ELEVES-DEBUT.

           MOVE ZEROES TO WS-INDICE2.
       
           PERFORM ECRITURE-OUTPUT-DEBUT THRU ECRITURE-OUTPUT-FIN
               VARYING WS-INDICE2 FROM 1 BY 1
               UNTIL WS-INDICE2 > WS-MAX-NOTE.
       TRAIT-ELEVES-FIN.
           EXIT.


      ***************************************************
      * ECRITURE DU FICHIER DE SORTIE
      ***************************************************
       ECRITURE-OUTPUT-DEBUT.
       
           INITIALIZE REC-OUTPUT.
           INITIALIZE WS-REC-OUTPUT.

      *
      ****** ECRITURE ENTETE
           IF WS-COMPTEUR-FILE-OUTPUT = ZEROES
               WRITE REC-OUTPUT FROM ENTETE
               ADD 1 TO WS-COMPTEUR-FILE-OUTPUT.
       
           IF WS-TAB-MATIERE(WS-INDICE, WS-INDICE2) NOT = SPACES
               MOVE WS-TAB-PRENOM (WS-INDICE) TO OUTPUT-PRENOM
               MOVE WS-TAB-NOM (WS-INDICE) TO OUTPUT-NOM
               MOVE WS-TAB-AGE (WS-INDICE) TO OUTPUT-AGE
               MOVE WS-TAB-MATIERE (WS-INDICE, WS-INDICE2) TO
                        OUTPUT-MATIERE
               MOVE WS-TAB-COEFF (WS-INDICE, WS-INDICE2) TO OUTPUT-COEFF
               MOVE WS-TAB-NOTE (WS-INDICE, WS-INDICE2) TO OUTPUT-NOTE
               WRITE REC-OUTPUT FROM WS-REC-OUTPUT AFTER ADVANCING 1
               ADD 1 TO WS-COMPTEUR-FILE-OUTPUT.
       
       ECRITURE-OUTPUT-FIN.
           EXIT.

           