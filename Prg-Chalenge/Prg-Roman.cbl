       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGROMAN.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. PERSONAL COMPUTER.
       DATE-WRITTEN. 06-02-2024.
       DATE-COMPILED. 06-02-2024.
       SECURITY. NON-CONFIDENTIAL.
       
      ******************************************************************
      * OBJECTIF: Convertir un chiffre arabe en romain, max 4999
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      ******* ZONES DE TRAVAIL DE LA SAISIE
       01  WS-ACCEPTE    PIC X(04) VALUE SPACES.
       01  WS-ARABIC     PIC 9(04) VALUE ZEROES.
       01  WS-ROMAN      PIC X(20) VALUE SPACES.
       01  WS-INTERMED   PIC X(20) VALUE SPACES.
       01  I             PIC 9(02) VALUE ZEROES.

      ****** ZONE POUR ARRETER UNE BOUCLE
       01  WS-BOUCLE PIC X(01).
           88  WS-BOUCLE-EOF  VALUE "Y".
           88  WS-BOUCLE-NEOF VALUE "N".

      ****************************************************************
      ******* VALEUR DES CHIFFRES ROMAINS
      ****************************************************************
       01  WS-LISTE-CARACTERE.
           05 WS-CAR-ROMAN  PIC X(04) VALUE SPACES.
           05 WS-NBR-ARABIC PIC 9(04) VALUE ZEROES.

       01  WS-CARACT-NBRE  PIC X(03) VALUE SPACES.
       01  LINE-POS         PIC 999.
      *
      ****** ZONE POUR ACCEPTER DE CONTINUER OU NON D'EXECUTER LE PROGRAMME
       01  WS-REPONSE    PIC X(01).      
           88  WS-REPONSE-Y  VALUE "Y".
           88  WS-REPONSE-N  VALUE "N".
      
           
       PROCEDURE DIVISION.
       
      *******************************************************
      * PROGRAMME PRINCIPAL
      *******************************************************
       0000-MAIN-DEBUT.
           
      *     DISPLAY "DEBUT DE PROGRAMME."
      *
      ****** TRAITEMENT DE LA REPONSE POUR CONTINUER
           PERFORM UNTIL FUNCTION UPPER-CASE (WS-REPONSE) = "N"
              DISPLAY "ENTREZ UN NBRE nnnn (MAX 4999):" NO ADVANCING
              ACCEPT WS-ACCEPTE
      *
      ****** TRAITEMENT DU CHIFFRE SAISI
              IF WS-ACCEPTE > ZEROES AND < 5000
                 PERFORM 7000-TRAIT-ARABIC-ROMAN-DEBUT 
                    THRU 7000-TRAIT-ARABIC-ROMAN-FIN
              ELSE
                 DISPLAY "LE CHIFFRE DOIT ETRE > 0000 ET < 5000"
              END-IF
              MOVE SPACES TO WS-REPONSE
      *
      ****** CONTINUER LA SAISIE Y/N
             PERFORM UNTIL FUNCTION UPPER-CASE (WS-REPONSE) = "Y" OR "N"
                 DISPLAY "VOULEZ-VOUS CONTINUER? (Y/N)" NO ADVANCING
                 ACCEPT WS-REPONSE
                 IF NOT (WS-REPONSE-Y OR WS-REPONSE-N)
                    DISPLAY "LA REPONSE DOIR ETRE 'Y' OU 'N'"
                 END-IF
              END-PERFORM
           END-PERFORM.

      *     DISPLAY "FIN DE PROGRAMME".

       0000-MAIN-FIN.

           STOP RUN.

      ******************************************************************
      * TRAITEMENT DU CHIFFRE ARABE EN ROMAIN
      ******************************************************************
       7000-TRAIT-ARABIC-ROMAN-DEBUT.
           
      *     DISPLAY "7000-TRAIT-ARABIC-ROMAN-DEBUT".

           MOVE SPACES     TO WS-ROMAN.
           MOVE ZEROES     TO I.
           MOVE "N"        TO WS-BOUCLE.
           MOVE WS-ACCEPTE TO WS-ARABIC.
      *
      ****** BOUCLE DE LECTURE SUR LE MOT SAISI
      *     PERFORM VARYING I FROM 1 BY 1 UNTIL WS-BOUCLE-EOF
           PERFORM UNTIL WS-BOUCLE-EOF
              MOVE WS-ROMAN TO WS-INTERMED
              MOVE SPACES   TO WS-ROMAN
              MOVE SPACES   TO WS-CAR-ROMAN
              MOVE ZEROES   TO WS-NBR-ARABIC
      *
      ****** "M" : 1000 à 4000
              EVALUATE TRUE
                 WHEN WS-ARABIC > 999
                    MOVE "M"   TO WS-CAR-ROMAN 
                    MOVE 1000  TO WS-NBR-ARABIC
                    DISPLAY "M" NO ADVANCING
      *         
      ****** "C" : 100 à 300
                 WHEN WS-ARABIC > 099 AND < 400 
                    MOVE "C"   TO WS-CAR-ROMAN 
                    MOVE 100   TO WS-NBR-ARABIC
                    DISPLAY "C" NO ADVANCING      
      *         
      ****** "CD" : 400
                 WHEN WS-ARABIC > 399 AND < 500 
                    MOVE "CD"   TO WS-CAR-ROMAN 
                    MOVE 400   TO WS-NBR-ARABIC
                    DISPLAY "CD" NO ADVANCING               
      *         
      ****** "D" : 500
                 WHEN WS-ARABIC > 499 AND < 600 
                    MOVE "D"   TO WS-CAR-ROMAN 
                    MOVE 500   TO WS-NBR-ARABIC
                    DISPLAY "D" NO ADVANCING               
      *         
      ****** "DC" : 600
                 WHEN WS-ARABIC > 599 AND < 700 
                    MOVE "DC"   TO WS-CAR-ROMAN 
                    MOVE 600   TO WS-NBR-ARABIC
                    DISPLAY "DC" NO ADVANCING               
      *         
      ****** "DCC" : 700
                 WHEN WS-ARABIC > 699 AND < 800 
                    MOVE "DCC"   TO WS-CAR-ROMAN 
                    MOVE 700   TO WS-NBR-ARABIC
                    DISPLAY "DCC" NO ADVANCING               
      *         
      ****** "DCC" : 800
                 WHEN WS-ARABIC > 799 AND < 900 
                    MOVE "DCCC"   TO WS-CAR-ROMAN 
                    MOVE 800    TO WS-NBR-ARABIC
                    DISPLAY "DCCC" NO ADVANCING               
      *         
      ****** "DCC" : 900
                 WHEN WS-ARABIC > 899 AND < 1000 
                    MOVE "CM"   TO WS-CAR-ROMAN 
                    MOVE 900    TO WS-NBR-ARABIC
                    DISPLAY "CM" NO ADVANCING               
      *         
      ****** "X" : 10 à 30
                 WHEN WS-ARABIC > 09 AND < 40 
                    MOVE "X"   TO WS-CAR-ROMAN 
                    MOVE 10    TO WS-NBR-ARABIC
                    DISPLAY "X" NO ADVANCING      
      *         
      ****** "XL" : 40
                 WHEN WS-ARABIC > 39 AND < 50 
                    MOVE "XL"   TO WS-CAR-ROMAN 
                    MOVE 40   TO WS-NBR-ARABIC
                    DISPLAY "XL" NO ADVANCING      
      *         
      ****** "L" : 50
                 WHEN WS-ARABIC > 49 AND < 60 
                    MOVE "L"   TO WS-CAR-ROMAN 
                    MOVE 50   TO WS-NBR-ARABIC
                    DISPLAY "L" NO ADVANCING      
      *         
      ****** "LX" : 60
                 WHEN WS-ARABIC > 59 AND < 70 
                    MOVE "LX"   TO WS-CAR-ROMAN 
                    MOVE 60   TO WS-NBR-ARABIC
                    DISPLAY "LX" NO ADVANCING      
      *         
      ****** "LXX" : 70
                 WHEN WS-ARABIC > 69 AND < 80 
                    MOVE "LXX"   TO WS-CAR-ROMAN 
                    MOVE 70   TO WS-NBR-ARABIC
                    DISPLAY "LXX" NO ADVANCING      
      *         
      ****** "LXXX" : 80
                 WHEN WS-ARABIC > 79 AND < 90 
                    MOVE "LXXX"   TO WS-CAR-ROMAN 
                    MOVE 80   TO WS-NBR-ARABIC
                    DISPLAY "LXXX" NO ADVANCING      
      *         
      ****** "XC" : 90
                 WHEN WS-ARABIC > 89 AND < 100 
                    MOVE "XC"   TO WS-CAR-ROMAN 
                    MOVE 90   TO WS-NBR-ARABIC
                    DISPLAY "XC" NO ADVANCING      
      *         
      ****** "I" : 1 à 3
                 WHEN WS-ARABIC > 0 AND < 4 
                    MOVE "I"   TO WS-CAR-ROMAN 
                    MOVE 1    TO WS-NBR-ARABIC
                    DISPLAY "I" NO ADVANCING      
      *         
      ****** "IV" : 4
                 WHEN WS-ARABIC = 4 
                    MOVE "IV"   TO WS-CAR-ROMAN 
                    MOVE 4    TO WS-NBR-ARABIC
                    DISPLAY "IV" NO ADVANCING      
      *         
      ****** "V" : 5
                 WHEN WS-ARABIC = 5 
                    MOVE "V"   TO WS-CAR-ROMAN 
                    MOVE 5    TO WS-NBR-ARABIC
                    DISPLAY "V" NO ADVANCING      
      *         
      ****** "VI" : 6
                 WHEN WS-ARABIC = 6 
                    MOVE "VI"   TO WS-CAR-ROMAN 
                    MOVE 6    TO WS-NBR-ARABIC
                    DISPLAY "VI" NO ADVANCING            
      *         
      ****** "VII" : 7
                 WHEN WS-ARABIC = 7 
                    MOVE "VII"   TO WS-CAR-ROMAN 
                    MOVE 7    TO WS-NBR-ARABIC
                    DISPLAY "VII" NO ADVANCING      
      *         
      ****** "VIII" : 8
                 WHEN WS-ARABIC = 8 
                    MOVE "VIII"   TO WS-CAR-ROMAN 
                    MOVE 8    TO WS-NBR-ARABIC
                    DISPLAY "VIII" NO ADVANCING      
      *         
      ****** "IX" : 9
                 WHEN WS-ARABIC = 9 
                    MOVE "IX"   TO WS-CAR-ROMAN 
                    MOVE 9    TO WS-NBR-ARABIC
                    DISPLAY "IX" NO ADVANCING      
      *
      ******
                 WHEN OTHER               
                    MOVE "Y" TO WS-BOUCLE
              END-EVALUATE
      *
      ****** CONCATENATION DU CHIFFRE EN COURS AVEC LE NOMBRE PRECEDENT
              STRING WS-INTERMED   DELIMITED BY SPACE, 
                     WS-CAR-ROMAN  DELIMITED BY SPACE 
                                   INTO WS-ROMAN
              SUBTRACT WS-NBR-ARABIC FROM WS-ARABIC
      *
      ****** FIN DE TRAITEMENT DE TOUS LES CHIFFRES? FIN DE BOUCLE
                 IF WS-ARABIC = ZEROES
                    MOVE "Y" TO WS-BOUCLE
                 END-IF
           END-PERFORM.             
      * 
      ****** AFFICHAGE DU RESULTAT
           DISPLAY "==================================================".
              DISPLAY "LE CHIFFRE ROMAIN EST " WS-ROMAN.

      *     DISPLAY "7000-TRAIT-ARABIC-ROMAN-FIN".

       7000-TRAIT-ARABIC-ROMAN-FIN.
           EXIT.
