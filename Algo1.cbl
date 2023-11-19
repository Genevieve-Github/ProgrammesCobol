       IDENTIFICATION DIVISION. 
       PROGRAM-ID. COMPARE. 
       AUTHOR. GENEVIEVE GIANNASI. 
       INSTALLATION. MAC. 
       DATE-WRITTEN. 09-11-2023. 
       DATE-COMPILED. 09-11-2023. 
       SECURITY. NON-CONFIDENTIAL. 
      ****************************************************************** 
      * OBJECTIF: COMPARER 2 VARIABLES NUMERIQUES 
      ****************************************************************** 
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES. 
            DECIMAL-POINT IS COMMA. 
      
       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01  WS-SOMME1                   PIC 9(04) VALUE 10. 
       01  WS-SOMME2                   PIC 9(04) VALUE 243. 
 
       PROCEDURE DIVISION. 
      
       DEBUT-PROGRAMME. 
      ******************************************************* 
      *  PARAGRAPHE PRINCIPAL 
      ******************************************************* 
          
           DISPLAY "VARIABLE1: " WS-SOMME1 ", VARIABLE2: " WS-SOMME2. 
 
           IF WS-SOMME1 = WS-SOMME2 
              DISPLAY "VARIABLE1 = VARIABLE2! " 
           END-IF. 
           IF WS-SOMME1 > WS-SOMME2 
              DISPLAY "VARIABLE1 > VARIABLE2! " 
           END-IF. 
           IF WS-SOMME1 < WS-SOMME2 
              DISPLAY "VARIABLE1 < VARIABLE2! " 
           END-IF. 
 
       FIN-PROGRAMME. 
           STOP RUN. 
 