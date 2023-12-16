       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCALLER.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. PERSONNAL PC.
       DATE-WRITTEN. 30-11-2023.
       DATE-COMPILED. 30-11-2023.
       SECURITY. NON-CONFIDENTIAL.
      ******************************************************************
      * OBJECTIF: Utilisation des sous-programmes
      * Ce sous-programme est appelé par un programme principal
      * Afficher un display pour indique le passage dans ce ss-programme
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PARAM1  PIC X(40).
       01  WS-PARAM2  PIC X(40).
         
      *******************************************************
      *  PARAGRAPHE PRINCIPAL
      *******************************************************       
       PROCEDURE DIVISION.
       DEBUT-PROGRAMME.

           DISPLAY "PROGRAMME APPELANT: DEBUT DE TRAITEMENT".  
           
      *
      ****** CALL PAR REFERENCE (CALL PAR DEFAUT)
           MOVE "--> HELLO PROGRAMME APPELE" TO WS-PARAM1.  
           MOVE "--> COMMENT VAS-TU?"        TO WS-PARAM2.  
           DISPLAY "--> JE FAIS UN CALL PAR REFERENCE!".
           DISPLAY "----------------------------------------".
           DISPLAY "PARAMETRE 1: " WS-PARAM1.
           DISPLAY "PARAMETRE 2: " WS-PARAM2.
           CALL 'PRGCALLED' USING WS-PARAM1 WS-PARAM2.
           DISPLAY "PARAMETRE 1: " WS-PARAM1.
           DISPLAY "PARAMETRE 2: " WS-PARAM2.

      *
      ****** CALL PAR CONTENT 
           MOVE "--> HELLO PROGRAMME APPELE" TO WS-PARAM1.  
           MOVE "--> COMMENT VAS-TU?"        TO WS-PARAM2.  
           DISPLAY "--> JE FAIS UN CALL PAR CONTENT!".
           DISPLAY "----------------------------------------".
           DISPLAY "PARAMETRE 1: " WS-PARAM1.
           DISPLAY "PARAMETRE 2: " WS-PARAM2.
           CALL 'PRGCALLED' USING BY CONTENT WS-PARAM1,
                                  BY CONTENT WS-PARAM2.

           DISPLAY "--> PROGRAMME APPELANT: FIN DE TRAITEMENT".    

       FIN-PROGRAMME.
           STOP RUN.
