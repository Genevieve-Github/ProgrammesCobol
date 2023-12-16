       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCALLED.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. PERSONNAL PC.
       DATE-WRITTEN. 30-11-2023.
       DATE-COMPILED. 30-11-2023.
       SECURITY. NON-CONFIDENTIAL.
      ******************************************************************
      * OBJECTIF: Utilisation des sous-programmes
      * Ce programme appelle un sous-programme
      * Afficher un display pour indique le passage dans ce programme
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
           01  LS-PARAM1  PIC X(40).
           01  LS-PARAM2  PIC X(40).
      *******************************************************
      *  PARAGRAPHE PRINCIPAL
      *******************************************************       
       PROCEDURE DIVISION USING LS-PARAM1, LS-PARAM2.
       DEBUT-PROGRAMME.

           DISPLAY "PROGRAMME APPELE: DEBUT DE TRAITEMENT".           
           
           DISPLAY "PARAMETRE 1: " LS-PARAM1.
           DISPLAY "PARAMETRE 2: " LS-PARAM2.

           MOVE "--> JE VAIS BIEN MERCI."               TO LS-PARAM1.
           MOVE "--> ET TOI, CA VA PROGRAMME APPELANT?" TO LS-PARAM2.

           DISPLAY "JE CHANGE LES PARAMETRES DANS L'APPELE!".
           DISPLAY "PARAMETRE 1: " LS-PARAM1.
           DISPLAY "PARAMETRE 2: " LS-PARAM2.

           DISPLAY "PROGRAMME APPELE: FIN DE TRAITEMENT".  
            
       FIN-PROGRAMME.
           EXIT PROGRAM.
