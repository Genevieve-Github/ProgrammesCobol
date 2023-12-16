       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTURES.
       AUTHOR. GENEVIEVE GIANNASI.
       INSTALLATION. PERSONAL PC.
       DATE-WRITTEN. 13-12-2023.
       DATE-COMPILED. 13-12-2023.
       SECURITY. NON-CONFIDENTIAL.
      ******************************************************************
      * OBJECTIF: 
      * Permettre de générer un fichier de facturation organisé en sortie
      * comme suit :  
      ******************************************************************
      *  Le traitement lit trois fichiers en entrée: 
      *  Le fichier client, la tables des départements et le fichier des
      *  assurances
      *  
      *  Le fichier FCLISOR doit être :
      *    Trié par département
      *    Trié par nom
      *    Editer le code et le nom de l’assureur
      *    Éditer le code et le nom du département
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******** FICHIER EMPLOYE
           SELECT FCLIENT ASSIGN TO "FICHIERCLIENT.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-FCLIENT.

      ******** FICHIER DEPARTEMENT
           SELECT FDEPT ASSIGN TO "FR-LISTE-DEPT.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-FDEPT.

      ******** FICHIER ASSURANCES
           SELECT FASSUR ASSIGN TO "ASSURANCES.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-FASSUR.

      ******** FICHIER DE SORTIE
           SELECT FCLISOR ASSIGN TO "FICHIERSOR.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-STATUS-FILE-FCLISOR.

      ******** FICHIER DE TRI DU FICHIER CLIENT
           SELECT SCLISOR ASSIGN TO SORTFILE.

      ******** FICHIER DE TRI DU FICHIER ASSUREUR
           SELECT SASSUR ASSIGN TO SORTFILE.
      
      ******** FICHIER DE TRI DU FICHIER DEPARTEMENTS
           SELECT SDEPT ASSIGN TO SORTFILE.

       DATA DIVISION.
       FILE SECTION.
      ******** FICHIER CLIENT
           COPY 'COBOL-FD-FCLIENT.CPY'.        

      ******** FICHIER DEPARTEMENT
           COPY 'COBOL-FD-DEPT.cpy'.

      ******** FICHIER ASSURANCES
           COPY 'COBOL-FD-ASSURANCE.cpy'.

      ******** FICHIER DE SORTIE
           COPY 'COBOL-FD-FCLISOR.cpy'.

      ******** FICHIER DE TRI DES CLIENTS
           COPY 'COBOL-FD-FCLISOR.cpy'
               REPLACING LEADING ==FD== BY ==SD== 
                         LEADING ==FCLISOR== BY ==SCLISOR==
                         LEADING ==RCLISOR== BY ==SCLISOR-REC==.  

      ******** FICHIER DE TRI DES ASSURANCES
           COPY 'COBOL-FD-ASSURANCE.cpy'
               REPLACING LEADING ==FD== BY ==SD== 
                         LEADING ==FASSUR== BY ==SASSUR==
                         LEADING ==RASSUR== BY ==SASSUR-REC==.  

      ******** FICHIER DE TRI DES DEPARTEMENTS
           COPY 'COBOL-FD-DEPT.cpy'
               REPLACING LEADING ==FD== BY ==SD== 
                         LEADING ==FDEPT== BY ==SDEPT==
                         LEADING ==RDEPT== BY ==SDEPT-REC==.  


           
       WORKING-STORAGE SECTION.
      ***********************************************
      * DEFINTION DES STATUS DES FICHIERS
      ***********************************************
       01  WS-STATUS-FILE-FCLIENT    PIC X(02).
           88 WS-STATUS-FILE-FCLIENT-OK  VALUE "00".
           88 WS-STATUS-FILE-FCLIENT-EOF VALUE "10".       
       01  WS-COMPTEUR-FILE-FCLIENT  PIC 9(05) VALUE ZEROES.       
       
       01  WS-STATUS-FILE-FDEPT    PIC X(02).
           88 WS-STATUS-FILE-FDEPT-OK  VALUE "00".
           88 WS-STATUS-FILE-FDEPT-EOF VALUE "10".       
       01  WS-COMPTEUR-FILE-FDEPT  PIC 9(05) VALUE ZEROES.       
       
       01  WS-STATUS-FILE-FASSUR    PIC X(02).
           88 WS-STATUS-FILE-FASSUR-OK  VALUE "00".
           88 WS-STATUS-FILE-FASSUR-EOF VALUE "10".       
       01  WS-COMPTEUR-FILE-FASSUR  PIC 9(05) VALUE ZEROES.

       01   WS-STATUS-FILE-FCLISOR PIC X(02).
           88 WS-STATUS-FILE-FCLISOR-OK  VALUE "00".
           88 WS-STATUS-FILE-FCLISOR-EOF VALUE "10".
       01  WS-COMPTEUR-FILE-FCLISOR  PIC 9(05) VALUE ZEROES.    

       01  ws-tri pic x(154) value "+++".

      *************************************
      *  TABLE DES DEPARTEMENTS 
      *************************************    
       01  WS-TABLE-DEPARTEMENT.
           05  WS-TABLE-DEPT OCCURS 120 
                       ASCENDING KEY IS WS-TABLE-DEPT-ID
                       INDEXED BY WS-INDICE.
               10  WS-TABLE-DEPT-ID PIC X(03).
               10  WS-TABLE-DEP      PIC X(23).
		       10  WS-TABLE-REGION   PIC X(27).

      **************************************
      *  ZONE DE TRAVAIL DU DEPARTEMENT
      **************************************
       01  WS-DEPT.
           05 WS-DEPT-ID PIC X(03) VALUE SPACES.
           05 WS-DEPART  PIC X(23) VALUE SPACES.
           05 WS-REGION  PIC X(27) VALUE SPACES.


      *************************************
      *  TABLE DES ASSURANCES 
      *************************************    
       01  WS-TABLE-ASSURANCE.
           05  WS-TABLE-ASSUR OCCURS 120 TIMES 
                       ASCENDING KEY IS WS-TABLE-ASSUR-ID 
                       INDEXED BY WS-INDICE2.
               10  WS-TABLE-ASSUR-ID PIC 9(08).
               10  WS-TABLE-ASSUREUR PIC X(30).
               10  WS-TABLE-SITUATION PIC X(08).

      **************************************
      *  ZONE DE TRAVAIL DE L'ASSUREUR
      **************************************
       01  WS-ASSUR.
           05 WS-ASSUR-ID  PIC 9(08) VALUE ZEROES.
           05 WS-ASSUR-NOM PIC X(30) VALUE SPACES.
           05 WS-ASSUR-SIT PIC X(08) VALUE SPACES.


      ******************************************
      *  ZONE DE TRAVAIL DU TOTAL DES SALAIRES
      ******************************************
       01  WS-TOTAL-MONTANT   PIC 9(09)V99 VALUE ZEROES.
       01  WS-EDIT-MONTANT    PIC Z.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       0000-MAIN-DEB.
      *******************************************************
      *  PARAGRAPHE PRINCIPAL
      *******************************************************
      *
      *********** CONTROLE D'EXISTENCE DU FICHIER FCLIENT    
           OPEN INPUT FCLIENT.
           IF NOT WS-STATUS-FILE-FCLIENT-OK
               DISPLAY "ERREUR OUVERTURE FICHIER 'FCLIENT': "
                   WS-STATUS-FILE-FCLIENT
           ELSE 
               CLOSE FCLIENT
           END-IF.
      *
      *********** TRAITEMENT DU FICHIER FDEPT ET CHARGEMENT EN TABLE     
           OPEN INPUT FDEPT.
           IF NOT WS-STATUS-FILE-FDEPT-OK
               DISPLAY "ERREUR OUVERTURE FICHIER 'FDEPT': "
                   WS-STATUS-FILE-FDEPT
           ELSE 
               PERFORM 6010-TRAITEMENT-FICHIER-DEPARTEMENT-DEBUT THRU   
                       6010-TRAITEMENT-FICHIER-DEPARTEMENT-FIN
           END-IF.
           
           DISPLAY "NOMBRE DE DEPARTEMENTS CHARGES EN TABLES: "
                   WS-COMPTEUR-FILE-FDEPT.
           CLOSE FDEPT.            
      *
      *********** TRAITEMENT DU FICHIER FASSUR ET CHARGEMENT EN TABLE     
           OPEN INPUT FASSUR.
           IF NOT WS-STATUS-FILE-FASSUR-OK
               DISPLAY "ERREUR OUVERTURE FICHIER 'FASSUR': "
                   WS-STATUS-FILE-FASSUR
           ELSE 
               PERFORM 6030-TRAITEMENT-FICHIER-ASSURANCE-DEBUT THRU   
                       6030-TRAITEMENT-FICHIER-ASSURANCE-FIN
           END-IF.
           
           DISPLAY "NOMBRE D'ASSUREURS CHARGES EN TABLES: "
                   WS-COMPTEUR-FILE-FASSUR.
           CLOSE FASSUR.            
      *
      *********** OUVERTURE DU FICHIER EN SORTIE EXISTANT     
           OPEN OUTPUT FCLISOR.

           IF NOT WS-STATUS-FILE-FCLISOR-OK
      *********** CREATION DU FICHIER EN SORTIE CAR INEXISTANT     
           OPEN EXTEND FCLISOR
               IF NOT WS-STATUS-FILE-FCLISOR-OK
                   DISPLAY "ERREUR OUVERTURE FICHIER 'FCLISOR': "
                       WS-STATUS-FILE-FCLISOR
               END-IF
           END-IF.
           
           IF  WS-STATUS-FILE-FCLIENT-OK 
           AND WS-STATUS-FILE-FDEPT-OK 
           AND WS-STATUS-FILE-FCLISOR-OK
      ***** LECTURE DU FICHIER CLIENT ET ECRITURE EN SORTIE
               PERFORM 6060-TRAITEMENT-FICHIER-FCLIENT-DEBUT THRU
                       6060-TRAITEMENT-FICHIER-FCLIENT-FIN
      ***** TRI INTERNE DU FICHIER FCLISOR
               PERFORM 6050-TRI-FICHIER-SCLISOR-DEBUT
                  THRU 6050-TRI-FICHIER-SCLISOR-FIN
           END-IF.

           DISPLAY "NOMBRE D'ENREGISTREMENTS CLIENTS LUS: "
                       WS-COMPTEUR-FILE-FCLIENT.
               
           CLOSE FCLISOR.
           DISPLAY "NOMBRE D'ENREGISTREMENTS COMPTE-RENDU ECRITS: "
                    WS-COMPTEUR-FILE-FCLISOR.    
      *
      ****** AFFICHAGE DU MONTANT TOTAL DES SALAIRES.
           DISPLAY "==============================================".
           MOVE WS-TOTAL-MONTANT TO WS-EDIT-MONTANT.
           DISPLAY "TOTAL MONTANT: " WS-EDIT-MONTANT.
           DISPLAY "==============================================".

       0000-MAIN-FIN.
           STOP RUN.

      *********************************************
      *  TRAITEMENT DU FICHIER DES DEPARTEMENTS 
      *********************************************
       6010-TRAITEMENT-FICHIER-DEPARTEMENT-DEBUT.
           
           DISPLAY "--> DEBUT DE TRAITEMENT FICHIER DEPARTEMENT".
      *
      ****** TRI DU FICHIER DES DEPARTEMENTS
           DISPLAY "--> DEBUT DE TRI FICHIER DEPARTEMENT"
           SORT SDEPT ON ASCENDING KEY SDEPT-REC-ID
               USING FDEPT GIVING FDEPT.

           DISPLAY "--> FIN DE TRI FICHIER DEPARTEMENT".

           CLOSE FDEPT.
           OPEN INPUT FDEPT.
      *
      ****** LECTURE FICHIER FDEPT 
           INITIALIZE WS-TABLE-DEPARTEMENT.
           SET WS-INDICE TO 1.
           PERFORM UNTIL NOT WS-STATUS-FILE-FDEPT-OK
               READ FDEPT 
                   AT END 
                   DISPLAY "--> FIN DE TRAITEMENT FICHIER DEPARTEMENT"
                   NOT AT END 
                       ADD 1 TO WS-COMPTEUR-FILE-FDEPT
                       PERFORM 6020-CHARGEMENT-TABLE-DEPT-DEBUT THRU 
                               6020-CHARGEMENT-TABLE-DEPT-FIN
           END-PERFORM.

       6010-TRAITEMENT-FICHIER-DEPARTEMENT-FIN.
           EXIT.
      *
      ****** CHARGEMENT DE LA TABLE DES DEPARTEMENTS  
       6020-CHARGEMENT-TABLE-DEPT-DEBUT.

           MOVE RDEPT-ID       TO WS-TABLE-DEPT-ID (WS-INDICE).
           MOVE RDEPT-DEPT     TO WS-TABLE-DEP (WS-INDICE).
           MOVE RDEPT-REGION   TO WS-TABLE-REGION (WS-INDICE).

           SET WS-INDICE UP BY 1.

      *     DISPLAY "RDEPT-ID: " RDEPT-ID.
      *     DISPLAY "RDEPT-DEP: " RDEPT-DEP.
      *     DISPLAY "RDEPT-REGION: " RDEPT-REGION.
           
       6020-CHARGEMENT-TABLE-DEPT-FIN.
           EXIT.




      *********************************************
      *  TRAITEMENT DU FICHIER DES ASSURANCE 
      *********************************************
       6030-TRAITEMENT-FICHIER-ASSURANCE-DEBUT.
           
           DISPLAY "--> DEBUT DE TRAITEMENT FICHIER ASSURANCE".
      *
      ****** TRI DU FICHIER DES ASSURANCES
           DISPLAY "--> DEBUT DE TRI FICHIER ASSURANCE"
           SORT SASSUR ON ASCENDING KEY SASSUR-REC-IDENTIF
               USING FASSUR GIVING FASSUR.

           DISPLAY "--> FIN DE TRI FICHIER ASSURANCE".

           CLOSE FASSUR.
           OPEN INPUT FASSUR.
      *
      ****** LECTURE FICHIER FASSUR 
           INITIALIZE WS-TABLE-ASSURANCE.
           SET WS-INDICE2 TO 1.
           PERFORM UNTIL NOT WS-STATUS-FILE-FASSUR-OK
               READ FASSUR
                   AT END 
                   DISPLAY "--> FIN DE TRAITEMENT FICHIER ASSURANCE"
                   NOT AT END 
                       ADD 1 TO WS-COMPTEUR-FILE-FASSUR
                       PERFORM 6040-CHARGEMENT-TABLE-ASSUR-DEBUT THRU 
                               6040-CHARGEMENT-TABLE-ASSUR-FIN
           END-PERFORM.

       6030-TRAITEMENT-FICHIER-ASSURANCE-FIN.
           EXIT.
      *
      ****** CHARGEMENT DE LA TABLE DES ASSURANCES  
       6040-CHARGEMENT-TABLE-ASSUR-DEBUT.

           MOVE RASSUR-IDENTIF   TO WS-TABLE-ASSUR-ID (WS-INDICE2).
           MOVE RASSUR-ASSUREUR  TO WS-TABLE-ASSUREUR (WS-INDICE2).
           MOVE RASSUR-SITUATION TO WS-TABLE-SITUATION (WS-INDICE2).

           SET WS-INDICE2 UP BY 1.

      *     DISPLAY "RDEPT-ID: " RDEPT-ID.
      *     DISPLAY "RDEPT-DEP: " RDEPT-DEP.
      *     DISPLAY "RDEPT-REGION: " RDEPT-REGION.
           
       6040-CHARGEMENT-TABLE-ASSUR-FIN.
           EXIT.




      *******************************************************
      *  TRI DU FICHIER DES CLIENTS SUR NOM DE FAMILLE
      *******************************************************
       6050-TRI-FICHIER-SCLISOR-DEBUT.
           
           DISPLAY "--> DEBUT DE TRI FICHIER CLIENT SORTIE"

           SORT SCLISOR ON ASCENDING KEY RCLISOR-ID-DEPT, RCLISOR-NOM
               USING FCLISOR GIVING FCLISOR.

           DISPLAY "--> FIN DE TRI FICHIER CLIENT SORTIE".

       6050-TRI-FICHIER-SCLISOR-FIN.
           EXIT.

      ***************************************************************
      *  TRAITEMENT DU FICHIER DES CLIENTS POUR ECRITURE EN SORTIE
      ***************************************************************
       6060-TRAITEMENT-FICHIER-FCLIENT-DEBUT.

           DISPLAY "--> DEBUT DE TRAITEMENT FICHIER CLIENTS"

           OPEN INPUT FCLIENT.
      *
      ****** LECTURE FICHIER FCLIENT 
           PERFORM UNTIL NOT WS-STATUS-FILE-FCLIENT-OK
               READ FCLIENT 
                   AT END    
                       DISPLAY "--> FIN DE TRAITEMENT FICHIER CLIENT"
                   NOT AT END 
                       ADD 1 TO WS-COMPTEUR-FILE-FCLIENT
                       PERFORM 6070-TRAITEMENT-FICHIER-SORTIE-DEBUT THRU 
                               6070-TRAITEMENT-FICHIER-SORTIE-FIN
           END-PERFORM.

           CLOSE FCLIENT.
           CLOSE FCLISOR.

       6060-TRAITEMENT-FICHIER-FCLIENT-FIN.
          EXIT.


      *******************************************************
      *  TRAITEMENT DU FICHIER EN SORTIE
      *******************************************************
       6070-TRAITEMENT-FICHIER-SORTIE-DEBUT.
      *
      ****** RECHERCHE DU DEPARTEMENT DANS LA TABLE
           INITIALIZE WS-DEPT.

           SET WS-INDICE TO 1.

           SEARCH ALL WS-TABLE-DEPT
               AT END 
                   DISPLAY 'DEPARTEMENT NON TROUVE NO: ' RCLIENT-DEPT
               WHEN WS-TABLE-DEPT-ID (WS-INDICE) = RCLIENT-DEPT
                   MOVE WS-TABLE-DEP (WS-INDICE)    TO WS-DEPART
                   MOVE WS-TABLE-REGION (WS-INDICE) TO WS-REGION
           END-SEARCH.   
      *
      ****** RECHERCHE DE L'ASSUREUR DANS LA TABLE
           INITIALIZE WS-ASSUR.

           SET WS-INDICE2 TO 1.

           SEARCH ALL WS-TABLE-ASSUR
               AT END 
                   DISPLAY 'ASSUREUR NON TROUVE NO: ' RCLIENT-ASSUR
               WHEN WS-TABLE-ASSUR-ID (WS-INDICE2) = RCLIENT-ASSUR
                   MOVE WS-TABLE-ASSUR-ID (WS-INDICE2) TO WS-ASSUR-ID
                   MOVE WS-TABLE-ASSUREUR (WS-INDICE2) 
                                                  TO WS-ASSUR-NOM
                   MOVE WS-TABLE-SITUATION (WS-INDICE2) 
                                                  TO WS-ASSUR-SIT
           END-SEARCH.               
      *
      ****** ECRITURE DU FICHIER EN SORTIE
           
           INITIALIZE RCLISOR.

           MOVE RCLIENT-ID       TO RCLISOR-ID.
           MOVE RCLIENT-NOM      TO RCLISOR-NOM.
           MOVE RCLIENT-PRENOM   TO RCLISOR-PRENOM.
           MOVE RCLIENT-POSTE    TO RCLISOR-POSTE.
           MOVE FUNCTION NUMVAL(RCLIENT-MONTANT) TO RCLISOR-MONTANT.
           MOVE WS-DEPART        TO RCLISOR-DEPT.
           MOVE WS-REGION        TO RCLISOR-DEPT-NOM.
           MOVE WS-ASSUR-ID      TO RCLISOR-CODEASSUR.
           MOVE WS-ASSUR-NOM     TO RCLISOR-NOMASSUR.
           MOVE WS-ASSUR-SIT     TO RCLISOR-SITUATION.

           WRITE RCLISOR.
           DISPLAY "WS-STATUS-FILE-FCLISOR: " WS-STATUS-FILE-FCLISOR.
           ADD 1 TO WS-COMPTEUR-FILE-FCLISOR.
      *
      ****** TOTALISER LES MONTANTS

           ADD FUNCTION NUMVAL(RCLIENT-MONTANT) TO WS-TOTAL-MONTANT.

       6070-TRAITEMENT-FICHIER-SORTIE-FIN.
           EXIT.





