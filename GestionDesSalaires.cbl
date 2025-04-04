       identification division.
       program-id. GestionDesSalaires.
       
       environment division.
       configuration section.
       
       data division.
       working-storage section.

       01  Employe.
           05  Nom               PIC A(30).
           05  EmployeID                PIC 9(5).
           05  Taux-Horaire      PIC 9(3)V99. *> V99 pour afficher 2 chiffres après la virgule, gain de précision
           05  Heures-Travaillees PIC 9(3).
           05  Heures-Supp PIC 9(3).
           05  Salaire            PIC 9(7)V99.
           05  Taux-Imposition        PIC 9(3)V99 VALUE 0.2.  *> 20% d'imposition
           05  Retenue                PIC 9(7)V99.
           05  SalaireNet             PIC 9(7)V99.
           05  Erreur                 PIC A(100).
           01  Option                 PIC 9.

       
       procedure division.

       PERFORM MENU-PRINCIPAL.

       MENU-PRINCIPAL.
           DISPLAY "========================================".
           DISPLAY "          MENU PRINCIPAL                 ".
           DISPLAY "========================================".
           DISPLAY "1. Ajouter un Employe".
           DISPLAY "2. Calculer le Salaire".
           DISPLAY "3. Quitter".
           DISPLAY "Selectionnez une option : ".
           ACCEPT Option.

        EVALUATE Option
               WHEN '1'
                   PERFORM AJOUTER-EMPLOYE
               WHEN '2'
                   PERFORM CALCULER-SALAIRE
               WHEN '3'
                   DISPLAY "Au revoir !"
               WHEN OTHER
                   DISPLAY "Option invalide. Veuillez reessayer.".
                   PERFORM MENU-PRINCIPAL
           

           IF Option NOT = '3' THEN
               PERFORM MENU-PRINCIPAL
           END-IF.
            
       AJOUTER-EMPLOYE.

           DISPLAY 'Entrez le nom de l''employe : '.
           ACCEPT Nom.
           IF Nom = SPACES THEN *> gestion des erreurs
               MOVE 'Nom invalide.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez l''ID de l''employe : '.
           ACCEPT EmployeID.
           IF EmployeID < 1 OR EmployeID > 99999 THEN *> gestion des erreurs
               MOVE 'ID invalide.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez le taux horaire : '.
           ACCEPT Taux-Horaire.
           IF Taux-Horaire <= 0 THEN *> gestion des erreurs
               MOVE 'Taux horaire invalide.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez les heures travaillees : '.
           ACCEPT Heures-Travaillees.
           IF Heures-Travaillees < 0 OR Heures-Travaillees > 180 THEN *> gestion des erreurs
               MOVE 'Heures travaillees invalides.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez les heures supplementaires : '.
           ACCEPT Heures-Supp.
           IF Heures-Supp < 0 OR Heures-Supp > 20 THEN *> gestion des erreurs
               MOVE 'Heures supplementaires invalides.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

       CALCULER-SALAIRE.
           DISPLAY 'Calcul du salaire...'.
           
           DISPLAY 'Entrez le taux horaire : '.
           ACCEPT Taux-Horaire.
           IF Taux-Horaire <= 0 THEN
               MOVE 'Taux horaire invalide.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez les heures travaillees : '.
           ACCEPT Heures-Travaillees.
           IF Heures-Travaillees < 0 OR Heures-Travaillees > 180 THEN
               MOVE 'Heures travaillees invalides.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           DISPLAY 'Entrez les heures supplementaires : '.
           ACCEPT Heures-Supp.
           IF Heures-Supp < 0 OR Heures-Supp > 20 THEN
               MOVE 'Heures supplementaires invalides.' TO Erreur
               DISPLAY Erreur
               GOBACK
           END-IF.

           COMPUTE Salaire = (Heures-Travaillees * Taux-Horaire) +
                              (Heures-Supp * Taux-Horaire * 1.5).
           
           COMPUTE Retenue = Salaire * Taux-Imposition.
           COMPUTE SalaireNet = Salaire - Retenue.

           DISPLAY 'Bulletin de Paie:'.
           DISPLAY 'Nom : ' Nom.
           DISPLAY 'ID : ' EmployeID.
           DISPLAY 'Salaire Total : ' Salaire.
           DISPLAY 'Retenue Fiscale : ' Retenue.
           DISPLAY 'Salaire Net : ' SalaireNet.
        
           GOBACK.

  
       end program GestionDesSalaires.
