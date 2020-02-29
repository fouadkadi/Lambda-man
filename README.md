# Lambda-man

L'enoncé du projet est le fichier **lambda-man.pdf** ,il contient des indications sur la maniére de lancer le serveur. 

## Tache 1 
   Dans cette partie on programme le comportement du robot dans un monde simple sans obstacles ,c-a-d définir les targets et chopper les arbes un à un puis revenir au vaisseau. 
## Tache 2
   Dans cette partie on rajoute l'obstacle de ne pas franchir un **polygone de l'enfer** 
   et de choisir le plus court chemin entre la position du rebot et le prochain target ,d'ou l'implementation de l'algorithme de **Dijkstra**.
## Tache 3 
   Dans cette partie on prend en considération le *niveau de souffrance* à une position donnée , l'idée est de *pondérer* le chemin par le ratio distance et niveau de souffrance  pour pouvoir choisir le plus court chemin.  
## Tache 4 
   Dans cette partie on répartit le travail entre **plusieurs robots** pour allez plus vite , chaque rebot selon son **identifant** se voie attribuer une liste de targets en phase d'initialisation pour qu'il s'en occupe.
## Tache 5 
   Dans cette tache on a modifier le module worldGenerator pour etre plus libre dans la création de monde aléatoire en ajoutant quelques options à la commande : 

   ***
   > ./lambda generate -v -p 1 -r 5 -h 15 -g 25 -t 45 --mb 15 --ms 2.0 
   ***  

   L'option **-v** : permet de visualiser le monde crée  

   L'option **-p** : définit le nombre d'equipes dans le monde  

   L'option **-r** : définit le nombre de rebot par équipe  

   L'option **-h** : définit exactement le nombre de polygones de l'enfer  

   L'option **-g** : définit exactement le nombre de polygones de souffrance  

   L'option **-t** : définit exactement le nombre d'arbres dans le monde   

   L'option **--mb** : définit le maximum de branches pour chaque arbre   

   L'option **--ms** : définit le coefficient maximal de souffrance   

    
       
         

     
   A l'aide de worldGenerator on a pu generer quelques mondes plus complexe que ceux des testes ,ces dérniers ce trouve dans le repertoire **myworld** qu'on peu executer avec le serveur.  

   Chaque nom de monde définit le nombre d'équipes et de rebots necessaire.