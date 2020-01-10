(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space
open List
(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
    (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
   V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
   position courante pour que d'autres λmen puissent le lire.  Un
   microcode s'autodétruit au bout d'un certain nombre d'unités de
   temps mais si un microcode est laissé près d'un autre microcode
   identique, ils fusionnent en un unique microcode dont la durée de
   vie est le somme des durées de vie des deux microcodes initiaux.
   Construire un microcode demande de l'énergie au robot : chaque
   atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
   d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
   arbres de Böhm ont un nombre de branches variables. Couper une
   branche prend une unité de temps et augmente le score de 1
   point. Si on ramène cette branche au vaisseau, un second point est
   accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
   trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
   ainsi que des champs de souffrances où la vitesse de votre robot
   est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
     prochaine unité de temps. La vitesse ne peut pas être négative et
     elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)
let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, _) ->
     Printf.sprintf
       "going to %s" (String.concat " " (List.map string_of_position path))

(**

  Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

  On vous propose de structurer la mémoire comme suit:

*)
type memory = {
    known_world : World.t option;      (** Le monde connu par le robot.     *)
    graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
    objective   : objective;           (** L'objectif courant du robot.     *)
    targets     : Space.position list; (** Les points où il doit se rendre. *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
    known_world = None;
    graph       = Graph.empty;
    objective   = Initializing;
    targets     = [];
}

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize observation memory =
  let seen_world = World.world_of_observation observation in
  let known_world =
    match memory.known_world with
    | None -> seen_world
    | Some known_world -> World.extends_world known_world seen_world
  in
  if visualize then Visualizer.show ~force:true known_world;
  { memory with known_world = Some known_world }


(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

*)


(*   Garder que les arbres qui ne sont pas en cours de chooping  *)
let rec remove_micro trees micro team robot =
match micro with 
|[] -> trees 
|m::micro -> 
  remove_micro (List.filter (fun x -> (x=m.microcode_position)) trees) micro  team robot


let rec update_pos sommets =  match sommets with 
| [] -> []
| (x,y) :: l' -> let t = match List.length sommets with 
  | 1 -> [(x-.2.,y+.2.)]
  | 2 -> [(x +.2.,y +.2.)] 
  | 3-> [(x +.2.,y -.2.)]
  | 4 -> [(x-.2.,y -.2.)] 
  | _ -> [] 
  in t @ update_pos l'  
   
let rec  edge_valide  x l= match l with 
| []-> true
| y::l'-> if segment_intersects y x then  false else edge_valide x l'

let filter_edges edges segments= 
   let rec aux edges acc s  = match edges with 
   | [] -> acc
   | e::l->  if edge_valide e segments then aux l (e::acc) s
   else  aux l acc s in 
      let  rec f l =  match l with 
         |[]->[]
         |x::l' ->  match x with 
            |(a,b) -> let Distance d = dist2 a b in (a,b,d) ::f l'  
      in 
         f (aux edges  [] segments)

let make_polygones observation memory = 
   match memory.known_world with
   | None ->[]
   | Some n ->Space.polygons n.space ((=) Hell)

   let make_polygones_suffer observation memory = 
      match memory.known_world with
      | None ->[]
      | Some n ->Space.polygons n.space ((<>) Hell)

let make_segments observation memory = 
   match memory.known_world with
   | None ->[]
   | Some n -> hell_segments n 

let make_nodes observation memory = 
   let l = make_polygones observation memory in 
      let rec aux list =
         match list with 
            |[]->[]
            |x::l-> (update_pos (vertices x)) @(aux l) 
               in
               let usefull_trees=  remove_micro (World.tree_positions observation.trees) observation.messages 0 0
               in [observation.spaceship] @ [observation.position] @ (usefull_trees) @ aux l

let make_edges l= 
   let rec aux2 y list =
      match list with
      | [] ->[]
      | z::list' ->(y,z) ::(aux2 y list')  
         in
            let rec aux list = 
               match list with
                  | [] ->[]
                  | y::list' -> (aux2 y list')@(aux list')  
                  in aux l 
  
let intersectionn (p,p') (p'',p''') = 
   let a (x0,y0) (x1,y1)=(y1-.y0)/.(x1-.x0) in 
      let b (x0,y0) (x1,y1)=y0 -. (x0*.(a (x0,y0) (x1,y1))) in 
         let a1 = a p p' in let a2 = a p'' p''' in let b1 = b p p' in let b2= b p'' p''' in
            let x = (b2-.b1)/.(a1-.a2) in let y=a2*.x+.b2 
               in (x,y)


let half (x,y) (x',y') = ((x+.x')/.(2.0),(y+.y')/.(2.0))

let foo p e t =
   let pol = polygon_segments p in 
   let (a,b,d)=e in
   let rec aux l (a,b,d) =
      match l with
         | [] -> []
         | (x,y)::l' ->if segment_intersects (a,b) (x,y) 
            then (intersectionn (a,b) (x,y))::(aux l' e)
            else aux l' e  
      in let points = aux pol e 
         in match List.length points with
            |0-> e
            |1-> 
               let p =List.nth points 0 in 
                  let Distance d' = dist2 a p  in let Distance d'' = dist2 p b  in
                     let f = suffering  t b  in 
                        (a,b,d'+. d''/.f)
            |_-> 
               let p =List.nth points 0 in let p' = List.nth points 1  in 
                  let Distance d' = dist2 p p' in 
                     let f = suffering  t (half p p')  in 
                        (a,b,d -. d'+. d'/.f)
      
   let rec change_distance_edge e pols t =
      match pols with
      | []->e
      | p::pols' ->change_distance_edge (foo p e t) pols' t

   let change_distances_to_suffer observation memory edges= 
      let pols = make_polygones_suffer observation memory
      in 
            match memory.known_world with
               | None-> edges
               | Some t ->let rec aux l = match l with 
                  | [] ->[]
                  | x::l' -> (change_distance_edge x pols t)::(aux l')
               in aux edges 

   let visibility_graph observation memory = 
      let segments = make_segments observation memory in
         let nodes = make_nodes observation memory in
            let edges = make_edges nodes in
               let filtred_edges = filter_edges edges segments in
                  let suffred_edges = change_distances_to_suffer observation memory filtred_edges in
                     Graph.make nodes suffred_edges 



(**

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

*)
module Noeud = 
   struct
      type t = Graph.node 

      let compare p1 p2 =
         match (p1,p2) with 
         | (a,b) , (c,d) ->
          match  a,c with
          | v1 ,v2 when v1>v2 -> 1
          | v1 ,v2 when v1<v2 -> -1
          | v1 ,v2 -> match  b,d with
          | v1 ,v2 when v1>v2 -> 1
          | v1 ,v2 when v1<v2 -> -1
          | v1 ,v2 -> 0


                     

end;;

module FILE_PRIO = PriorityQueue.Make (Noeud) (Float)

module Liste = Map.Make(Noeud) 


let rec initialiser_dji_F file source noeuds=
match noeuds with 
|[]-> file
|l::noeuds ->let n= match l=source with
                    |true -> 0.
                    |false-> infinity 
   in initialiser_dji_F (FILE_PRIO.insert file l n)  source noeuds


let rec initialiser_dji_P_V pere noeuds=
match noeuds with
|[]->pere
|l::noeuds-> initialiser_dji_P_V (Liste.add l false pere) noeuds

let rec initialiser_dji_P pere noeuds=
match noeuds with
|[]->pere
|l::noeuds-> initialiser_dji_P (Liste.add l l pere) noeuds

let condition_dijsktra u v w liste file = not(Liste.find v liste) && ((FILE_PRIO.priority file v) > u +. w  )
let f a b = match a with 
           |(f,p,vi,va) -> match b with 
           |(u,v,w) ->     match condition_dijsktra va v w vi f with
           |true  -> ((FILE_PRIO.decrease f v (va +. w)),(Liste.add v u p ),vi,va)
           |false -> a


let rec parcours_dijsktra  file graph pere pere_v=
match (FILE_PRIO.length file) with 
|0 ->  pere
|_ ->  let point=(FILE_PRIO.get_min file) in
       let newfile=FILE_PRIO.remove_min file in
       match point with
                  |None -> pere
                  |Some (p,v)  -> 
                     let  edges= Graph.out graph v in                             
                     let  maj_visite =(Liste.add v true pere_v) in
                     match List.fold_left f (newfile,pere,maj_visite,p) edges with
                     |(file2,pred,vi,va)-> parcours_dijsktra  file2 graph pred maj_visite

                     


let shortest_path graph source target : path =
   let edges= Graph.nodes graph in 
   let file=initialiser_dji_F FILE_PRIO.empty source edges in 
   let pere_v=initialiser_dji_P_V Liste.empty edges in
   let pere=initialiser_dji_P Liste.empty edges in 
   let pere_final= (parcours_dijsktra  file graph pere pere_v )in
   let rec aux_parcours pere source target path=
   match Liste.find target pere with
   | p-> if(p=source)then( path )
      else aux_parcours pere source p (p::path)
   in [source]@aux_parcours pere_final source target [target]

(**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en train de récolter du bois, il n'y a rien à faire
   qu'attendre qu'il est fini.

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
   faites, et le recalculer si jamais ce n'est pas le cas.

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
   et le faire suivre un premier chemin.

*)
let plan visualize observation memory =
   match memory.objective with
      | Initializing ->
         let cibles = ( World.tree_positions observation.trees) @ [observation.spaceship] in
            {
               memory with
               objective = GoingTo([List.hd cibles],[observation.position]);
               targets = cibles
            }
      | GoingTo(path,path') -> 
         if edge_valide (observation.position,List.nth path 0) (make_segments observation memory) =false 
         then 
         let newpath=  shortest_path (visibility_graph observation memory)  (observation.position ) (List.hd path ) in 
             { memory with 
              objective = GoingTo ( (List.tl newpath)@path,path');
             graph = visibility_graph observation memory
         }
       else{
            memory with 
            objective = GoingTo ( path, path');
            graph = visibility_graph observation memory
          }
      | Chopping ->   
         let branc =       
               let t = tree_at observation.trees observation.position in
                  match t with
                     |None -> 0
                     |Some t -> t.branches
         in
                        if branc > 0 then  {
                           memory with
                           objective = Chopping
                        }
                        else {
                           memory with
                           objective = GoingTo([(List.nth memory.targets 1)],[]);
                           targets = List.tl memory.targets
                        }
            



(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif.

   Si l'objectif est de couper du bois, coupons du bois! Si on vient
   de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible.

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes.

*)
let get_angle a b = match a,b with
| (xa,ya),(xb,yb) -> atan2 (yb -. ya) (xb -. xa)


let next_action visualize observation memory =
   match memory.objective with
      | Chopping -> ChopTree,memory 
      | GoingTo(path,path') -> 
      let d=get_angle observation.position (List.nth path 0) in
         if (close (List.nth path 0) observation.position 1.) then
            if List.nth path 0 <> observation.spaceship  then
               if (List.nth path 0) <> (List.nth memory.targets 0)
                  then 
                  Move(Space.angle_of_float d,observation.max_speed),{
                     memory with
                     objective = GoingTo(List.tl path,path')
                  }   
               else 
               let pos =(List.nth path 0) in 
               let tr = World.tree_at observation.trees pos in 
               match tr with 
               |None ->
                   Move(Space.angle_of_float d,Space.speed_of_float 0.),{
                     memory with
                     objective = Chopping
                  } 
                 
               |Some tree -> match memory.known_world with
                            |None -> assert false 
                                 (*Move(Space.angle_of_float d,Space.speed_of_float 0.),{
                                  memory with
                                  objective = Chopping
                                 } *)
                            |Some world ->
               let branche = tree.branches in 
               let new_world= World.put world (MicroAtom(0)) (Space.duration_of_int branche) pos in
               let ()=Printf.eprintf "i'me here " in 
               Move(Space.angle_of_float d,Space.speed_of_float 0.),{
                     memory with
                     objective = Chopping;
                     
                     known_world=Some new_world 
                  }          
            else
               Die "win",memory
         else 
            Move(Space.angle_of_float d,observation.max_speed),memory

      | _ -> Wait,memory



(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =
  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in
  let () = Visualizer.show_graph memory.graph in
  next_action visualize observation memory