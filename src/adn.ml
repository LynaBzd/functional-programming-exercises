type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* transforme une string en une liste de char (pas de séparateur) *)
let explode (str : string) : char list =
  if str = "" then []
  else let rec aux lst idx = 
    if idx = -1 then lst (* case de base *)
    else aux (str.[idx]::lst) (idx-1) (* récursion terminale *)
  in aux [] (String.length str -1) (* 1er appel *)

(* fait correspondre des char à une base, avec par défaut WC *)
let base_of_char (c : char) : base =
  match c with
  | 'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _   -> WC

(* convertit une string en une liste de bases *)
let dna_of_string (s : string) : base list = 
  (* on utilise les fonctions écrites plus haut *)
  List.map (base_of_char) (explode s) 

(* convertit une liste de bases en une string *)
let rec string_of_dna (seq : dna) : string =
  let rec aux res lst =
    match lst with
    | []      -> res (* case de base *)
    (* on appelle en concat et en enlevant la tête de liste *)
    | x :: xs -> aux (res ^ (string_of_base x)) xs
  in aux "" seq



(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match slice with
  | [] -> Some (list) (* case de base *)
  | x :: xs -> 
    match list with 
    | []-> None (* si la liste est finie mais pas slices *)
    | y :: ys -> 
      (* si on rencontre une différence entre slice et list à un index donné *)
      if (x <> List.hd list) 
        then None 
      else 
        cut_prefix xs ys


(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let rec first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  let rec aux prefix suffix =
    if (prefix = suffix) then
      (* si les listes sont pareilles à un moment 
         dans la récursion, c'est qu'on coupe tout *)
      Some ([], [])
    else match suffix with
    | [] -> None (* si suffix est vide avant prefix *)
    | x :: xs ->
      match cut_prefix slice suffix with (* on essaie d'enlever le prefix *)
      (* si on trouve on renvoie le prefix construit à gauche (à l'envers)
         et le le suffixe obtenu avec cut_prefix à droite *)
      | Some cut -> Some ((List.rev prefix), cut)
      (* sinon on construit le prefix et on réduit la liste *)
      | None -> aux (x :: prefix) xs
  in aux [] list
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let slices_between
    (start : 'a list) (stop : 'a list) (list : 'a list) 
    : 'a list list =
  let rec aux res l = 
  match l with
  | []      -> List.rev res (* renvoie la liste de listes
     dans l'odre inverse (uniquement pour les listes internes) *)
  | _ :: l' -> (* s'il teste des éléments dans la liste *)
    (* on garde que ce qu'il y a après le start du l courant *)
    match cut_prefix start l with
    | None   -> aux res l' (* si on en trouve pas on rappelle avec l' *)
    | Some k -> (* sinon on cherche avec first_occ une correspondance *)
      match first_occ stop k with
      | None     -> List.rev res (* si pas de correspondance on renvoie la liste *)
      | Some som -> aux ((fst som) :: res) (snd som) (* sinon on accumule *)
  in aux [] list
            

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  let start = dna_of_string "ATG"  
  and stop  = dna_of_string "TAA" in 
  match dna with
    | [] -> []
    | _::l' -> (slices_between start stop dna)

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  (* tab est une liste des couples (élément, occurrences) *)
  let rec aux (tab : ('a * int) list) (max : int ) (lst :'a list) 
      : 'a consensus =
    match lst with
    | []      -> (
      (* une fois que la liste est vide *)
      match tab with
      | []  -> No_consensus (* si le tableau est vide pas de consensus *)
      | [x] -> Full (fst x) (* 1 seul élément donne le consensus total *)
      | _   ->  (* + d'éléments -> il faut comparer les occurrences pour savoir
                   si c'est partial ou no_consensus *)
        (* filtre : garde les couples aux occurrences = au max 
           trouvé à partir de la récursion *)
        let sortie = List.filter (fun tuple -> (snd tuple) = max) tab in 
        (* si 1 seul couple trouvé on le renvoie avec partial *)
        if (List.length sortie = 1) then 
          let part = List.hd sortie in 
          Partial (fst part, snd part )
        (* s'il y a + d'un couple pas de consensus*)
        else 
          No_consensus
    )
    (* analyse de la liste : on compte les occurrences de chaque élément 
       et on les met à jour dans tab, jusqu'à a ce que la liste soit vide *)
    | x :: _  -> 
      (* longueur de la liste si on retire tous les éléments = x *)
      let rest = List.filter (fun e -> e <> x ) lst in
      (* le nombre d'occurrences de x est la différence
         entre la taille de la liste et sa taille sans les x *)
      let occ  = (List.length lst ) - (List.length rest) in
      (* mise à jour du max si x est l'élément actuel le plus fréquent *)
      let max' = if occ > max then occ else max in
      (* appel récursif avec les arguments mis à jour *)
      aux ((x, occ) :: tab) max' rest
  in aux [] 0 list


(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let consensus_sequence (ll : 'a list list) : 'a consensus list =
  (* sépare le premier élément de chaque sous-liste ey renvoie 
     un couple (tête des sous-liste, reste des sous-listes) *)
  let rec hd_all (ll : 'a list list) (left : 'a list) (rght : 'a list list)
      : (('a list) * ('a list list)) =
    match ll with
    (* pour garantir la récursion terminale et la simplicité de l'ajout,
       les listes sont construites à l'envers donc on les renvoie retournées *)
    | []       -> (List.rev left, List.rev rght)
    | [] :: ls -> hd_all ls left rght (* ignore les sous-listes vides *)
    | (hd :: tl) :: ls -> (* ajout du 1er élément de la sous liste à gauche,
                             et le reste à droite *)
      hd_all ls (hd :: left) (tl :: rght)
  (* applique consensus sur chaque liste des premiers éléments
     des sous-listes de départ *)
  in let rec aux (lla : 'a list list) (acc' : 'a consensus list) 
      : 'a consensus list =
    match lla with
    (* les 2 premiers cas garantissent un match with exhaustif *)
    | []       -> List.rev acc'
    | [] :: ls -> List.rev acc'
    | _        ->  (* récup des têtes de sous listes isolées *)
      let t = hd_all lla [] [] in
      (* appel récursif avec la liste sans ses têtes actuelles
         et avec application de consensus sur ces têtes
         pour les ajouter au résultat à renvoyer *)
      aux (snd t) ((consensus (fst t)) :: acc')
  in aux ll []


(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
