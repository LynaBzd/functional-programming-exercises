open Regex_base

(* prend un entier n et une liste l en entrée.
    retourne une concaténation de n répétitions de la liste l. *)
let repeat n l =
  (* fonction auxiliaire pour ajouter une liste accumulatrice en paramètre *)
  let rec aux n l l' =
    match l with 
    | [] -> l' (* cas de base, terminaison, renvoi *)
    (* tant que n > 0 on concat *)
    | _  -> if (n = 0) then l' else  aux (n-1) l (l @ l')
  in aux n l []

(* retourne une expression régulière qui reconnaît la concaténation
    de n mots reconnus par e *)
let expr_repeat n e =
  (* fonction auxiliaire pour ajouter une expression accumulatrice c en paramètre *)
  let rec aux n e c = 
    if (n = 0) then Eps (* 0 occurrences de e c'est la chaîne vide *)
    else if (n = 1) then c (* 1 occurrence de e, c'est e (ici c vaut e) *)
    else aux (n-1) e (Concat (e,c)) (* sinon on appelle n fois 
                                    en concaténant via Concat *)
  in aux n e e 

(* teste si le mot vide est reconnu par l'expression *)
let rec is_empty e =
  match e with
  | Eps          -> true
  | Base x       -> false
  | Joker        -> false
  (* appels récursifs correspondants, avec && pour renvoyer
      false si on trouve au moins 1 qui est Base x ou Joker *)
  | Concat (x,y) -> is_empty x && is_empty y 
  | Alt (x,y)    -> is_empty x && is_empty y
  | Star x       -> is_empty x

(* teste si le mot vide est reconnu par l'expression *)
let rec null e =
  match e with
  | Eps          -> true
  | Base x       -> false
  | Joker        -> false
  | Concat (x,y) -> null x && null y (* Concat force que x ET y reconnaisent le mot vide *)
  | Alt (x,y)    -> null x || null y (* on cherche si au moins x ou y reconnaissent le mot vide *)
  | Star x       -> true (* étoile autorise 0 occurrences de x donc Eps *)


(* teste si le l'angage reconnu par l'expression est fini *)
let rec is_finite e =
  match e with
  | Eps          -> true
  | Base x       -> true
  | Joker        -> true
  | Concat (x,y) -> is_finite x && is_finite y 
  | Alt (x,y)    -> is_finite x && is_finite y
  (* avec étoile vrai ssi seulement Eps est reconnu par e
     mais devient faux si l'étoile concerne au moins une Base
     ou un Joker *)
  | Star x       -> if (is_empty e) then true else false

(* produit cartésien des deux listes *)
let product l1 l2 =
  (* aux pour mettre un accumulateur *)
  let rec aux l1 l2 l' conc =
    match l1 with
      (* on accumule à l'envers donc on renvoie la liste retournée *)
      | []      -> List.rev conc
      | x :: xs ->
        match l2 with
        (* si l2 vide appel récursif avec l1 sans sa tête et l2 qui vaut l' *)
        | []      -> aux xs l' l' conc
        (* sinon appel récursif où L1 reste pareil, l2 perd sa tête
           et conc accumume les listes x et y *)
        | y :: ys -> aux l1 ys l' ((x @ y) :: conc)
  in aux l1 l2 l2 []

(* transforme une liste en une liste de listes où chaque sous-liste
   contient un seul élément de la liste de départ *)
let explode (lst : 'a list) : ('a list list) = 
  let rec aux (lst' : 'a list) (acc : 'a list list) 
      : ('a list list) =
    match lst' with
    | [] -> List.rev acc
    | x :: xs -> aux xs ([x] :: acc)
  in aux lst []

(* convertit un type option en la valeur qu'il contient.
   échoue si l'élément de départ est None *)
let opt_to_val (opt : 'a option) : 'a = 
  match opt with
  | None -> failwith "none"
  | Some x -> x

(* renvoie une liste de listes correspondant au langage reconnu par alphabet
   si ce langage est fini, None si le langage est infini
   ou si pas de langage reconnu *)
let enumerate (alphabet : char list) (e : char expr) 
    : (char list list option) =
  (* seul argument expression car c'est le seul qui est amené à être modifié
     aplhabet ne change pas donc on peut appeler celui de enumerate *)
  let rec aux e' =
    match e' with
    | Eps          -> Some [[]] (* le mot vide est reconnu par Eps *)
    | Base x       -> (
      if (List.mem x alphabet) then
        Some ([x] :: []) (* le caractère x si dans alphabet est reconnu par Base x *)
      else 
        Some [[]] (* sinon le mot vide *)
    )
    | Joker        -> Some (explode alphabet) (* reconnaît tout l'alphabet *)
    | Concat (x,y) -> 
      (* appels récursifs gauche et droite en parallèle *)
      let left = aux x in
      let rght = aux y in
      (* si aucun des deux ne renvoie None on renvoie le produit sinon None *)
      if (left <> None && rght <> None) then
        Some (product (opt_to_val left) (opt_to_val rght))
      else None
    | Alt (x,y)    ->
      let left = aux x in
      let rght = aux y in
      (* même principe que Concat mais appliqué avec l'un ou l'autre ou les deux
         doivent ne être pas None*)
      if (left = None && rght = None) then 
        None
      else if (left <> None) then
        Some (opt_to_val left)
      else if (rght <> None) then
        Some (opt_to_val rght)
      else 
        (* aussi on concat au lieu de faire le produit *)
        Some ((opt_to_val left) @ (opt_to_val rght))
        (* si Star rend le langage infini -> None *)
    | Star x       -> if (is_empty e) then aux x else None
  in aux e

(* renvoie l'ensemble des lettres uniques apparaissant dans e *)
let alphabet_expr e =
  (* fonction auxiliaire pour ajouter une liste accumulatrice en paramètre *)
  let rec aux e l = 
    match e with
    | Eps          -> List.rev l
    | Base x       -> 
      (* ajout de x si pas déjà présent *)
      if (List.mem x l) then l else x :: l
    | Joker        -> List.rev l
    | Concat (x,y) -> aux x (aux y l) (* appel en cascade *)
    | Alt (x,y)    -> aux x (aux y l) (* appel en cascade *)
    | Star x       -> aux x l
  in aux e []

type answer =
  Infinite | Accept | Reject

(* renvoie Accept si le langage reconnu par e contient le mot w
   sinon renvoie Reject
   mais renvoie Infinite si ce langage est infini *)
let rec accept_partial (e : char expr) (w : char list) 
: (answer) =
  match e with
  | Eps          -> if (w = []) then Accept else Reject
  | Base x       ->  
    if (w = [x]) then Accept else Reject
  | Joker        -> 
    if (List.length w = 1) then Accept else Reject
  | Concat (x,y) -> (
    match w with
    (* si w est Eps on accepte si le langage peut reconnaitre Eps *)
    | [] -> if (null x && null y) then Accept else Reject
    | v :: vs -> 
      (* on teste si c'est accepté pour les deux parties à la fois si oui Accept sinon Reject *)
      if (accept_partial x [v] = Accept && accept_partial y vs = Accept) then Accept else Reject
    )
  | Alt (x,y)    -> 
    (* on test si au moins un des deux entre gauche et droite est accepté
       si oui Accept sinon Reject *)
    if (accept_partial x w = Accept || accept_partial y w = Accept) then Accept else Reject
  | Star x       -> Infinite
