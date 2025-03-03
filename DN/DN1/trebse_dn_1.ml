(*----------------------------------------------------------------------------*
 # 1. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Ogrevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Števke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `stevke : int -> int -> int list`, ki sprejme pozitivni celi
 števili $b$ in $n$ ter vrne seznam števk števila $n$ v bazi $b$. Pri tem tudi
 za baze, ki so večje od $10$, uporabimo števke od $0$ do $b - 1$.
[*----------------------------------------------------------------------------*)

let rec stevke b n = 
  if n < b then [n]
  else stevke b (n / b) @ [n mod b]

let primer_1_1 = stevke 10 12345
(* val primer_1_1 : int list = [1; 2; 3; 4; 5] *)

let primer_1_2 = stevke 2 42
(* val primer_1_2 : int list = [1; 0; 1; 0; 1; 0] *)

let primer_1_3 = stevke 16 (3 * 16 * 16 * 16 + 14 * 16 * 16 + 15 * 16 + 9)
(* val primer_1_3 : int list = [3; 14; 15; 9] *)

(*----------------------------------------------------------------------------*
 ### Začetek seznama
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `take : int -> 'a list -> 'a list`, ki sprejme naravno
 število in vrne ustrezno število elementov z začetka danega seznama. Če je
 podani seznam krajši od zahtevane dolžine, naj funkcija vrne kar celoten
 seznam.
[*----------------------------------------------------------------------------*)

let rec take n sez =
  if n = 0 then [] else
  match sez with
  | [] -> []
  | t :: tsez -> t :: take (n-1) tsez
let primer_1_4 = take 3 [1; 2; 3; 4; 5]
(* val primer_1_4 : int list = [1; 2; 3] *)

let primer_1_5 = take 10 [1; 2; 3; 4; 5]
(* val primer_1_5 : int list = [1; 2; 3; 4; 5] *)

(*----------------------------------------------------------------------------*
 ### Odstranjevanje ujemajočih
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `drop_while : ('a -> bool) -> 'a list -> 'a list`, ki z
 začetka seznama odstrani vse elemente, ki zadoščajo danemu predikatu. Ko najde
 element, ki predikatu ne zadošča, vrne preostanek seznama.
[*----------------------------------------------------------------------------*)

let rec drop_while predikat seznam =
  match seznam with
  | [] -> []
  | [x] -> if predikat x then [] else [x] 
  | t :: tsez -> if predikat t then drop_while predikat tsez else [t] @ tsez

let primer_1_6 = drop_while (fun x -> x < 5) [3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5]
(* val primer_1_6 : int list = [5; 9; 2; 6; 5; 3; 5] *)

let primer_1_7 = drop_while (fun x -> x < 5) [9; 8; 7; 6; 5; 4; 3; 2; 1; 0]
(* val primer_1_7 : int list = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0] *)

(*----------------------------------------------------------------------------*
 ### Funkcija `filter_mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `filter_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b
 list`, ki deluje tako kot `List.filter_map`, le da funkcija poleg elemenov dobi
 še njihove indekse.
[*----------------------------------------------------------------------------*)

(*Glavna ideja je, da se sprehajamo po elementih seznama z uporabo match funkcije, potem pa preverimo ali je Some ali None pri funkciji za filtriranje*)
(*Implementacija z pomozno funkcijo je pravzaprav samo neka made-at-home oblika python for loopa?*)
let filter_mapi funkcija sez =
  let rec pomozna i sez =
    match sez with
    | [] -> []
    | x :: xs -> (* !!! puščica !!! *)
        match (funkcija i x) with
        | Some t -> t :: pomozna (i + 1) xs
        | None -> pomozna (i + 1) xs
  in
  pomozna 0 sez

let primer_1_8 =
  filter_mapi
    (fun i x -> if i mod 2 = 0 then Some (x * x) else None)
    [1; 2; 3; 4; 5; 6; 7; 8; 9]
(* val primer_1_8 : int list = [1; 9; 25; 49; 81] *)

(*----------------------------------------------------------------------------*
 ## Izomorfizmi množic
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Na predavanjih smo videli, da funkciji `curry : ('a * 'b -> 'c) -> ('a -> ('b
 -> 'c))` in `uncurry : ('a -> ('b -> 'c)) -> ('a * 'b -> 'c)` predstavljata
 izomorfizem množic $C^{A \times B} \cong (C^B)^A$, če kartezični produkt
 predstavimo s produktnim, eksponent pa s funkcijskim tipom.

 Podobno velja tudi za ostale znane izomorfizme, če disjunktno unijo
   $$A + B = \{ \mathrm{in}_1(a) \mid a \in A \} \cup \{ \mathrm{in}_2(b) \mid b
 \in B \}$$
 predstavimo s tipom `('a, 'b) sum`, definiranim z:
[*----------------------------------------------------------------------------*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

(*----------------------------------------------------------------------------*
 Napišite pare funkcij `phi1` & `psi1`, …, `phi7` & `psi7`, ki predstavljajo
 spodnje izomorfizme množic. Tega, da so si funkcije inverzne, ni treba
 dokazovati.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### $A \times B \cong B \times A$
[*----------------------------------------------------------------------------*)

let phi1 (a, b) = (b, a)
let psi1 (b, a) = (a, b)

(*----------------------------------------------------------------------------*
 ### $A + B \cong B + A$
[*----------------------------------------------------------------------------*)

let phi2 = 
  function
  | In1 a -> In2 a
  | In2 b -> In1 b
let psi2 = (fun x -> phi2 x)

(*----------------------------------------------------------------------------*
 ### $A \times (B \times C) \cong (A \times B) \times C$
[*----------------------------------------------------------------------------*)

let phi3 (a, (b, c)) = ((a, b), c)
let psi3 ((a, b), c)= (a, (b, c))

(*----------------------------------------------------------------------------*
 ### $A + (B + C) \cong (A + B) + C$
[*----------------------------------------------------------------------------*)

let phi4 =
  function
  | In1 a -> In1 (In1 a)
  | In2 (In1 a) -> In1 (In2 a)
  | In2 (In2 a) -> In2 a
let psi4 =
  function
  | In1 (In1 a) -> In1 a
  | In1 (In2 a) -> In2 (In1 a)
  | In2 a -> In2 (In2 a)

(*----------------------------------------------------------------------------*
 ### $A \times (B + C) \cong (A \times B) + (A \times C)$
[*----------------------------------------------------------------------------*)

let phi5 (a, b) =
  (*Ne vem kako bi uporabil function tako, da bi opazovala le snd arg*)
  match b with
  | In1 c -> In1 (a,c)
  | In2 c -> In2 (a,c)
let psi5 =
  function
  | In1 (a,b) -> (a, In1 b)
  | In2 (a,b) -> (a, In2 b) 

(*----------------------------------------------------------------------------*
 ### $A^{B + C} \cong A^B \times A^C$
[*----------------------------------------------------------------------------*)

let phi6 f = ((fun a -> f (In1 a)), (fun a -> f (In2 a)))
let psi6 (f, g) = 
  function
  | In1 a -> f a
  | In2 a -> g a

(*----------------------------------------------------------------------------*
 ### $(A \times B)^C \cong A^C \times B^C$
[*----------------------------------------------------------------------------*)

let phi7 f = ((fun a -> fst (f a)), (fun a -> snd (f a)))
let psi7 (f, g) = fun a -> (f a, g a)

(*----------------------------------------------------------------------------*
 ## Polinomi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Polinome $a_0 + a_1 x + \cdots + a_n x^n$ predstavimo s seznami celoštevilskih
 koeficientov od prostega do vodilnega člena. Na primer, polinom $1 - 2 x + 3
 x^2$ predstavimo s seznamom `[1; -2; 3]`.
[*----------------------------------------------------------------------------*)

type polinom = int list

(*----------------------------------------------------------------------------*
 ### Odstranjevanje odvečnih ničel
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `pocisti : polinom -> polinom`, ki s konca seznama
 koeficientov odstrani odvečne ničle.
[*----------------------------------------------------------------------------*)

let rec pocisti polinom =
  match List.rev polinom with
  | [] -> []
  | 0 :: nekaj -> pocisti (List.rev nekaj)
  | nekaj -> List.rev nekaj

let primer_3_1 = pocisti [1; -2; 3; 0; 0]
(* val primer_3_1 : int list = [1; -2; 3] *)

(*----------------------------------------------------------------------------*
 ### Seštevanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( +++ ) : polinom -> polinom -> polinom`, ki sešteje dva
 polinoma.
[*----------------------------------------------------------------------------*)

let rec ( +++ ) p1 p2 =
  match (p1, p2) with
  | ([], []) -> []
  | (t :: ts, v :: tv) -> (t + v) :: pocisti (( +++ ) ts tv)
  | (t :: ts, []) -> t :: ( +++ ) ts []
  | ([], v :: tv) -> v :: ( +++ ) [] tv

let primer_3_2 = [1; -2; 3] +++ [1; 2]
(* val primer_3_2 : int list = [2; 0; 3] *)

let primer_3_3 = [1; -2; 3] +++ [1; 2; -3]
(* val primer_3_3 : int list = [2] *)

(*----------------------------------------------------------------------------*
 ### Množenje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( *** ) : polinom -> polinom -> polinom`, ki zmnoži dva
 polinoma.
[*----------------------------------------------------------------------------*)

(*Self-explanatory*)
let rec monomsko koeficient potenca polinom =
  match polinom with
  | [] -> []
  | t :: ts -> if potenca > 0 then 0 :: monomsko koeficient (potenca - 1) polinom else (koeficient * t) :: monomsko koeficient 0 ts

let ( *** ) p1 p2 = 
  let rec pomozna p1 akumulator potenca = 
    match p1 with
    | [] -> akumulator
    | prosti_koeficient :: ostalo -> 
      let produkt_z_monomom = monomsko prosti_koeficient potenca p2 in
      let novo = ( +++ ) akumulator produkt_z_monomom in 
      pomozna ostalo novo (potenca + 1) in
  pomozna p1 [] 0

let primer_3_4 = [1; 1] *** [1; 1] *** [1; 1]
(* val primer_3_4 : int list = [1; 3; 3; 1] *)

let primer_3_5 = [1; 1] *** [1; -1]
(* val primer_3_5 : int list = [1; 0; -1] *)

(*----------------------------------------------------------------------------*
 ### Izračun vrednosti v točki
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `vrednost : polinom -> int -> int`, ki izračuna vrednost
 polinoma v danem argumentu.
[*----------------------------------------------------------------------------*)

let vrednost polinom vrednost = List.fold_right(fun koeficient akumulator -> (akumulator * vrednost) + koeficient ) polinom 0

(*
let vrednost2 polinom vrednost =
  match vrednost with
  | `Int x -> List.fold_right(fun koeficient akumulator -> (akumulator * x) + koeficient ) polinom 0
  | `Float x -> List.fold_right(fun koeficient akumulator -> (akumulator *. x) +. float_of_int (koeficient)) polinom 0.0 *)

let primer_3_6 = vrednost [1; -2; 3] 2
(* val primer_3_6 : int = 9 *)

(*----------------------------------------------------------------------------*
 ### Odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odvod : polinom -> polinom`, ki izračuna odvod polinoma.
[*----------------------------------------------------------------------------*)

let odvod polinom =
  let rec konstruktor poli n =
    match poli with
    | [] -> []
    | t :: ts when n = 0 -> konstruktor ts (n+1)
    | v :: vs -> (v * n) :: konstruktor vs (n+1)
  in
  konstruktor polinom 0

let primer_3_7 = odvod [1; -2; 3]
(* val primer_3_7 : int list = [-2; 6] *)

(*----------------------------------------------------------------------------*
 ### Lep izpis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `izpis : polinom -> string`, ki polinom lepo izpiše. Na
 primer, `izpis [1; -2; 3]` vrne `"3 x^2 - 2 x + 1"` oziroma še bolje kot `"3 x²
 - 2 x + 1"`. Pozorni bodite, da izpis začnete z vodilnim členom.
[*----------------------------------------------------------------------------*)

let superscript n =
  let superscripts = [|"⁰"; "¹"; "²"; "³"; "⁴"; "⁵"; "⁶"; "⁷"; "⁸"; "⁹"|] in
  let rec stevke_v_super n =
    if n = 0 then "⁰"
    else
      let rec pomozna akumulator n =
        if n = 0 then akumulator
        else
          let stevka = n mod 10 in
          pomozna (superscripts.(stevka) ^ akumulator) (n / 10)
      in
      pomozna "" n
  in
  (*Če je potenca enaka 1 pa nočemo superscripta zato:*)
  if n = 1 then ""
  else stevke_v_super n

let izpis poli =
  let obrnjeno = List.rev poli in
  let rec formatiraj_clen koef potenca je_vodilni =
    if koef = 0 then ""
    else
      let koef_niz =
        match (koef, potenca) with
        | (1, 0) -> if je_vodilni then "1" else " + 1"
        | (-1, 0) -> if je_vodilni then "-1" else " - 1"
        | (_, 0) -> (if je_vodilni then "" else if koef > 0 then " + " else " - ") ^ string_of_int (abs koef)
        | (1, 1) -> if je_vodilni then "x" else " + x"
        | (-1, 1) -> if je_vodilni then "-x" else " - x"
        | (1, _) -> if je_vodilni then "x" ^ superscript potenca else " + x" ^ superscript potenca
        | (-1, _) -> if je_vodilni then "-x" ^ superscript potenca else " - x" ^ superscript potenca
        | (_, 1) -> (if je_vodilni then "" else if koef > 0 then " + " else " - ") ^ string_of_int (abs koef) ^ " x"
        | _ -> (if je_vodilni then "" else if koef > 0 then " + " else " - ") ^ string_of_int (abs koef) ^ " x" ^ superscript potenca
      in
      koef_niz
  in
  (*i=0 pove ali je clen vodilni*)
  let cleni = List.mapi (fun i coeff -> formatiraj_clen coeff (List.length poli - i - 1) (i = 0)) obrnjeno in
  let formatiran_polinom = String.concat "" cleni in
  formatiran_polinom


let primer_3_8 = izpis [1; 2; 1]
(* val primer_3_8 : string = "x² + 2 x + 1" *)

let primer_3_9 = izpis [1; 0; -1; 0; 1; 0; -1; 0; 1; 0; -1; 0; 1]
(* val primer_3_9 : string = "x¹² - x¹⁰ + x⁸ - x⁶ + x⁴ - x² + 1" *)

let primer_3_10 = izpis [0; -3; 3; -1]
(* val primer_3_10 : string = "-x³ + 3 x² - 3 x" *)

(*----------------------------------------------------------------------------*
 ## Samodejno odvajanje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ob razmahu strojnega učenja, ki optimalno rešitev išče s pomočjo gradientnega
 spusta, si želimo čim bolj enostavno računati odvode. Odvod funkcije $f$ v
 točki $x_0$ lahko seveda ocenimo tako, da v

 $$\frac{f (x_0 + h) - f(x_0)}{h}$$

 vstavimo dovolj majhno število $h$.
[*----------------------------------------------------------------------------*)

let priblizek_odvoda f x0 h =
  (f (x0 +. h) -. f x0) /. h
(* val priblizek_odvoda : (float -> float) -> float -> float -> float = <fun> *)

let primer_4_1 =
  let f x = sin x +. cos x +. exp x in
  List.map (priblizek_odvoda f 1.) [0.1; 0.01; 0.001; 0.0001; 0.00001]
(* val primer_4_1 : float list =
  [2.48914386298364931; 2.42384618742050861; 2.41778190719976749;
   2.41717997997881184; 2.41711983210990411] *)

(*----------------------------------------------------------------------------*
 Pri samodejnem odvajanju izkoristimo to, da poznamo odvode elementarnih
 funkcij, odvode sestavljenih funkcij pa lahko izračunamo iz posameznih odvodov.
 Tako bomo vsako funkcijo predstavili s parom: prvotno funkcijo in njenim
 odvodom.
[*----------------------------------------------------------------------------*)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, (fun x -> -. sin x))
let eksp : odvedljiva = (exp, exp)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
  (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
  fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))
(* val sinus : odvedljiva = (<fun>, <fun>) *)
(* val kosinus : odvedljiva = (<fun>, <fun>) *)
(* val eksp : odvedljiva = (<fun>, <fun>) *)
(* val ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva = <fun> *)

let primer_4_2 =
  let (_, f') = sinus ++. kosinus ++. eksp in
  f' 1.
(* val primer_4_2 : float = 2.41711314951928813 *)

(*----------------------------------------------------------------------------*
 ### Vrednost odvoda
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `vrednost : odvedljiva -> float -> float` in `odvod :
 odvedljiva -> float -> float`, ki izračunata vrednost funkcije in njenega
 odvoda v danem argumentu.
[*----------------------------------------------------------------------------*)

let vrednost (f, f') x = f x
let odvod (f, f') x = f' x

(*----------------------------------------------------------------------------*
 ### Osnovne funkcije
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `konstanta : float -> odvedljiva` in `identiteta :
 odvedljiva`, ki predstavljata konstantno in identično funkcijo.
[*----------------------------------------------------------------------------*)

let konstanta c = ((fun x -> c), (fun x -> 0.))
let identiteta = ((fun x -> x), (fun x -> 1.))

(*----------------------------------------------------------------------------*
 ### Produkt in kvocient
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkciji `( **. ) : odvedljiva -> odvedljiva -> odvedljiva` in `( //.
 ) : odvedljiva -> odvedljiva -> odvedljiva`, ki predstavljata produkt in
 kvocient dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( **. ) (f, f') (g, g') = ((fun x -> (f x) *. (g x)),(fun x -> ((f x) *. (g' x)) +. ((f' x) *. (g x))))

let ( //. ) (f, f') (g, g') = ((fun x -> (f x) /. (g x)), (fun x -> ((f' x) *. (g x) -. (f x) *. (g' x) ) /. ((g x) *. (g x))))

let kvadrat = identiteta **. identiteta
(* val kvadrat : odvedljiva = (<fun>, <fun>) *)

(*----------------------------------------------------------------------------*
 ### Kompozitum
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `( @@. ) : odvedljiva -> odvedljiva -> odvedljiva`, ki
 predstavlja kompozitum dveh odvedljivih funkcij.
[*----------------------------------------------------------------------------*)

let ( @@. ) (f, f') (g, g') = ((fun x -> (f (g x))), (fun x -> ((f' (g x)) *. (g' x))))

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi nima tipa *)
let vedno_ena = (kvadrat @@. sinus) ++. (kvadrat @@. kosinus)
(* val vedno_ena : odvedljiva = (<fun>, <fun>) *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
let primer_4_3 = vrednost vedno_ena 12345.
(* val primer_4_3 : float = 0.999999999999999889 *)

(* POZOR: Primer je zaenkrat zakomentiran, saj brez vedno_ena ne deluje *)
let primer_4_4 = odvod vedno_ena 12345.
(* val primer_4_4 : float = 0. *)

(*----------------------------------------------------------------------------*
 ## Substitucijska šifra
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Substitucijska šifra je preprosta šifra, pri kateri črke abecede med seboj
 permutiramo. Na primer, če bi (angleško) abecedo permutirali kot

 ```
 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
 T H E Q U I C K B R W N F X J M P S O V L A Z Y D G
 ```

 bi besedo `HELLO` šifrirali kot `KUNNJ`. Ključe, s katerimi šifriramo besedila
 bomo predstavili kar z nizi črk, v katere se slikajo črke abecede.
[*----------------------------------------------------------------------------*)

let abeceda = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"
(* val quick_brown_fox : string = "THEQUICKBRWNFXJMPSOVLAZYDG" *)
(* val rot13 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

(*----------------------------------------------------------------------------*
 Včasih bomo v primerih uporabljali tudi krajše ključe, a vedno lahko
 predpostavite, da bodo ključi permutacije začetnega dela abecede. Prav tako si
 pri delu lahko pomagate s funkcijama `indeks` in `crka`:
[*----------------------------------------------------------------------------*)

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A')
(* val indeks : char -> int = <fun> *)
(* val crka : int -> char = <fun> *)

(*----------------------------------------------------------------------------*
 ### Šifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `sifriraj : string -> string -> string`, ki besedilo šifrira
 z danim ključem. Vse znake, ki niso velike tiskane črke, pustimo pri miru.
[*----------------------------------------------------------------------------*)

let sifriraj permutacija niz =
  String.map (fun chr -> 
    let i = indeks chr in
    if i < 0 || i > 25 then
      chr
    else
      let vmesno = String.sub permutacija i 1 in
      vmesno.[0]
    ) niz

let primer_5_1 = sifriraj quick_brown_fox "HELLO, WORLD!"
(* val primer_5_1 : string = "KUNNJ, ZJSNQ!" *)

let primer_5_2 = "VENI, VIDI, VICI" |> sifriraj rot13
(* val primer_5_2 : string = "IRAV, IVQV, IVPV" *)

let primer_5_3 = "VENI, VIDI, VICI" |> sifriraj rot13 |> sifriraj rot13
(* val primer_5_3 : string = "VENI, VIDI, VICI" *)

(*----------------------------------------------------------------------------*
 ### Inverzni ključ
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `inverz : string -> string`, ki iz ključa izračuna njegov
 inverz.
[*----------------------------------------------------------------------------*)

let inverz permutacija =
  String.map (fun chr ->
    let lokacija = String.index_opt permutacija chr in
    match lokacija with
    | Some i -> String.get abeceda i
    | None -> chr
  ) (String.sub abeceda 0 (String.length permutacija))
  
let primer_5_4 = inverz quick_brown_fox
(* val primer_5_4 : string = "VIGYCMZBFOHUPLSQDJRAETKNXW" *)

let primer_5_5 = inverz rot13
(* val primer_5_5 : string = "NOPQRSTUVWXYZABCDEFGHIJKLM" *)

let primer_5_6 = inverz "BCDEA"
(* val primer_5_6 : string = "EABCD" *)

(*----------------------------------------------------------------------------*
 ### Ugibanje ključa
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Včasih seveda ne poznamo ključa, a vemo, da je besedilo v angleščini. Tako
 lahko ključ poskusimo uganiti tako, da šifrirane besede paroma primerjamo z
 besedami iz slovarja, ki smo si ga sposodili [s
 spleta](https://gist.github.com/deekayen/4148741).
[*----------------------------------------------------------------------------*)

let besede = "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see him two has look more day could go come did number sound no most people my over know water than call first who may down side been now find any new work part take get place made live where after back little only round man year came show every good me give our under name very through just form sentence great think say help low line differ turn cause much mean before move right boy old too same tell does set three want air well also play small end put home read hand port large spell add even land here must big high such follow act why ask men change went light kind off need house picture try us again animal point mother world near build self earth father head stand own page should country found answer school grow study still learn plant cover food sun four between state keep eye never last let thought city tree cross farm hard start might story saw far sea draw left late run don't while press close night real life few north open seem together next white children begin got walk example ease paper group always music those both mark often letter until mile river car feet care second book carry took science eat room friend began idea fish mountain stop once base hear horse cut sure watch color face wood main enough plain girl usual young ready above ever red list though feel talk bird soon body dog family direct pose leave song measure door product black short numeral class wind question happen complete ship area half rock order fire south problem piece told knew pass since top whole king space heard best hour better true . during hundred five remember step early hold west ground interest reach fast verb sing listen six table travel less morning ten simple several vowel toward war lay against pattern slow center love person money serve appear road map rain rule govern pull cold notice voice unit power town fine certain fly fall lead cry dark machine note wait plan figure star box noun field rest correct able pound done beauty drive stood contain front teach week final gave green oh quick develop ocean warm free minute strong special mind behind clear tail produce fact street inch multiply nothing course stay wheel full force blue object decide surface deep moon island foot system busy test record boat common gold possible plane stead dry wonder laugh thousand ago ran check game shape equate hot miss brought heat snow tire bring yes distant fill east paint language among grand ball yet wave drop heart am present heavy dance engine position arm wide sail material size vary settle speak weight general ice matter circle pair include divide syllable felt perhaps pick sudden count square reason length represent art subject region energy hunt probable bed brother egg ride cell believe fraction forest sit race window store summer train sleep prove lone leg exercise wall catch mount wish sky board joy winter sat written wild instrument kept glass grass cow job edge sign visit past soft fun bright gas weather month million bear finish happy hope flower clothe strange gone jump baby eight village meet root buy raise solve metal whether push seven paragraph third shall held hair describe cook floor either result burn hill safe cat century consider type law bit coast copy phrase silent tall sand soil roll temperature finger industry value fight lie beat excite natural view sense ear else quite broke case middle kill son lake moment scale loud spring observe child straight consonant nation dictionary milk speed method organ pay age section dress cloud surprise quiet stone tiny climb cool design poor lot experiment bottom key iron single stick flat twenty skin smile crease hole trade melody trip office receive row mouth exact symbol die least trouble shout except wrote seed tone join suggest clean break lady yard rise bad blow oil blood touch grew cent mix team wire cost lost brown wear garden equal sent choose fell fit flow fair bank collect save control decimal gentle woman captain practice separate difficult doctor please protect noon whose locate ring character insect caught period indicate radio spoke atom human history effect electric expect crop modern element hit student corner party supply bone rail imagine provide agree thus capital won't chair danger fruit rich thick soldier process operate guess necessary sharp wing create neighbor wash bat rather crowd corn compare poem string bell depend meat rub tube famous dollar stream fear sight thin triangle planet hurry chief colony clock mine tie enter major fresh search send yellow gun allow print dead spot desert suit current lift rose continue block chart hat sell success company subtract event particular deal swim term opposite wife shoe shoulder spread arrange camp invent cotton born determine quart nine truck noise level chance gather shop stretch throw shine property column molecule select wrong gray repeat require broad prepare salt nose plural anger claim continent oxygen sugar death pretty skill women season solution magnet silver thank branch match suffix especially fig afraid huge sister steel discuss forward similar guide experience score apple bought led pitch coat mass card band rope slip win dream evening condition feed tool total basic smell valley nor double seat arrive master track parent shore division sheet substance favor connect post spend chord fat glad original share station dad bread charge proper bar offer segment slave duck instant market degree populate chick dear enemy reply drink occur support speech nature range steam motion path liquid log meant quotient teeth shell neck"
(* val besede : string =
  "the of to and a in is it you that he was for on are with as i his they be at one have this from or had by word but what some we can out other were all there when up use your how said an each she which do their time if will way about many then them write would like so these her long make thing see h"... (* string length 5837; truncated *) *)

(*----------------------------------------------------------------------------*
 Sestavite vrednost `slovar : string list`, ki vsebuje vse besede iz slovarja,
 pretvorjene v velike tiskane črke.
[*----------------------------------------------------------------------------*)

let slovariziraj niz = 
  let besedice = String.split_on_char ' ' niz |> List.filter(fun s -> s <> "") in
  List.map String.uppercase_ascii besedice

let slovar = slovariziraj besede

let primer_5_7 = take 42 slovar
(* val primer_5_7 : string list =
  ["THE"; "OF"; "TO"; "AND"; "A"; "IN"; "IS"; "IT"; "YOU"; "THAT"; "HE";
   "WAS"; "FOR"; "ON"; "ARE"; "WITH"; "AS"; "I"; "HIS"; "THEY"; "BE"; "AT";
   "ONE"; "HAVE"; "THIS"; "FROM"; "OR"; "HAD"; "BY"; "WORD"; "BUT"; "WHAT";
   "SOME"; "WE"; "CAN"; "OUT"; "OTHER"; "WERE"; "ALL"; "THERE"; "WHEN"; "UP"] *)

(* POZOR: Primer je zaenkrat zakomentiran, saj ob prazni rešitvi sproži izjemo *)
let primer_5_8 = List.nth slovar 321
(* val primer_5_8 : string = "MEASURE" *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa s črko
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Med ugibanjem seveda ne bomo poznali celotnega ključa. V tem primeru bomo za
 neznane črke uporabili znak `_`. Na primer, če bi vedeli, da je črka `A` v
 besedilu šifrirana kot `X`, črka `C` pa kot `Y`, bi ključ zapisali kot
 `"X_Y_______________________"`.

 Napišite funkcijo `dodaj_zamenjavo : string -> char * char -> string option`,
 ki sprejme ključ ter ga poskusi razširiti z zamenjavo dane črke. Funkcija naj
 vrne `None`, če razširitev vodi v ključ, ki ni bijektiven (torej če ima črka že
 dodeljeno drugo zamenjavo ali če smo isto zamenjavo dodelili dvema različnima
 črkama).
[*----------------------------------------------------------------------------*)

(*Casework it is...*)
let dodaj_zamenjavo kljuc (a, b) =
  let zelena_pozicija = indeks a in
  (*index_opt v prvem argumentu isce drugi argument (obvezno znak!) in vrne option int - Some lokacija ali None*)
  let obstojeca_pozicija = String.index_opt kljuc b in
  match obstojeca_pozicija with
  | Some pos when pos = zelena_pozicija ->
      Some kljuc
  | Some thing ->
      None
  | None ->
      if kljuc.[zelena_pozicija] = '_' then
        Some (String.mapi (fun i c -> if i = zelena_pozicija then b else c) kljuc)
      (*Ocaml NE podpira elif*)
      else if kljuc.[zelena_pozicija] = b then
        Some kljuc
      else
        None

let primer_5_9 = dodaj_zamenjavo "AB__E" ('C', 'X')
(* val primer_5_9 : string option = Some "ABX_E" *)

let primer_5_10 = dodaj_zamenjavo "ABX_E" ('C', 'X')
(* val primer_5_10 : string option = Some "ABX_E" *)

let primer_5_11 = dodaj_zamenjavo "ABY_E" ('C', 'E')
(* val primer_5_11 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Razširjanje ključa z besedo
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 S pomočjo funkcije `dodaj_zamenjavo` sestavite še funkcijo `dodaj_zamenjave :
 string -> string * string -> string option`, ki ključ razširi z zamenjavami, ki
 prvo besedo preslikajo v drugo.
[*----------------------------------------------------------------------------*)

let dodaj_zamenjave kljuc (beseda_1, beseda_2) =
  let dol_1 = String.length beseda_1 in
  let dol_2 = String.length beseda_2 in
  if dol_1 <> dol_2 then
    None
  else
  (*Želeli bi izvajati for zankp, zato ustvarimo pomozno funkcijo. Ideja je, da se sočasno sprehajamo po istoležnih
  črkah besede_1 in besede_2, na parih istoležnih črk pa uporabimo dodaj_zamenjavo.*)
    let rec pomozna kljuc index =
      if index >= dol_1 then
        Some kljuc
      else
        let chr_1 = beseda_1.[index] in
        let chr_2 = beseda_2.[index] in
        match dodaj_zamenjavo kljuc (chr_1, chr_2) with
        | Some nov_kljuc -> pomozna nov_kljuc (index + 1)
        | None -> None
    in
    pomozna kljuc 0


let primer_5_12 = dodaj_zamenjave "__________________________" ("HELLO", "KUNNJ")
(* val primer_5_12 : string option = Some "____U__K___N__J___________" *)

let primer_5_13 = dodaj_zamenjave "ABCDU_____________________" ("HELLO", "KUNNJ")
(* val primer_5_13 : string option = Some "ABCDU__K___N__J___________" *)

let primer_5_14 = dodaj_zamenjave "ABCDE_____________________" ("HELLO", "KUNNJ")
(* val primer_5_14 : string option = None *)

(*----------------------------------------------------------------------------*
 ### Vse možne razširitve
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite funkcijo `mozne_razsiritve : string -> string -> string list ->
 string list`, ki vzame ključ, šifrirano besedo ter slovar vseh možnih besed,
 vrne pa seznam vseh možnih razširitev ključa, ki šifrirano besedo slikajo v eno
 od besed v slovarju.
[*----------------------------------------------------------------------------*)

let mozne_razsiritve _ _ _ = []

let mozne_razsiritve kljuc sifrirana_beseda dict =
  List.fold_right (fun beseda akumulator ->
      match dodaj_zamenjave kljuc (sifrirana_beseda, beseda) with
      | Some nov_kljuc -> nov_kljuc :: akumulator
      | None -> akumulator
    ) dict []

let primer_5_15 =
  slovar
  |> mozne_razsiritve (String.make 26 '_') "KUNNJ"
  |> List.map (fun kljuc -> (kljuc, sifriraj kljuc "KUNNJ"))
(* val primer_5_15 : (string * string) list =
  [("_________YC__R______A_____", "CARRY");
   ("_________DS__O______T_____", "STOOD");
   ("_________NG__E______R_____", "GREEN");
   ("_________LW__E______H_____", "WHEEL");
   ("_________PS__E______L_____", "SLEEP");
   ("_________YH__P______A_____", "HAPPY");
   ("_________RF__O______L_____", "FLOOR");
   ("_________DS__E______P_____", "SPEED");
   ("_________DB__O______L_____", "BLOOD");
   ("_________YH__R______U_____", "HURRY");
   ("_________LS__E______T_____", "STEEL");
   ("_________TS__E______H_____", "SHEET")] *)
(*----------------------------------------------------------------------------*
 ### Odšifriranje
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite funkcijo `odsifriraj : string -> string option`, ki sprejme šifrirano
 besedilo in s pomočjo slovarja besed ugane odšifrirano besedilo. Funkcija naj
 vrne `None`, če ni mogoče najti nobenega ustreznega ključa.
[*----------------------------------------------------------------------------*)
let odsifriraj _ = ()

let rec probaj_desifrirati besede kljuc =
  match besede with
  | [] -> Some kljuc
  | beseda :: ostale_besede ->
      let razsiritev = mozne_razsiritve kljuc beseda slovar in
      let rec morebitna_razsiritev = function
        | [] -> None
        | en_kljuc :: ostali_kljuci ->
            match probaj_desifrirati ostale_besede en_kljuc with
            | Some cel_key -> Some cel_key
            | None -> morebitna_razsiritev ostali_kljuci
      in
      morebitna_razsiritev razsiritev

let odsifriraj sifrirano =
  let besede = String.split_on_char ' ' sifrirano in
  let zacetni_kljuc = String.make 26 '_' in
  match probaj_desifrirati besede zacetni_kljuc with
  | Some kljuc -> Some (String.concat " " (List.map (sifriraj kljuc) besede))
  | None -> None


let primer_5_16 = sifriraj quick_brown_fox "THIS IS A VERY HARD PROBLEM"
(* val primer_5_16 : string = "VKBO BO T AUSD KTSQ MSJHNUF" *)

let primer_5_17 = odsifriraj "VKBO BO T AUSD KTSQ MSJHNUF"
(* val primer_5_17 : string option = Some "THIS IS A VERY HARD PROBLEM" *)
