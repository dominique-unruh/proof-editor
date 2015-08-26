type rope = BatText.t

exception NotImplemented of string
exception Syntax_Error of string
exception UserError of string
exception MathError of string

module BigInt : sig (* Bigint type with comparisons *)
  type bigint
  val zero : bigint
  val one : bigint
  val two : bigint
  val bigint_of_int : int -> bigint
  val bigint_of_string : string -> bigint
  val string_of_bigint : bigint -> string
  val rope_of_bigint : bigint -> rope
  val add_bigint : bigint -> bigint -> bigint
  val minus_bigint : bigint -> bigint
  val sub_bigint : bigint -> bigint -> bigint
  val mult_bigint : bigint -> bigint -> bigint
  val sqrt_bigint : bigint -> bigint (* MathError if sqrt does not exist *)
  val div_bigint : bigint -> bigint -> bigint
  val power_bigint : bigint -> bigint -> bigint (* Exponentiation, MathError if not an int, or 0**0 *)
end = struct
  type bigint = string
  let to_big_int i = i |> Big_int.big_int_of_string
  let from_big_int i = i |> Big_int.string_of_big_int
  let bigint_of_int int = int |> string_of_int 
  let zero = bigint_of_int 0
  let one = bigint_of_int 1
  let two = bigint_of_int 2
  let bigint_of_string str = str |> Big_int.big_int_of_string |> from_big_int
  let string_of_bigint x = x
  let rope_of_bigint = BatText.of_string
  let add_bigint a b = Big_int.add_big_int (to_big_int a) (to_big_int b) |> from_big_int
  let minus_bigint a = Big_int.minus_big_int (to_big_int a) |> from_big_int
  let sub_bigint a b = Big_int.sub_big_int (to_big_int a) (to_big_int b) |> from_big_int
  let mult_bigint a b = Big_int.mult_big_int (to_big_int a) (to_big_int b) |> from_big_int
  let power_bigint a b = 
    let b' = to_big_int b in
    let sign = Big_int.sign_big_int b' in
    if a=zero && sign=1 then zero
    else if a=zero then raise (MathError "0^b with b<=0")
    else if a=one then one
    else Big_int.power_big_int_positive_big_int (to_big_int a) b' |> from_big_int
  let sqrt_bigint a =
    let a = to_big_int a in
    let b = try Big_int.sqrt_big_int a
      with Invalid_argument _ -> raise (MathError "negative sqrt")
    in
    if Big_int.eq_big_int (Big_int.mult_big_int b b) a
    then b |> from_big_int
    else raise (MathError "non-int sqrt")
  let div_bigint a b =
    let (q,r) = try Big_int.quomod_big_int (to_big_int a) (to_big_int b)
      with Division_by_zero -> raise (MathError "division by zero") in
    if Big_int.sign_big_int r=0 then from_big_int q
    else raise (MathError "fractional division")

    
end
open BigInt

type number = Int of bigint | IEEE of float | Real of bigint * int
type cmathml' = 
  CN of number
| CI of rope (* type can be added using annotations *)
| CSymbol of rope * rope (* cd, name *)
| CS of rope (* UTF8 *)
| Apply of cmathml * cmathml list
| Bind of cmathml * bvar list * cmathml
| CError of rope * rope * cmathml list (* cd name contents *)
| CBytes of rope
and
  cmathml = cmathml' * semantics
and
  semantics = (rope * rope * annotation) list (* cd name value *)
and
  annotation = CMLAnnot of cmathml (* encoding="MathML-Content" *)
and
  bvar = rope * semantics

type path = int list

let sym_dot = BatText.of_string "."
let ci_to_bvar x : bvar = match x with
    CI(name),attr -> name,attr
  | _ -> raise (MathError ("Not a CI"))
let bvar_to_ci (name,attr) = CI name, attr


module TopXml = Xml

module Abbrevs = struct

let int i = CN (Int (bigint_of_int i)), []
let var x = CI (BatText.of_string x), []
let plus a b = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "plus"),[]), [a;b]), [];;
let minus a b = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "minus"),[]), [a;b]), [];;
let times a b = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "times"),[]), [a;b]), [];;
let divide a b = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "divide"),[]), [a;b]), [];;
let equals_sym = CSymbol (BatText.of_string "relation1", BatText.of_string "eq")
let equals a b = Apply((equals_sym,[]), [a;b]), [];;
let power a b = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "power"),[]), [a;b]), [];;
let implies_sym = CSymbol (BatText.of_string "logic1", BatText.of_string "implies")
let implies a b = Apply((implies_sym,[]), [a;b]), [];;
let sqrt a = Apply((CSymbol (BatText.of_string "arith1", BatText.of_string "root"),[]), [a;int 2]), []
let complex_i = CSymbol (BatText.of_string "nums1", BatText.of_string "i"), []
let true_bool = CSymbol (BatText.of_string "logic1", BatText.of_string "true"), []
let false_bool = CSymbol (BatText.of_string "logic1", BatText.of_string "false"), []
let forall_sym = CSymbol (BatText.of_string "quant1", BatText.of_string "forall"), []
let forall x a = Bind(forall_sym,x,a), []

end

module Xml =
struct

open Xml


let rec make_strict_cmml_cn xml =
  let typ = try attrib xml "type" with No_attribute _ -> "real" in
  let base = try attrib xml "base" |> int_of_string with No_attribute _ -> 10 in
  let content = children xml in
  match typ with
    ("integer"|"real"|"double"|"hexdouble") -> 
      (match base,content with
	10,[PCData _] -> xml
      | _,[PCData _] -> raise (NotImplemented "base != 10")
      | _,_ -> raise (Syntax_Error ("CN of type "^typ^" must have a single PCData child")))
  | "complex-cartesian" -> 
    (match content with
      [PCData re;Element("sep",_,_);PCData im] -> 
	Element("apply",[],[
	  Element("csymbol",["cd","complex1"],[PCData "complex_cartesian"]);
	  make_strict_cmml_cn (Element("cn",["base",string_of_int base;"type","real"],[PCData re]));
	  make_strict_cmml_cn (Element("cn",["base",string_of_int base;"type","real"],[PCData im]))])
    | _ -> raise (Syntax_Error("CN complex_cartesian")))
  | ("e-notation" | "rational" | "complex-polar" | "constant")
      -> raise (NotImplemented ("make_strict_cmml_cn: CN type "^typ))
  | _ -> raise (Syntax_Error ("invalid CN type "^typ))

let rec make_strict_cmml xml =
  match xml with
    Element("cn",_,_) -> make_strict_cmml_cn xml
  | Element("infinity",_,_) -> Element("csymbol",["cd","nums1"],[PCData "infinity"])
  | Element(tag,attrib,children) -> Element(tag,attrib,List.map make_strict_cmml children)
  | PCData _ -> xml

let rec split_last xs = match xs with
    [] -> failwith "empty list"
  | [x] -> [],x
  | x::xs -> let (rest,last) = split_last xs in (x::rest,last)
	     

(* XSLT for reading non-strict CMML: http://dpcarlisle.blogspot.com/2011/07/converting-to-strict-content-mathml.html *)
let from_xml ?strict:(strict=true) xml =
  let dom = parse_string xml in
  let dom = if strict then dom else make_strict_cmml dom in
  let rec f' element = match element with 
      PCData _ -> raise (Syntax_Error "Unexpected PCData in XML")
    | Element ("apply",_,head::args) -> Apply (f head, List.map f args)
    | Element ("bind",_,([]|[_])) -> raise (Syntax_Error ("bind must have at least two children"))
    | Element ("bind",_,head::bvars_arg) -> 
      let (bvars,arg) = split_last bvars_arg in
      let b bvar = match bvar with
	  Element("bvar",[],[v]) -> ci_to_bvar (f v)
	| Element("bvar",[],_) -> raise (Syntax_Error ("bvar must have exactly one child"))
	| _ -> raise (Syntax_Error "expected bvar below bind") in
      Bind (f head, List.map b bvars, f arg)
    | Element ("ci",_,[PCData n]) -> CI (BatText.of_string n)
    | Element ("csymbol",_,[PCData name]) -> CSymbol (BatText.of_string (attrib element "cd"), BatText.of_string name)
    | Element ("cn",_,[PCData num]) ->
      (match attrib element "type" with
	"integer" -> CN (Int (bigint_of_string num))
      | "real" -> 
	let (int,exp) = 
	  try let comma = String.index num '.' in 
		    (String.sub num 0 comma ^ String.sub num (comma+1) (String.length num - comma -1),
		     String.length num - comma - 1)
	  with Not_found -> (num,0) in
	CN (Real (bigint_of_string num, exp))
      | typ -> raise (NotImplemented ("CN of type "^typ^" not supported")))
    | Element ("cn",_,_) -> raise (Syntax_Error ("CN must have exactly one PCData child"))
    | Element (tag,attr,children) -> raise (Syntax_Error ("Unexpected tag "^tag^" in XML")) 
  and f element = (f' element,[]) in
  f dom;;

let rec to_xml' math =
  match math with
    CSymbol(cd,name) -> Element("csymbol",["cd",BatText.to_string cd],[PCData (BatText.to_string name)])
  | CI name -> Element("ci",[],[PCData (BatText.to_string name)])
  | Apply(head,args) -> Element("apply",[],List.map to_xml'' (head::args))
  | CN (Int i) -> Element("cn",["type","integer"],[PCData (string_of_bigint i)])
  | CN _ -> raise (NotImplemented "to_xml: CN (not Int)")
  | CS _ -> raise (NotImplemented "to_xml: CS")
  | CBytes _ -> raise (NotImplemented "to_xml: CBytes")
  | Bind(head,vars,arg) -> 
    Element("bind", [],
	    to_xml'' head
	    :: List.map (fun (v,a) -> Element("bvar",[],[to_xml'' (CI v,a)])) vars
	    @ [to_xml'' arg])
  | CError _ -> raise (NotImplemented "to_xml: CError")
and to_xml'' (math,annot) =
  match annot with
    [] -> to_xml' math
  | _ -> raise (NotImplemented "to_xml: annotations")
  
let to_xml math = to_xml'' math |> Xml.to_string

end (* module Xml *)

let (^^^) = BatText.(^^^)


module RopeKeyType = struct
  type t = rope
  let compare = BatText.compare
end
module RopeMap = BatMap.Make(RopeKeyType)
module RopeSet = BatSet.Make(RopeKeyType)


module Popcorn =
struct

(* Spec: http://java.symcomp.org/FormalPopcorn.html *)

let popcorn_var_prefix = BatText.of_string "$" 
let popcorn_open = BatText.of_string "(" 
let popcorn_close = BatText.of_string ")" 
let popcorn_bind_open = BatText.of_string "[" 
let popcorn_bind_close = BatText.of_string "]" 
let popcorn_bind_arrow = BatText.of_string " -> " 
let popcorn_sep = BatText.of_string ", " 
let popcorn_sym_dot = BatText.of_string "." 
let popcorn_symbol_abbrevs = 
  [ ("transc1.cos", "cos");
    ("transc1.cosh", "cosh");
    ("transc1.cot", "cot");
    ("transc1.coth", "coth");
    ("transc1.csc", "csc");
    ("transc1.csch", "csch");
    ("transc1.exp", "exp");
    ("transc1.sec", "sec");
    ("transc1.sech", "sech");
    ("transc1.sin", "sin");
    ("transc1.sinh", "sinh");
    ("transc1.tan", "tan");
    ("transc1.tanh", "tanh");
    ("arith1.abs", "abs");
    ("arith1.root", "root");
    ("arith1.sum", "sum");
    ("arith1.product", "product");
    ("calculus1.diff", "diff");
    ("calculus1.int", "int");
    ("calculus1.defint", "defint");
    ("nums1.pi", "pi");
    ("nums1.e", "e");
    ("nums1.i", "i");
    ("nums1.infinity", "infinity");
    ("minmax1.min", "min");
    ("minmax1.max", "max");
    ("fns1.lambda", "lambda");
    ("logic1.true", "true");
    ("logic1.false", "false");
    ("combinat1.binomial", "binomial");
    ("integer1.factorial", "factorial");
  ]
  |> List.map (fun (x,y) -> (BatText.of_string x, BatText.of_string y))
  |> BatList.enum
  |> RopeMap.of_enum;;

let popcorn_apply_renderer_default  (to_popcorn:?pri:int -> cmathml -> _) head args =
  to_popcorn ~pri:100 head ^^^ popcorn_open ^^^ (BatText.concat popcorn_sep (List.map to_popcorn args)) ^^^ popcorn_close

let popcorn_bind_renderer_default  (to_popcorn:?pri:int -> cmathml -> _) head vars arg =
  to_popcorn ~pri:100 head ^^^ popcorn_bind_open ^^^ 
    (BatText.concat popcorn_sep (List.map to_popcorn (List.map bvar_to_ci vars)))
  ^^^ popcorn_bind_arrow
  ^^^ to_popcorn arg
  ^^^ popcorn_bind_close

let popcorn_infixn sym sympri =
  let sym = BatText.of_string sym in
  fun (to_popcorn:?pri:int -> _ -> _) pri head args ->
    if (List.length args < 2) 
    then popcorn_apply_renderer_default to_popcorn head args
    else
      let args = List.map (to_popcorn ~pri:sympri) args in
      let expr = BatText.concat sym args in
      let expr = if pri>=sympri then popcorn_open^^^expr^^^popcorn_close else expr in
      expr

let popcorn_infix2 sym sympri =
  let sym = BatText.of_string sym in
  fun (to_popcorn:?pri:int -> _ -> _) pri head args ->
    if (List.length args != 2) 
    then popcorn_apply_renderer_default to_popcorn head args
    else
      let args = List.map (to_popcorn ~pri:sympri) args in
      let expr = BatText.concat sym args in
      let expr = if pri>=sympri then popcorn_open^^^expr^^^popcorn_close else expr in
      expr

let popcorn_list opn close sympri =
  let comma = BatText.of_string "," in
  let opn = BatText.of_string opn in
  let close = BatText.of_string close in
  fun (to_popcorn:?pri:int -> _ -> _) pri head args ->
    let args = List.map (to_popcorn ~pri:sympri) args in
    let expr = BatText.concat comma args in
    opn^^^expr^^^close;;

    

let popcorn_apply_renderers : ((?pri:int -> cmathml -> BatText.t) -> int -> cmathml -> cmathml list -> BatText.t) RopeMap.t = [
  "prog1.block", popcorn_infixn ";" 1;
  "prog1.assign", popcorn_infix2 ":=" 2;
  "logic1.implies", popcorn_infix2 "==>" 3;
  "logic1.equivalent", popcorn_infix2 "<=>" 4;
  "logic1.or", popcorn_infixn "or" 5;
  "logic1.and", popcorn_infixn "and" 6;
  "relation1.lt", popcorn_infix2 "<" 7;
  "relation1.leq", popcorn_infix2 "<=" 8;
  "relation1.gt", popcorn_infix2 ">" 9;
  "relation1.eq", popcorn_infix2 "=" 10;
  "relation1.geq", popcorn_infix2 ">=" 11;
  "relation1.neq", popcorn_infix2 "<>" 12;
  "relation1.neq", popcorn_infix2 "!=" 13;
  "interval1.interval", popcorn_infixn ".." 14;
  "arith1.plus", popcorn_infixn "+" 15;
  "arith1.minus", popcorn_infix2 "-" 16;
  "arith1.times", popcorn_infixn "*" 17;
  "arith1.divide", popcorn_infix2 "/" 18;
  "arith1.power", popcorn_infix2 "^" 19;
  "complex1.complex_cartesian", popcorn_infix2 "|" 20;
  "nums1.rational", popcorn_infix2 "//" 21;
  "list1.list", popcorn_list "[" "]" 22;
  "set1.set", popcorn_list "{" "}" 23;
(*  "list1.list": ((lambda a: "["+",".join(a)+']'),True,22),
#    "set1.set": ((lambda a: "{"+",".join(a)+'}'),True,23),
# TODO: if...then...else...endif	prog1.if	n	Not really a infix operator
# TODO: while...do...endwhile	prog1.while	n	Not really a infix operator *)
] |> List.map (fun (x,y) -> (BatText.of_string x, y))
  |> BatList.enum
  |> RopeMap.of_enum;;

							      
let rec to_popcorn ?pri:(pri=0) (math,annot) = match math with
      CSymbol(cd,name) -> 
	(let symname = cd^^^popcorn_sym_dot^^^name in
	 let symname = try RopeMap.find symname popcorn_symbol_abbrevs with Not_found -> symname in
	symname)
    | CI(name) -> popcorn_var_prefix^^^name
    | Apply((CSymbol(cd,name),[]) as head,args) -> 
      (let symname = cd^^^popcorn_sym_dot^^^name in
      try RopeMap.find symname popcorn_apply_renderers to_popcorn pri head args
      with Not_found -> popcorn_apply_renderer_default to_popcorn head args)
    | Apply(head,args) -> popcorn_apply_renderer_default to_popcorn head args
    | CN(Int i) -> rope_of_bigint i
    | CN(_) -> raise (NotImplemented "to_popcorn: CN (not Int)")
    | CS(_) -> raise (NotImplemented "to_popcorn: CS")
    | Bind(head,vars,arg) -> popcorn_bind_renderer_default to_popcorn head vars arg
    | CError(_) -> raise (NotImplemented "to_popcorn: CError")
    | CBytes(_) -> raise (NotImplemented "to_popcorn: CBytes")

let to_popcorn = to_popcorn ~pri:0;;

end (* module Popcorn *)


module PMathML =
struct

open TopXml

type config = {
  mathjax_hacks: bool;
  add_path: string; (* Attribute that will contain the path *)
}
let default_config : config = {mathjax_hacks=true; add_path="path"}

type state = {
  pri: int;
  path: int list;
  to_pmml: config -> state -> cmathml -> xml
};;

let list_with_sep sep list = 
  let rec f xs = match xs with [] -> [] | [_] -> xs | x::xs -> x::sep::f xs in
  f list

let mi name = Element("mi",[],[PCData name])
let mo_infix name = Element("mo",["form","infix"],[PCData name])
let mo_sep name = Element("mo",["separator","true"],[PCData name])
let mo name = Element("mo",[],[PCData name])
let mrow xs = Element("mrow",[],xs)
(*let mfenced_sep xs = Element("mfenced",[],xs)*)
(* let mfenced xs = Element("mfenced",["separators",""],xs) (* -- mfence gets lost in SVG conversion for some reason *) *)
let mfenced_sep xs = mrow (Element("mo",["fence","true"],[PCData "("])
			 :: list_with_sep (Element("mo",["separator","true"],[PCData ","])) xs @
			   [Element("mo",["fence","true"],[PCData ")"])])
let mfenced xs = mrow (Element("mo",["fence","true"],[PCData "("])
			 :: xs @
			   [Element("mo",["fence","true"],[PCData ")"])])


let symbol_presentations = 
  [ ("nums1.pi", mi "π");
    ("setname1.C", mi "ℂ");
    ("nums1.infinity", mi "∞");
    ("quant1.forall", mi "∀");
  ]
  |> List.map (fun (x,y) -> (BatText.of_string x, y))
  |> BatList.enum
  |> RopeMap.of_enum;;

let max_pri = 1000

let render_arglist config state pri args = 
  (List.mapi (fun i -> state.to_pmml config {state with pri=pri; path=i::1::state.path}) args);;


let add_classes classes element =
  match element with
    Element(tag,attr,children) -> Element(tag,("class",String.concat " " classes)::attr,children)
  | PCData _ -> failwith("add_classes called with PCData");;


let mo_apply_function = mo_infix "&ApplyFunction;" |> add_classes ["leaf"]
let apply_renderer_default config state head args =
  mrow [state.to_pmml config {state with pri=max_pri; path=0::state.path} head;
	mo_apply_function;
	mfenced_sep (render_arglist config state 0 args)]


let infixn sym (sympri:int) =
  fun config state head args ->
    if (List.length args < 2) 
    then apply_renderer_default config state head args
    else
      (*let rec f xs = match xs with [] -> [] | [_] -> xs | x::xs -> x::sym::f xs in*)
      let args = render_arglist config state sympri args in
      let expr = mrow (list_with_sep sym args) in
      let expr = if state.pri>=sympri then mfenced [expr] else expr in
      expr

let infix2 sym (sympri:int) =
  fun config state head args ->
    if (List.length args != 2) 
    then apply_renderer_default config state head args
    else
      let args = render_arglist config state sympri args in
      let expr = match args with [a1;a2] -> [a1;sym;a2] | _ -> assert false in
      let expr = if state.pri>=sympri then mfenced expr else mrow expr in
      expr


let render f =
  fun config state head args ->
    let args' = render_arglist config state 0 args in
    let expr = try f args' with Failure _ -> apply_renderer_default config state head args in
    expr

let render2 f = render (fun args -> match args with [a;b] -> f a b | _ -> failwith "argnum")

let strut = Element("mspace",["height","1ex";"class","leaf"],[])

let apply_renderers : (config -> state -> cmathml -> cmathml list -> xml) RopeMap.t = [
  "prog1.block", infixn (mo_infix ";") 10;
  "prog1.assign", infix2 (mo_infix ":=") 20;
  "logic1.implies", infix2 (mo_infix "⇒") 30;
  "logic1.equivalent", infix2 (mo_infix "⇔") 40;
  "logic1.or", infixn (mo_infix "∨") 50;
  "logic1.and", infixn (mo_infix "∧") 60;
  "relation1.lt", infix2 (mo_infix "<") 70;
  "relation1.leq", infix2 (mo_infix "≤") 80;
  "relation1.gt", infix2 (mo_infix ">") 90;
  "relation1.eq", infix2 (mo_infix "=") 100;
  "set1.in", infix2 (mo_infix "∈") 100;
  "relation1.geq", infix2 (mo_infix "≥") 110;
  "relation1.neq", infix2 (mo_infix "≠") 120;
(*  "interval1.interval", infixn (mo_infix "..") 14; *)
  "arith1.plus", infixn (mo_infix "+") 150;
  "arith1.minus", infix2 (mo_infix "-") 160;
  "arith1.times", infixn (mo_infix "·") 170;
  (*"arith1.divide", infix2 (mo_infix "/") 180;*)
  (*"arith1.power", infix2 (mo_infix "^") 190;*)
  "complex1.complex_cartesian", infix2 (mo_infix "|") 200;
  "nums1.rational", infix2 (mo_infix "//") 210;
(*  "list1.list", list "[" "]" 22;
  "set1.set", list "{" "}" 23; *)


  "arith1.divide", render2 (fun a b -> Element("mfrac",[],[a;b]));
  "arith1.root", render2 (fun a base -> match base with
    Element("mn",_,[PCData "2"]) -> Element("msqrt",[],[mrow [strut;a]]) 
  | _ -> Element("mroot",[],[a;base]));
  "arith1.power", render2 (fun a b -> Element("msup",[],[mrow[strut;a];b])); (* TODO: priorities! *)

] |> List.map (fun (x,y) -> (BatText.of_string x, y))
  |> BatList.enum
  |> RopeMap.of_enum;;




(* let render_arglist config state pri args = 
  (List.mapi (fun i -> state.to_pmml config {state with pri=pri; path=i+1::state.path}) args);; *)

let bind_renderer_default config state head bvars arg =
  mrow [state.to_pmml config {state with pri=max_pri; path=0::state.path} head;
	mrow (
	  bvars
	 |> List.mapi (fun i v -> 
	   state.to_pmml config {state with path=i::1::state.path} (bvar_to_ci v))
	 |> list_with_sep (mo_sep ",")
	);
	mo_sep ".";
	state.to_pmml  config {state with path=2::state.path} arg]

let bind_renderers : (config -> state -> cmathml -> bvar list -> cmathml -> xml) RopeMap.t = [
] |> List.map (fun (x,y) -> (BatText.of_string x, y))
  |> BatList.enum
  |> RopeMap.of_enum;;


			

let add_path config state element =
  match (config.add_path,element) with
    "",_ -> element
  | pathattr, Element(tag,attr,children) -> Element(tag,(pathattr,String.concat "." (List.map string_of_int (List.rev state.path)))::attr,children)
  | _, PCData _ -> failwith("add_path called with PCData")
				      
let rec to_pmml config (state:state) (math,annot) = (match math with
      CSymbol(cd,name) -> 
	(let symname = cd^^^sym_dot^^^name in
	 let symname = try RopeMap.find symname symbol_presentations with Not_found -> mo (BatText.to_string symname) in
	symname) |> add_classes ["leaf";"symbol"]
    | CI(name) -> mi (BatText.to_string name) |> add_classes ["leaf";"variable"]
    | Apply((CSymbol(cd,name),[]) as head,args) -> 
      (let symname = cd^^^sym_dot^^^name in
      try RopeMap.find symname apply_renderers config state head args
      with Not_found -> apply_renderer_default config state head args) |> add_classes ["apply"]
    | Apply(head,args) -> apply_renderer_default config state head args |> add_classes ["apply"]

    | Bind((CSymbol(cd,name),[]) as head,bvars,arg) -> 
      (let symname = cd^^^sym_dot^^^name in
      try RopeMap.find symname bind_renderers config state head bvars arg
      with Not_found -> bind_renderer_default config state head bvars arg) |> add_classes ["bind"]
    | Bind(head,bvars,arg) -> bind_renderer_default config state head bvars arg |> add_classes ["bind"]

    | CN(Int i) -> Element("mn",[],[PCData (string_of_bigint i)]) |> add_classes ["leaf";"number";"integer"]
    | CN(Real(i,e)) -> 
      let num = string_of_bigint i in
      let _ = assert (e>=0) in
      let num = if e>0 then num ^ "e-" ^ string_of_int e else num in
      Element("mn",[],[PCData num])
    | CN(IEEE _) -> raise (NotImplemented "to_pmml: CN/IEEE")
    | CS(_) -> raise (NotImplemented "to_pmml: CS")
    | CError(_) -> raise (NotImplemented "to_pmml: CError")
    | CBytes(_) -> raise (NotImplemented "to_pmml: CBytes"))
  |> add_path config state 

let to_pmml config math = to_pmml config {pri=0; path=[]; to_pmml=to_pmml} math |> TopXml.to_string

end

module RPN = struct

open Abbrevs

module StringMap = Map.Make(String)

let abbrevs : (string list) StringMap.t option ref = ref None

let get_abbrevs () = match !abbrevs with 
    Some m -> m 
  | None ->
    let lines = BatFile.lines_of "abbrevs.txt" in
    let lines = lines |> BatEnum.map (fun s -> BatString.nsplit s " ") in
    let lines = lines |> BatEnum.filter (fun l -> l <> []) in
    let lines = lines |> BatEnum.map (fun s -> match s with x::xs -> x,xs | _ -> failwith "empty line in abbrevs") in
    let map = BatEnum.fold (fun m (k,v) -> StringMap.add k v m) StringMap.empty lines in
    abbrevs := Some map; map



let rec from_list commands stack = 
  (*let _ = print_endline ((String.concat "!" commands)^"#"^(string_of_int (List.length stack))) in*)
  let abbrevs = get_abbrevs () in
  let parse_bigint i =
    try Some (bigint_of_string i) with Failure _ -> None in
  match commands,stack with
    [],[x] -> x
  | [],[] -> failwith "stack empty"
  | [],_ -> failwith "stack has leftover elements"
  | ""::cmds,_ -> from_list cmds stack
  | cmd::cmds,_ ->
    (*let _ = print_endline ("Cmd: "^cmd) in*)
    match parse_bigint cmd with
      Some i -> from_list cmds ((CN (Int i),[])::stack)
    | None -> match cmd,stack with
      "+",a::b::stack -> from_list cmds (plus b a::stack)
      | "-",a::b::stack -> from_list cmds (minus b a::stack)
      | "*",a::b::stack -> from_list cmds (times b a::stack)
      | "/",a::b::stack -> from_list cmds (divide b a::stack)
      | "^",a::b::stack -> from_list cmds (power b a::stack)
      | "sqrt",a::stack -> from_list cmds (sqrt a::stack)
      | "all",v::a::stack -> from_list cmds (forall [ci_to_bvar v] a::stack)
      | "=",a::b::stack -> from_list cmds (equals b a::stack)
      | "->",a::b::stack -> from_list cmds (implies b a::stack)
      | x,_ -> 
	(try from_list (StringMap.find x abbrevs @ cmds) stack
	 with Not_found -> from_list cmds (var x::stack))

let from_string str = from_list (BatString.nsplit str " ") []

end

let autoconvert_from_string str =
  try Xml.from_xml ~strict:false str
  with (Failure _|Syntax_Error _|TopXml.Error _) ->
    try RPN.from_string str
    with Failure _ ->
      raise (UserError "Could not parse the input math (format: auto)")

module Test = struct

let commutative_ops = 
  [ "minmax1.min"; "minmax1.max"; "arith1.plus"; "arith1.times"; "relation1.eq";
  "logic1.equivalent"; "logic1.or"; "logic1.and"; "relation1.neq";
  ]
  |> List.map BatText.of_string |> BatList.enum |> RopeSet.of_enum;;

let commute math = match math with
    Apply(sym,[a1;a2]),attr -> 
      (match sym with
	CSymbol(cd,name),_ when RopeSet.mem (cd^^^sym_dot^^^name) commutative_ops ->
	  Apply(sym,[a2;a1]),attr
      | _ -> raise (UserError ("This is not a commutative operation.
Examples for commutative operations:
+, *, =, etc., but not -, /, etc.)")))
  | _,_ -> raise (UserError "You should select a binary operation (e.g., a+b)")


let associative_ops = 
  [ "minmax1.min"; "minmax1.max"; "logic1.or"; "logic1.and"; "arith1.plus"; "arith1.times";
  ]
  |> List.map BatText.of_string |> BatList.enum |> RopeSet.of_enum;;

let associativity math = 
  let check (sym,_) (sym',_) = 
    (if sym <> sym'
    then raise (UserError "You cannot apply associativity to an expression with two different operations.
E.g. (a+b)+c is OK, and a*(b*c) is OK, but a+(b*c) is not OK."));
    (match sym with
      CSymbol(cd,name) when RopeSet.mem (cd^^^sym_dot^^^name) associative_ops -> ()
    | _ -> raise (UserError "The operation is not associative.
(Associative operations are for example +, * but not -)"))
  in
  match math with
    Apply(sym,[a1;Apply(sym',[a2;a3]),_]),_ -> 
      (check sym sym'; Apply(sym,[Apply(sym,[a1;a2]),[];a3]),[])
  | Apply(sym,[Apply(sym',[a1;a2]),_;a3]),_ -> 
    (check sym sym'; Apply(sym,[a1;Apply(sym,[a2;a3]),[]]),[])
  | _,_ -> raise (UserError "Select a subexpression of the form (a+b)+c or a+(b+c) where + is some associative operation")


let substitute math eq = match eq with
    Apply((sym,_),[a;b]),_ ->
      if sym <> Abbrevs.equals_sym
      then raise (UserError ("Second formula must be an equation (e.g., A=B)"))
      else if a<>math
      then raise (UserError ("Selected term in the first formula must match the left hand side of the second formula.
E.g., f(selected) and selected=other."))
      else b
  | _ -> raise (UserError ("Second formula must be an equation (e.g., A=B)"))

let rec list_map_nth f list i = match i,list with
    0,x::xs -> f x::xs
  | i,x::xs -> x::list_map_nth f xs (i-1)
  | _,[] -> raise (Invalid_argument "list_map_nth: index out of bounds")

let apply_to_subterm f path math = 
  let rec a' (path:int list) math : cmathml' = match math, path with 
      _,[] -> assert false
    | Apply(head,args),0::p -> Apply(a p head,args)
    | Apply(head,args),1::i::p -> Apply(head,list_map_nth (a p) args i)
    | Apply(head,args),[1] -> failwith "path withing argument of Apply lacks index"
    | Apply(head,args),i::_ -> failwith ("index "^(string_of_int i)^" in Apply path invalid")
    | (CN _|CI _|CSymbol _|CS _|CBytes _|CError _),_::_ -> failwith "path too long"
    | Bind _, _::_ -> raise (NotImplemented "apply_to_subterm Bind")
  and a (path:int list) (math,attr) : cmathml = match path with
      [] -> f (math,attr)
    | _ -> (a' path math,attr) in
  a path math;;

let calculate1 sym args : cmathml =
  let sym = BatText.to_string sym in match sym,args with
      "arith1.plus", [CN (Int a),_; CN (Int b),_] -> CN (Int (add_bigint a b)),[]
    | "arith1.times", [CN (Int a),_; CN (Int b),_] -> CN (Int (mult_bigint a b)),[]
    | "arith1.minus", [CN (Int a),_; CN (Int b),_] -> CN (Int (sub_bigint a b)),[]
    | "arith1.root", [CN (Int a),_; CN (Int b),_] when b=BigInt.two -> CN (Int (BigInt.sqrt_bigint a)),[]
    | "arith1.divide", [CN (Int a),_; CN (Int b),_] -> CN (Int (div_bigint a b)),[]
    | "arith1.power", [CN (Int a),_; CN (Int b),_] -> CN (Int (power_bigint a b)),[]
    | "arith1.plus", [a; CN (Int b),_] when b=BigInt.zero -> a
    | "arith1.plus", [CN (Int a),_; b] when a=BigInt.zero -> b
    | "arith1.minus", [a; CN (Int b),_] when b=BigInt.zero -> a
    | "arith1.times", [a; CN (Int b),_] when b=BigInt.one -> a
    | "arith1.times", [CN (Int a),_; b] when a=BigInt.one -> b
    | "arith1.divide", [a; CN (Int b),_] when b=BigInt.one -> a
    | "arith1.times", [a; CN (Int b),_] when b=BigInt.zero -> CN (Int (BigInt.zero)),[]
    | "arith1.times", [CN (Int a),_; b] when a=BigInt.zero -> CN (Int (BigInt.zero)),[]
    | _ -> raise (MathError "Can't calculate")

let rec calculate math = match math with
    Apply(sym,args),attr ->
      let args = List.map calculate args in
      let sym = calculate sym in
      (match sym with
	CSymbol(cd,name),_ ->
	  (try (calculate1 (cd^^^sym_dot^^^name) args)
	  with MathError _ -> Apply(sym,args),attr)
      | _ -> Apply(sym,args),attr)
  | Bind(sym,vars,body),attr -> Bind(calculate sym,vars,calculate body),attr
  | _ -> math
	

(* all=true => no path implies whole formula *)
let apply_to_subterm' ?all:(all=false) (f:cmathml->cmathml) (args:(cmathml * path option) list) =
  match args with
    [math,Some path] -> apply_to_subterm f path math
  | [math,None] -> 
    if all
    then apply_to_subterm f [] math
    else raise (UserError "You need to select a subterm in the formula.")
  | _ -> failwith "Not exactly one transformation argument";;

(* Subterm in the first out of two arguments *)
let apply_to_subterm'' (f:cmathml->cmathml->cmathml)
                       (args:(cmathml * path option) list) =
  match args with
    [math,Some path;math2,None] -> apply_to_subterm (fun x -> f x math2) path math
  | [_,None;_] -> raise (UserError "You need to select a subterm in the first formula.")
  | [_;_,Some _] -> raise (UserError "You must not select a subterm in the second formula.")
  | [] | [_] | _::_::_::_ -> failwith "Not exactly two transformation elements"





let commute_subterm = apply_to_subterm' commute
let associativity_subterm = apply_to_subterm' associativity
let substitute_subterm = apply_to_subterm'' substitute
let calculate_subterm = apply_to_subterm' ~all:true calculate

let modusponens (args:(cmathml * path option) list) = match args with
  [(Apply((sym,_),[a';b]),_), None;
   a,                         None] ->
    if sym <> Abbrevs.implies_sym
    then raise (UserError("First formula must be an implication (e.g., A=>B)"))
    else if a<>a'
    then raise (UserError("Second formula must match premise of first formula.
E.g. A=>B and A,  not A=>B and C."))
    else b
  | [(Apply((_,_),_),_), None;  _,None] ->
    raise (UserError("Second formula must be an implication (e.g., A=>B)"))
  | [_,None; _, None] ->
    raise (UserError("Second formula must be an implication (e.g., A=>B)"))
  | [_,Some _; _,_] ->
    raise (UserError("Do not selected any subterms with this transformation."))
  | [_,_; _,Some _] ->
    raise (UserError("Do not selected any subterms with this transformation."))
  | [] | [_] | _::_::_::_ ->
    failwith ("invalid number of arguments for modusponens")


let trafo name : (cmathml * path option) list -> cmathml = 
  match name with
    "commute" -> commute_subterm
  | "associativity" -> associativity_subterm
  | "modusponens" -> modusponens
  | "substitute" -> substitute_subterm
  | "calculate" -> calculate_subterm
  | _ -> failwith ("Unknown transformation "^name)

let rec cmml_path_pairs args : (cmathml * path option) list = match args with
    cmml::path::rest -> (Xml.from_xml ~strict:true cmml, 
			 if path = "-" then None
			 else BatString.nsplit path "." |> List.map int_of_string 
			    |> fun x -> Some x)
      :: cmml_path_pairs rest
  | [] -> []
  | _ -> failwith "Invalid CMML/Path interleaving"


end;;

let test_main () =
  let from_cmml = Xml.from_xml ~strict:false in
  let from_auto = autoconvert_from_string in
  let to_cmml = Xml.to_xml in
  let to_pmml = PMathML.to_pmml PMathML.default_config in
  let to_popcorn x = x |> Popcorn.to_popcorn |> BatText.to_string in
  match Array.to_list Sys.argv with
    [_; "cmml2pmml"; xml] -> xml |> from_cmml |> to_pmml |> print_endline
  | [_; "cmml2popcorn"; xml] -> xml |> from_cmml |> to_popcorn |> print_endline
  | [_; "rpn2pmml"; xml] -> xml |> RPN.from_string |> to_pmml |> print_endline
  | [_; "rpn2popcorn"; xml] -> xml |> RPN.from_string |> to_popcorn |> print_endline
  | [_; "auto2pmml"; xml] -> xml |> from_auto |> to_pmml |> print_endline
  | [_; "auto2popcorn"; xml] -> xml |> from_auto |> to_popcorn |> print_endline
  | [_; "auto2cmml"; xml] -> xml |> from_auto |> to_cmml |> print_endline


(*  | [_; "commute_popcorn"; xml; path] -> xml |> autoconvert_from_string |> Test.commute_subterm path |> Popcorn.to_popcorn |> BatText.to_string |> print_endline *)

  | _ :: "transformation" :: trafo :: args -> args |> Test.cmml_path_pairs |> Test.trafo trafo |> Xml.to_xml |> print_endline

  | _ -> failwith "invalid command line arguments";;

try test_main()
with UserError(error) ->
  (prerr_string error; exit(1))





