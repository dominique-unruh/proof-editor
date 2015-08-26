module BigInt : sig
  type bigint
end

type number = Int of BigInt.bigint | IEEE of float | Real of BigInt.bigint * int
type cmathml' = 
  CN of number
| CI of BatText.t (* type can be added using annotations *)
| CSymbol of BatText.t * BatText.t (* cd, name *)
| CS of BatText.t (* UTF8 *)
| Apply of cmathml * cmathml list
| Bind of cmathml * bvar list * cmathml
| CError of BatText.t * BatText.t * cmathml list (* cd name contents *)
| CBytes of BatText.t
and
  (** Encodes a String Content MathML element *)
  cmathml = cmathml' * semantics
and
  semantics = (BatText.t * BatText.t * annotation) list (* cd name value *)
and
  annotation = CMLAnnot of cmathml (* encoding="MathML-Content" *)
and
  bvar = BatText.t * semantics


module Xml : sig
  (** Converts a Content MathML to its XML representation *)
  val to_xml : cmathml -> string
  val from_xml : string -> cmathml
  (** Translates Content MathML (in XML) to the internal representation as a datatype.

     @param strict If false, an attempt is made to translate non-strict Content MathML (default: true)
     @param xml XML code to convert *)
  val from_xml : ?strict:bool -> string -> cmathml
end


module Popcorn : sig
  (** Translates Strict Content MathML to the Popcorn-representation.
      (Popcorn is a representation for OpenMath, but Strict Content MathML has the same data model as OpenMath.)

      @see <http://java.symcomp.org/FormalPopcorn.html> Popcorn specification 
  *)
  val to_popcorn : cmathml -> BatText.t
end


module PMathML : sig
  type config
  val default_config : config
  val to_pmml : config -> cmathml -> string
end
