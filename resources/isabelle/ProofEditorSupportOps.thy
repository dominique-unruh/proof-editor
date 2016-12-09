theory ProofEditorSupportOps
imports 
  ProofEditorSupport
  "protocol/Protocol"
  (*"operations/Basic"
  "operations/ML_Expr"
  "operations/HOL_Operations"*)
begin

operation_setup simplify_term = \<open>
  {from_lib = Codec.term,
   to_lib = Codec.term,
   action = simplify_term}\<close>

operation_setup type_inference = \<open>
  {from_lib = Codec.term,
   to_lib = Codec.term,
   action = type_inference}\<close>

operation_setup term_to_string = \<open>
  {from_lib = Codec.term,
   to_lib = Codec.string,
   action = term_to_string}\<close>

operation_setup ping = \<open>
  {from_lib = Codec.unit,
   to_lib = Codec.unit,
   action = (fn () => ())}\<close>

end