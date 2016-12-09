theory ProofEditorSupport
imports Complex_Main
begin

ML {*
  val ctx = @{context}
  fun simplify_term t = 
  let val ct = Thm.cterm_of ctx t
      val eq = Simplifier.asm_full_rewrite ctx ct
      val (_,result) = eq |> Thm.prop_of |> Logic.dest_equals
  in 
  result
  end 

  val type_inference = Syntax.check_term ctx
*}

ML {*
  val term_to_string = let
  val ctx = @{context} |> Config.put show_markup false
                       |> Config.put show_types true
                       |> Config.put show_sorts true
  in
  Print_Mode.setmp ["ASCII"] (Syntax.string_of_term ctx)
  end
*}


end
