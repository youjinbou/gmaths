open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
  | Before_rules -> 
    begin
    end
  | After_rules ->
    begin

    flag ["ocaml";"compile";"native";"fastmath"] (S [A "-ffast-math"]);


     ocaml_lib ~extern:true ~dir:"+oUnit" "oUnit";
     ocaml_lib ~extern:true ~dir:"+lablgl" "lablgl";
     ocaml_lib ~extern:true ~dir:"+lablgl" "lablglut";
     ocaml_lib ~extern:false ~dir:"" "gmaths"
    
    end
      

  | _ -> ()
end;;
