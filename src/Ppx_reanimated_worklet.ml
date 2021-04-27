open Ppxlib
open Ppx_worklet

let () =
 Driver.register_transformation
    ~rules:[
      Apply.Rule.expr;
      Apply.Rule.letexpr;
      Ui.Rule.expr;
      Js.Rule.expr;
      Worklet.Rule.expr;
      Worklet.Rule.letexpr;
      Worklet.Rule.stmt;
    ]
   "precisely"
