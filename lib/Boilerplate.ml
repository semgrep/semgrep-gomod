(**
   Boilerplate to be used as a template when mapping the gomod CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_raw_string_literal (env : env) (tok : CST.raw_string_literal) =
  (* raw_string_literal *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_imm_tok_prec_p1_pat_101b4f2 (env : env) (tok : CST.imm_tok_prec_p1_pat_101b4f2) =
  (* pattern "[^\"\\n\\\\]+" *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Raw_str_lit tok -> R.Case ("Raw_str_lit",
      (* raw_string_literal *) token env tok
    )
  | `Inte_str_lit (v1, v2, v3) -> R.Case ("Inte_str_lit",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Imm_tok_prec_p1_pat_101b4f2 x -> R.Case ("Imm_tok_prec_p1_pat_101b4f2",
              map_imm_tok_prec_p1_pat_101b4f2 env x
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* "\"" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_string_or_ident (env : env) (x : CST.string_or_ident) =
  (match x with
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_module_path (env : env) (x : CST.module_path) =
  map_string_or_ident env x

let map_tool (env : env) (x : CST.tool) =
  map_string_or_ident env x

let map_toolchain_name (env : env) (x : CST.toolchain_name) =
  map_string_or_ident env x

let map_version (env : env) (x : CST.version) =
  map_string_or_ident env x

let map_go_version (env : env) (x : CST.go_version) =
  map_string_or_ident env x

let map_require_spec (env : env) ((v1, v2, v3) : CST.require_spec) =
  let v1 = map_module_path env v1 in
  let v2 = map_version env v2 in
  let v3 = (* "\n" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_exclude_spec (env : env) ((v1, v2, v3) : CST.exclude_spec) =
  let v1 = map_module_path env v1 in
  let v2 = map_version env v2 in
  let v3 = (* "\n" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_replace_spec (env : env) (x : CST.replace_spec) =
  (match x with
  | `Module_path_opt_vers_EQGT_file_path_LF (v1, v2, v3, v4, v5) -> R.Case ("Module_path_opt_vers_EQGT_file_path_LF",
      let v1 = map_module_path env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_version env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "=>" *) token env v3 in
      let v4 = (* identifier *) token env v4 in
      let v5 = (* "\n" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Module_path_opt_vers_EQGT_module_path_vers_LF (v1, v2, v3, v4, v5, v6) -> R.Case ("Module_path_opt_vers_EQGT_module_path_vers_LF",
      let v1 = map_module_path env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_version env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "=>" *) token env v3 in
      let v4 = map_module_path env v4 in
      let v5 = map_version env v5 in
      let v6 = (* "\n" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

let map_retract_spec (env : env) ((v1, v2) : CST.retract_spec) =
  let v1 =
    (match v1 with
    | `LBRACK_vers_COMMA_vers_RBRACK (v1, v2, v3, v4, v5) -> R.Case ("LBRACK_vers_COMMA_vers_RBRACK",
        let v1 = (* "[" *) token env v1 in
        let v2 = map_version env v2 in
        let v3 = (* "," *) token env v3 in
        let v4 = map_version env v4 in
        let v5 = (* "]" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    | `Vers x -> R.Case ("Vers",
        map_version env x
      )
    )
  in
  let v2 = (* "\n" *) token env v2 in
  R.Tuple [v1; v2]

let map_directive (env : env) (x : CST.directive) =
  (match x with
  | `Module_dire (v1, v2) -> R.Case ("Module_dire",
      let v1 = (* "module" *) token env v1 in
      let v2 =
        (match v2 with
        | `Module_path x -> R.Case ("Module_path",
            map_module_path env x
          )
        | `LPAR_LF_module_path_LF_RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_module_path_LF_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = map_module_path env v3 in
            let v4 = (* "\n" *) token env v4 in
            let v5 = (* ")" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Go_dire (v1, v2, v3) -> R.Case ("Go_dire",
      let v1 = (* "go" *) token env v1 in
      let v2 = map_go_version env v2 in
      let v3 = (* "\n" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Tool_dire_c03956b (v1, v2) -> R.Case ("Tool_dire_c03956b",
      let v1 = (* "tool" *) token env v1 in
      let v2 =
        (match v2 with
        | `Tool x -> R.Case ("Tool",
            map_tool env x
          )
        | `LPAR_LF_rep_tool_RPAR_LF (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_rep_tool_RPAR_LF",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = R.List (List.map (map_tool env) v3) in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* "\n" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Tool_dire_55e7f4a (v1, v2) -> R.Case ("Tool_dire_55e7f4a",
      let v1 = (* "toolchain" *) token env v1 in
      let v2 = map_toolchain_name env v2 in
      R.Tuple [v1; v2]
    )
  | `Requ_dire (v1, v2) -> R.Case ("Requ_dire",
      let v1 = (* "require" *) token env v1 in
      let v2 =
        (match v2 with
        | `Requ_spec x -> R.Case ("Requ_spec",
            map_require_spec env x
          )
        | `LPAR_LF_rep_requ_spec_RPAR_LF (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_rep_requ_spec_RPAR_LF",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = R.List (List.map (map_require_spec env) v3) in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* "\n" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Excl_dire (v1, v2) -> R.Case ("Excl_dire",
      let v1 = (* "exclude" *) token env v1 in
      let v2 =
        (match v2 with
        | `Excl_spec x -> R.Case ("Excl_spec",
            map_exclude_spec env x
          )
        | `LPAR_LF_rep_excl_spec_RPAR_LF (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_rep_excl_spec_RPAR_LF",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = R.List (List.map (map_exclude_spec env) v3) in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* "\n" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Repl_dire (v1, v2) -> R.Case ("Repl_dire",
      let v1 = (* "replace" *) token env v1 in
      let v2 =
        (match v2 with
        | `Repl_spec x -> R.Case ("Repl_spec",
            map_replace_spec env x
          )
        | `LPAR_LF_rep_repl_spec_RPAR_LF (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_rep_repl_spec_RPAR_LF",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = R.List (List.map (map_replace_spec env) v3) in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* "\n" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Retr_dire (v1, v2) -> R.Case ("Retr_dire",
      let v1 = (* "retract" *) token env v1 in
      let v2 =
        (match v2 with
        | `LPAR_LF_rep_retr_spec_RPAR_LF (v1, v2, v3, v4, v5) -> R.Case ("LPAR_LF_rep_retr_spec_RPAR_LF",
            let v1 = (* "(" *) token env v1 in
            let v2 = (* "\n" *) token env v2 in
            let v3 = R.List (List.map (map_retract_spec env) v3) in
            let v4 = (* ")" *) token env v4 in
            let v5 = (* "\n" *) token env v5 in
            R.Tuple [v1; v2; v3; v4; v5]
          )
        | `Retr_spec x -> R.Case ("Retr_spec",
            map_retract_spec env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

let map_source_file (env : env) (xs : CST.source_file) =
  R.List (List.map (map_directive env) xs)

let map_comment (env : env) ((v1, v2) : CST.comment) =
  let v1 = (* "//" *) token env v1 in
  let v2 = map_pat_4fd4a56 env v2 in
  R.Tuple [v1; v2]

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
