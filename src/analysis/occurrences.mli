type res = { locs: Warnings.loc list; synced: bool }

val locs_of
  : config:Mconfig.t
  -> env:Env.t
  -> typer_result:Mtyper.result
  -> pos:Lexing.position
  -> scope:[`Project | `Buffer]
  -> string
  -> (res, string) result
