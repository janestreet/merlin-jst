<<<<<<< HEAD
type res = { locs: Warnings.loc list; synced: bool }

val locs_of
  : config:Mconfig.t
  -> env:Env.t
  -> typer_result:Mtyper.result
  -> pos:Lexing.position
  -> scope:[`Project | `Buffer]
  -> string
  -> (res, string) result
||||||| 7b73c6aa3f
=======
type t = { locs: Warnings.loc list; status: Query_protocol.occurrences_status }

val locs_of
  : config:Mconfig.t
  -> env:Env.t
  -> typer_result:Mtyper.result
  -> pos:Lexing.position
  -> scope:[`Project | `Buffer]
  -> string
  -> t
>>>>>>> upstream/main
