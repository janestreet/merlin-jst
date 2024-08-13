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
||||||| fcc3157ab0
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
>>>>>>> 501-plus-upstream-main-9fa77db
