(** An alias to the parameter [P] *)
module As_alias = P

module Included : sig
  include module type of struct
    include P
  end
end
