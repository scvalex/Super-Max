open Core.Std

module On_disk : sig
  type t

  include Sexpable.S with type t := t
end

type t
