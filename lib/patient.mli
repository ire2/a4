(** @author Ignacio Estrada Cavero (ire2) lib/patient.ml *)

type t
(** The type representing a patient. *)

val create : string -> string -> t
(** [create name diagnosis] creates a new patient with the given [name] and
    [diagnosis]. *)

val name : t -> string
(** [name patient] returns the name of the [patient]. *)

val diagnosis : t -> string
(** [diagnosis patient] returns the diagnosis of the [patient]. *)

val priority : t -> int
(** [priority patient] returns the priority of the [patient]. *)
