(** @author Ignacio Estrada Cavero (ire2), *)

(** A priority queue is a data abstraction that stores elements with associated
    priorities. Elements are removed in order of priority. If elements are tied
    in priority, then they are removed in order of insertion: an element that
    was enqueued earlier is removed before an element that was enqueued later. *)
module type PriorityQueue = sig
  type elt
  (** The type of an element in the priority queue. *)

  type t
  (** The type of a priority queue. *)

  exception Empty
  (** Raised when an operation cannot be performed because a priority queue is
      empty. *)

  val empty : t
  (** [empty] is the empty priority queue. *)

  val is_empty : t -> bool
  (** [is_empty q] is [true] when [q] is the empty priority queue, and [false]
      otherwise. *)

  val enqueue : elt -> t -> t
  (** [enqueue x q] is the priority queue that results from inserting [x] into
      [q]. *)

  val front : t -> elt
  (** [front q] is the highest-priority element in [q]. Raises: [Empty] when [q]
      is empty. *)

  val dequeue : t -> t
  (** [dequeue q] is the priority queue that results from deleting the
      highest-priority element from [q]. Raises: [Empty] when [q] is empty. *)

  val to_list : t -> elt list
  (** [to_list q] is the list of elements in [q], in the order that they would
      be removed from the queue. That is, the elements are listed in the order
      they would be seen if we were to repeatedly call [front] and [dequeue]
      until [q] ran empty. *)
end

module type Prioritizable = sig
  type t

  val priority : t -> int
  (** [priority x] is a non-negative number representing the priority of [x].
      Smaller integers represent higher priority. The highest priority is
      therefore [0]. *)
end

module MakeListPQ (T : Prioritizable) : PriorityQueue with type elt = T.t =
struct
  type elt = T.t

  (* AF: A list `t` represents a priority queue where the elements are ordered
     according to their priority. *)
  (* RI: The list `t` is sorted in non-decreasing order of priority. *)
  type t = elt list

  exception Empty

  let empty = []
  let is_empty q = q = []

  let enqueue x q =
    let rec aux elt = function
      | [] -> [ elt ]
      | hd :: tl ->
          if T.priority elt < T.priority hd then elt :: hd :: tl
          else hd :: aux elt tl
    in
    aux x q

  let front = function
    | [] -> raise Empty
    | hd :: _ -> hd

  let dequeue = function
    | [] -> raise Empty
    | _ :: tl -> tl

  let to_list q = q
end