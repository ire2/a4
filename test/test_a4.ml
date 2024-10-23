open A4
include PriorityQueues
open OUnit2

module LengthPrioritizedString = struct
  include String

  (* The priority of a string is its length *)
  let priority s = String.length s
end

module StringPQ = MakeListPQ (LengthPrioritizedString)

(* Test inserting strings into the priority queue and checking the resulting
   list *)
let test_insert_and_to_list _ =
  let pq = StringPQ.empty in
  let pq = StringPQ.enqueue "hello" pq in
  let pq = StringPQ.enqueue "world" pq in
  let pq = StringPQ.enqueue "this" pq in
  let pq = StringPQ.enqueue "is" pq in
  let pq = StringPQ.enqueue "a" pq in
  let pq = StringPQ.enqueue "test" pq in
  let result = StringPQ.to_list pq in
  assert_equal [ "a"; "is"; "this"; "test"; "hello"; "world" ] result

(* Add more black-box tests for other interactions like removal or checking
   size *)
let test_remove _ =
  let pq =
    StringPQ.empty |> StringPQ.enqueue "hello" |> StringPQ.enqueue "world"
  in
  let pq = StringPQ.dequeue pq in
  let result = StringPQ.to_list pq in
  assert_equal [ "world" ] result

let suite =
  "PriorityQueue Tests"
  >::: [
         "test_insert_and_to_list" >:: test_insert_and_to_list;
         "test_remove" >:: test_remove;
       ]

let () = run_test_tt_main suite
