open A4
include PriorityQueues
include Patient
open OUnit2

module LengthPrioritizedString = struct
  include String

  (* The priority of a string is its length *)
  let priority s = String.length s
end

module StringPQ = MakeListPQ (LengthPrioritizedString)

(* Test inserting strings into the priority queue and checking the resulting
   list *)
let test_insert_and_to_list_string _ =
  let pq = StringPQ.empty in
  let pq = StringPQ.enqueue "hello" pq in
  let pq = StringPQ.enqueue "world" pq in
  let pq = StringPQ.enqueue "this" pq in
  let pq = StringPQ.enqueue "is" pq in
  let pq = StringPQ.enqueue "a" pq in
  let pq = StringPQ.enqueue "test" pq in
  let result = StringPQ.to_list pq in
  assert_equal [ "a"; "is"; "this"; "test"; "hello"; "world" ] result

(* Add more black-box tests for other interactions like removal or checking size
   string *)
let test_remove_string _ =
  let pq =
    StringPQ.empty |> StringPQ.enqueue "hello" |> StringPQ.enqueue "world"
  in
  let pq = StringPQ.dequeue pq in
  let result = StringPQ.to_list pq in
  assert_equal [ "world" ] result

(* Test checking if the priority queue is empty *)
let test_is_empty_string _ =
  let pq = StringPQ.empty in
  assert_equal true (StringPQ.is_empty pq);
  let pq = StringPQ.enqueue "hello" pq in
  assert_equal false (StringPQ.is_empty pq)

(* Test dequeueing from an empty queue *)
let test_dequeue_empty _ =
  let pq = StringPQ.empty in
  assert_raises StringPQ.Empty (fun () -> StringPQ.dequeue pq)

(* Test inserting multiple elements with the same priority *)
let test_multiple_same_priority _ =
  let pq = StringPQ.empty in
  let pq = StringPQ.enqueue "a" pq in
  let pq = StringPQ.enqueue "b" pq in
  let pq = StringPQ.enqueue "c" pq in
  let result = StringPQ.to_list pq in
  assert_equal [ "a"; "b"; "c" ] result

(* Test front element of the priority queue *)
let test_front_string _ =
  let pq =
    StringPQ.empty |> StringPQ.enqueue "hello" |> StringPQ.enqueue "world"
  in
  assert_equal "hello" (StringPQ.front pq)

module PatientListPq = MakeListPQ (Patient)

(* Function to load csv file of patients *)
let load_patients_from_csv file =
  let csv_data = Csv.load file in
  List.map (fun row -> create (List.nth row 0) (List.nth row 1)) csv_data

(* Test to make sure they loaded c orrectly in priorityQueue *)
let test_load_patients_from_csv _ =
  let patients = load_patients_from_csv "../data/waiting_room.csv" in
  let pq =
    List.fold_left
      (fun pq patient -> PatientListPq.enqueue patient pq)
      PatientListPq.empty patients
  in
  let result = PatientListPq.to_list pq in
  assert_equal
    [
      "Jennifer Lawrence";
      "Tom Holland";
      "Austin Butler";
      "Zendaya";
      "Timothee Chalamet";
      "Margot Robbie";
    ]
    (List.map Patient.name result)

let suite =
  "PriorityQueue Tests"
  >::: [
         "test_insert_and_to_list" >:: test_insert_and_to_list_string;
         "test_remove" >:: test_remove_string;
         "test_dequeue_empty" >:: test_dequeue_empty;
         "test_is_empty_string" >:: test_is_empty_string;
         "test_front_string" >:: test_front_string;
         "test_multiple_same_priority" >:: test_multiple_same_priority;
         "test_load_patients_from_csv" >:: test_load_patients_from_csv;
       ]

let () = run_test_tt_main suite
