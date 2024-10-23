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

module PatientListPq = MakeListPQ (Patient)

let load_patients_from_csv file =
  let csv_data = Csv.load file in
  List.map (fun row -> create (List.nth row 0) (List.nth row 1)) csv_data

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
         "test_load_patients_from_csv" >:: test_load_patients_from_csv;
       ]

let () = run_test_tt_main suite
