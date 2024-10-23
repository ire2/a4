open A4
(**@author Ignac io Estrada Cavero (ire2), test/test_a4.ml *)

include PriorityQueues
include Patient
open OUnit2

module LengthPrioritizedString = struct
  include String

  (* The priority of a string is its length *)
  let priority s = String.length s
end

module StringPQTester
    (Q : PriorityQueue with type elt = LengthPrioritizedString.t) =
struct
  open OUnit2

  (* Helper function to convert a string list to a string for printing *)
  let string_list_printer lst = "[" ^ String.concat "; " lst ^ "]"

  (* Test inserting strings into the priority queue and checking the resulting
     list *)
  let test_insert_and_to_list_string _ =
    let pq =
      Q.empty |> Q.enqueue "hello" |> Q.enqueue "world" |> Q.enqueue "this"
      |> Q.enqueue "is" |> Q.enqueue "a" |> Q.enqueue "test"
    in
    let result = Q.to_list pq in
    assert_equal ~printer:string_list_printer
      [ "a"; "is"; "this"; "test"; "hello"; "world" ]
      result

  (* Test removing an element from the priority queue *)
  let test_remove_string _ =
    let pq = Q.empty |> Q.enqueue "hello" |> Q.enqueue "world" in
    let pq = Q.dequeue pq in
    let result = Q.to_list pq in
    assert_equal ~printer:string_list_printer [ "world" ] result

  (* Helper function to convert a bool to a string *)
  let bool_printer b = if b then "true" else "false"

  (* Test checking if the priority queue is empty *)
  let test_is_empty_string _ =
    let pq = Q.empty in
    assert_equal ~printer:bool_printer true (Q.is_empty pq);
    let pq = Q.enqueue "hello" pq in
    assert_equal ~printer:bool_printer false (Q.is_empty pq)

  (* Test dequeueing from an empty queue *)
  let test_dequeue_empty _ =
    let pq = Q.empty in
    assert_raises Q.Empty (fun () -> Q.dequeue pq)

  (* Test inserting multiple elements with the same priority *)
  let test_multiple_same_priority _ =
    let pq = Q.empty |> Q.enqueue "a" |> Q.enqueue "b" |> Q.enqueue "c" in
    let result = Q.to_list pq in
    assert_equal ~printer:string_list_printer [ "a"; "b"; "c" ] result

  (* Test front element of the priority queue *)
  let test_front_string _ =
    let pq = Q.empty |> Q.enqueue "hello" |> Q.enqueue "world" in
    assert_equal ~printer:(fun s -> s) "hello" (Q.front pq)

  let suite =
    "StringPQ Tests"
    >::: [
           "test_insert_and_to_list_string" >:: test_insert_and_to_list_string;
           "test_remove_string" >:: test_remove_string;
           "test_is_empty_string" >:: test_is_empty_string;
           "test_dequeue_empty" >:: test_dequeue_empty;
           "test_multiple_same_priority" >:: test_multiple_same_priority;
           "test_front_string" >:: test_front_string;
         ]
end

module PatientTester (Q : PriorityQueue with type elt = Patient.t) = struct
  open OUnit2

  (* Helper function to convert a string list to a string for printing *)
  let string_list_printer lst = "[" ^ String.concat "; " lst ^ "]"

  (* Function to load csv file of patients *)
  let load_patients_from_csv file =
    let csv_data = Csv.load file in
    List.map (fun row -> create (List.nth row 0) (List.nth row 1)) csv_data

  (* Test to make sure they loaded correctly in priorityQueue *)
  let test_load_patients_from_csv _ =
    let patients = load_patients_from_csv "../data/waiting_room.csv" in
    let pq =
      List.fold_left (fun pq patient -> Q.enqueue patient pq) Q.empty patients
    in
    let result = Q.to_list pq in
    assert_equal ~printer:string_list_printer
      [
        "Jennifer Lawrence";
        "Tom Holland";
        "Timothee Chalamet";
        "Margot Robbie";
        "Austin Butler";
        "Zendaya";
      ]
      (List.map Patient.name result)

  (* Test creating a patient with an invalid diagnosis *)
  let test_create _ =
    assert_raises
      (Failure "Invalid diagnosis! Must be 'Appendicitis', 'Sprain', or 'Flu'.")
      (fun () -> create "Invalid Patient" "Cold")

  (* Test the diagnosis orde of the patients *)
  let test_diagnosis _ =
    let patients = load_patients_from_csv "../data/waiting_room.csv" in
    let pq =
      List.fold_left (fun pq patient -> Q.enqueue patient pq) Q.empty patients
    in
    let result = Q.to_list pq in
    assert_equal ~printer:string_list_printer
      [ "Appendicitis"; "Appendicitis"; "Sprain"; "Sprain"; "Flu"; "Flu" ]
      (List.map Patient.diagnosis result)

  let suite =
    "PatientPQ Tests"
    >::: [
           "test_load_patients_from_csv" >:: test_load_patients_from_csv;
           "test_diagnosis" >:: test_diagnosis;
           "test_create" >:: test_create;
         ]
end

module ListStringPQTester = StringPQTester (MakeListPQ (LengthPrioritizedString))
module TreeStringPQTester = StringPQTester (MakeTreePQ (LengthPrioritizedString))
module ListPatientPQTester = PatientTester (MakeListPQ (Patient))
module TreePatientPQTester = PatientTester (MakeTreePQ (Patient))

let suite =
  "Combined PriorityQueue Tests"
  >::: [
         ListStringPQTester.suite;
         TreeStringPQTester.suite;
         ListPatientPQTester.suite;
         TreePatientPQTester.suite;
       ]

let () = run_test_tt_main suite
