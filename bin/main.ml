open A4
include Patient
include PriorityQueues
open Csv
module PatientTreePQ = MakeTreePQ (Patient)

(** ANSI color code for severity levels: Red *)
let red = "\x1b[1m\x1b[31m"

(** ANSI color code for severity levels: Yellow *)
let yellow = "\x1b[1m\x1b[33m"

(** ANSI color code for severity levels: Green *)
let green = "\x1b[1m\x1b[32m"

(** resets it *)
let reset = "\x1b[1m\x1b[0m"

(** Helper function to get color based on priority *)
let get_color priority =
  match priority with
  | 1 -> red
  | 2 -> yellow
  | 3 -> green
  | _ -> reset

(** [preview pq] prints list [pq] with color given severity. Requirement: has to
    have severity 1-3 *)
let preview pq =
  let patients = PatientTreePQ.to_list pq in
  if patients = [] then print_endline "No patients in the queue."
  else
    List.iteri
      (fun i patient ->
        let color = get_color (Patient.priority patient) in
        print_endline
          (color
          ^ string_of_int (i + 1)
          ^ ". " ^ Patient.name patient ^ " - " ^ Patient.diagnosis patient
          ^ reset))
      patients

(** [load_from_csv file] loads a list of patients from the specified CSV file
    [file]. Each row in the CSV should contain two fields: a patient's name and
    diagnosis. Requirements: The CSV file must have exactly two columns: the
    first column for the patient's name and the second for the diagnosis. *)
let load_from_csv file =
  try
    let csv_data = Csv.load file in
    let list_patients =
      List.map
        (fun row ->
          let name = List.nth row 0 in
          let diagnosis = List.nth row 1 in
          Patient.create name diagnosis)
        csv_data
    in
    List.fold_left
      (fun pq patient -> PatientTreePQ.enqueue patient pq)
      PatientTreePQ.empty list_patients
  with Sys_error msg ->
    print_endline ("Error reading file: " ^ msg);
    PatientTreePQ.empty

(** [admit pq diagnosis patient ] Simulates the entry of a new [patient] into
    the waiting room of [pq]*)
let admit pq name diagnosis =
  match diagnosis with
  | "Appendicitis" | "Sprain" | "Flu" ->
      let new_patient = Patient.create name diagnosis in
      let updated_pq = PatientTreePQ.enqueue new_patient pq in
      print_endline (name ^ " with " ^ diagnosis ^ " has been admitted.");
      updated_pq
  | _ ->
      print_endline
        "Invalid diagnosis! Must be 'Appendicitis', 'Sprain', or 'Flu'.";
      pq

(* [treat pq] treats the patient with the highest priority and removes them from
   the waiting room [pq] *)
let treat pq =
  match PatientTreePQ.front pq with
  | exception PatientTreePQ.Empty ->
      print_endline "No patients in the queue.";
      pq
  | patient ->
      print_endline
        ("Treating: " ^ Patient.name patient ^ " with "
       ^ Patient.diagnosis patient ^ ".");
      PatientTreePQ.dequeue pq

(**main loop *)
let rec loop pq =
  print_string "\n\x1b[1mEnter command (lower-case): \n";
  print_string "\x1b[1m  areview\n";
  print_string "\x1b[1m  admit <diagnosis> <name>\n";
  print_string "\x1b[1m  treat\n";
  print_string "\x1b[1m  quit\n";
  print_endline "";
  print_string "\x1b[1mCommand: ";
  let input = read_line () in
  match String.split_on_char ' ' input with
  | [ "preview" ] ->
      preview pq;
      loop pq
  | "admit" :: diagnosis :: name_parts ->
      let name = String.concat " " name_parts in
      let pq = admit pq name diagnosis in
      loop pq
  | [ "treat" ] ->
      let pq = treat pq in
      loop pq
  | [ "quit" ] ->
      if not (PatientTreePQ.is_empty pq) then
        print_endline
          "There are still patients in the waiting room, but exiting anyway.";
      print_endline "Goodbye!";
      exit 0
  | _ ->
      print_endline "Invalid command. Please try again.";
      loop pq

(* Entry point for the driver *)
let () =
  let pq =
    if Array.length Sys.argv > 1 then (
      let file = Sys.argv.(1) in
      print_endline ("Loading patients from " ^ file ^ "...");
      load_from_csv file)
    else (
      print_endline "Starting with an empty waiting room...";
      PatientTreePQ.empty)
  in
  loop pq
