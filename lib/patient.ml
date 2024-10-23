(* lib/patient.ml *)

type t = {
  name : string;
  diagnosis : string;
}

(* Constructor function to create a new patient *)
let create name diagnosis = { name; diagnosis }

(* Getters for patient name and diagnosis *)
let name patient = patient.name
let diagnosis patient = patient.diagnosis

(* Define the priority based on diagnosis for triaged scheduling *)
let priority patient =
  match patient.diagnosis with
  | "Appendecitis" -> 3
  | "Sprain" -> 2
  | "Flu" -> 1
  | _ -> 0
