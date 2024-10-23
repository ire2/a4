(** @author Ignacio Estrada Cavero (ire2) lib/patient.ml*)

type t = {
  name : string;
  diagnosis : string;
}

let create name diagnosis =
  match diagnosis with
  | "Appendicitis" | "Sprain" | "Flu" -> { name; diagnosis }
  | _ ->
      failwith "Invalid diagnosis! Must be 'Appendicitis', 'Sprain', or 'Flu'."

let name patient = patient.name
let diagnosis patient = patient.diagnosis

let priority patient =
  match patient.diagnosis with
  | "Appendicitis" -> 1
  | "Sprain" -> 2
  | "Flu" -> 3
  | _ -> 0
