module type Effect = sig
  type 'a e

  val exec : 'a e -> 'a
end

module ITree (E : Effect) = struct
  type 'r t =
    | Ret : 'r -> 'r t
    | Tau : 'r t -> 'r t
    | Vis : ('a E.e * ('a -> 'r t)) -> 'r t

  let rec run_with_fuel n tr =
    if n = 0 then failwith "no more fuel"
    else
      match tr with
      | Ret x -> x
      | Tau tr' -> run_with_fuel (n - 1) tr'
      | Vis (act, f) -> run_with_fuel (n - 1) (f (E.exec act))

  let rec run tr =
    match tr with
    | Ret x -> x
    | Tau tr' -> run tr'
    | Vis (act, f) -> run (f (E.exec act))
end

module IO = struct
  type _ e = Input : int e | Output : int -> unit e

  let exec : type r. r e -> r = function
    | Input -> read_int ()
    | Output n ->
        print_int n;
        print_newline ()
end

module M = ITree (IO)

let rec print_ones : unit M.t = M.Vis (IO.Output 1, fun () -> print_ones)

let rec echo : unit M.t =
  M.Vis (IO.Input, fun x -> M.Vis (IO.Output x, fun () -> echo))

let rec spin : unit M.t = M.Tau spin

let () =
  M.run_with_fuel 5 print_ones;
  M.run echo
