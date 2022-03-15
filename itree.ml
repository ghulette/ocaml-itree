module type Effect = sig
  type 'a e

  val exec : 'a e -> 'a
end

module ITree (E : Effect) = struct
  type 'r t =
    | Ret : 'r -> 'r t
    | Tau : 'r t -> 'r t
    | Vis : ('a E.e * ('a -> 'r t)) -> 'r t

  let trigger e = Vis (e, fun x -> Ret x)
  let return x = Ret x

  let rec bind m k =
    match m with
    | Ret x -> k x
    | Tau tr -> Tau (bind tr k)
    | Vis (act, f) -> Vis (act, fun x -> bind (f x) k)

  let ( >>= ) = bind

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

let rec spin = M.Tau spin
let rec print_ones : unit M.t = M.Vis (IO.Output 1, fun () -> print_ones)

let rec echo () =
  let open M in
  trigger IO.Input >>= fun x ->
  trigger (IO.Output x) >>= fun () ->
  echo ()

let rec kill9 () =
  let open M in
  trigger IO.Input >>= fun x ->
  if x = 9 then return true else kill9 ()

(* let () = M.run kill9 |> Format.printf "%b\n" *)
let () = M.run (echo ())
