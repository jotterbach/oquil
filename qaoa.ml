open Lwt
open Cohttp
open Cohttp_lwt_unix
open Core
open Yojson
open Yojson.Safe
open Str
open Graph
open Owl


module EvoStrat : sig
  val optimize : lr:float ->
                 bs:int ->
                 scale:float ->
                 f:(inp:float array -> float) ->
                 x0:float array ->
                 epochs:int ->
                 float array * float array array
end = struct

(*
The Evolution Strategy Optimizer is a Hill-Climbing routine based on stochastic gradient
estimation (not to be confused with stochastic gradient descent optimization). The basic idea
is to perturb the current best parameter value using Gaussian noise and explore the function
values of the blackbox objective in the vicinity of the parameters. From these explorations one
can estimate the gradient and perform a gradient update step.
More information can be found in:
- T. Salimans et al. Evolution Strategies as a Scalable Alternative to Reinforcement Learning,
    arXiv:1703.03864, https://arxiv.org/pdf/1703.03864.pdf
- D. Wierstra et al. Natural Evolution Strategies, JMLR 15 (2014) 949-980,
    http://www.jmlr.org/papers/volume15/wierstra14a/wierstra14a.pdf
*)


  let es_grad ~batch_size:bs ~f:f ~inp:(inp : float array) ~scale:scale =
    let dim = Array.length inp in
    let center = Mat.of_array inp 1 dim in
    let all_perts = Mat.gaussian bs dim in
    let displaced_perts = Mat.(center + (scale $* all_perts)) in
    let all_func_vals = Mat.map_rows (fun m -> f ~inp:(m |> Mat.to_array)) displaced_perts in
    let mu = Stats.mean all_func_vals in
    let sigma = Stats.std all_func_vals in
    let norm_func_vec = Mat.( ((of_array all_func_vals bs 1) -$ mu) /$ sigma ) in
    Mat.(norm_func_vec * all_perts) |> Mat.fold ~axis:0 (+.) 0.


  let update_params lr bs scale f params =
    let par_vec = Mat.of_array (Array.copy params) 1 (Array.length params) in
    let grad = es_grad ~batch_size:bs ~f:f ~inp:params ~scale:scale in
    let alpha = lr /. (scale *. (bs |> float_of_int)) in
    Mat.(par_vec  + (alpha $* grad)) |> Mat.to_array


  let optimize ~lr:lr ~bs:bs ~scale:scale  ~f:f ~x0:x0 ~epochs:epochs =
    let vals = ref [|f ~inp:x0|] in
    let params = ref [|(Array.copy x0)|] in
    let p = ref (Array.copy x0) in
    let n_step = ref 0 in
    while !n_step <= epochs do
      incr n_step;
      p := (update_params lr bs scale f !p);
      params := Array.append !params [|!p|];
                                      vals := Array.append !vals [|(f ~inp:!p)|];
                                      done;
                                      (!vals, !params)

end

(*let uri = Uri.of_string "https://api.rigetti.com/qvm"*)
let uri = Uri.of_string "http://localhost:5000"


type meas_type =
  | TYPE_MULTISHOT_MEASURE
  | TYPE_EXPECTATION

let meas_type_to_string = function
  | TYPE_MULTISHOT_MEASURE -> "multishot-measure"
  | TYPE_EXPECTATION -> "expectation"

module Creds : sig
  type t = { key: string; userid: string}
  val from_file : string -> t
end = struct
  type t = { key: string; userid: string}
  let load_file path =
    let re = Str.regexp "\\(key\\|user_id\\): \\(.*\\)" in
    let raw_cnt = Core.In_channel.read_lines path in
    let match_fn line =
      if Str.string_match re line 0
      then
        (Str.matched_group 1 line,
         Str.matched_group 2 line)
      else ("", "") in
    let creds_list = List.map raw_cnt ~f:(fun x -> match_fn x)
                     |> List.filter ~f:(fun (x,y) -
                                            > x="key" || x="user_id") in
    let get_key = List.filter creds_list (fun (x,y) -> x="key")
                  |> List.map ~f:(fun (x,y) -> y) |> List.hd in
    let get_id = List.filter creds_list ~f:(fun (x,y) -> x="user_id")
                 |> List.map ~f:(fun (x,y) -> y) |> List.hd in
    (get_key, get_id)
  let from_file path =
    let exception Invalid_Credentials in
    let cred_tup = load_file path in
    match cred_tup with
      | (Some k, Some id) -> {key=k; userid=id}
      | _ -> raise Invalid_Credentials
end

module Quil : sig
  type gate =
    | I of int
    | PHASE of float * int
    | X of int
    | RX of float * int
    | Y of int
    | RY of float * int
    | Z of int
    | RZ of float * int
    | H of int
    | CNOT of int * int
    | CIRCUIT of gate list

  val to_string : gate -> string

  val ( @ ) : gate -> gate -> gate
end = struct
  type gate =
    | I of int
    | PHASE of float * int
    | X of int
    | RX of float * int
    | Y of int
    | RY of float * int
    | Z of int
    | RZ of float * int
    | H of int
    | CNOT of int * int
    | CIRCUIT of gate list

  let ( @ ) a b = match (a,b) with
    | (CIRCUIT (gl), CIRCUIT (gr)) -> CIRCUIT (gl @ gr)
    | (CIRCUIT (gl), g) -> CIRCUIT (gl @ [g])
    | (g, CIRCUIT (gr)) -> CIRCUIT ([g] @ gr)
    | (a, b) -> CIRCUIT [a; b]

  let rec to_string g = match g with
    | I(x) -> ["I ";  string_of_int x; "\n"] |> String.concat
    | H(x) -> ["H ";  string_of_int x; "\n"] |> String.concat
    | X(x) -> ["X ";  string_of_int x; "\n"] |> String.concat
    | Y(x) -> ["Y ";  string_of_int x; "\n"] |> String.concat
    | Z(x) -> ["Z ";  string_of_int x; "\n"] |> String.concat
    | PHASE(f, x) -> ["PHASE (";  string_of_float f; ") "; string_of_int x; "\n"] |> String.concat
    | RX(f, x) -> ["RX (";  string_of_float f; ") "; string_of_int x; "\n"] |> String.concat
    | RY(f, x) -> ["RY (";  string_of_float f; ") "; string_of_int x; "\n"] |> String.concat
    | RZ(f, x) -> ["RZ (";  string_of_float f; ") "; string_of_int x; "\n"] |> String.concat
    | CNOT(x, y) -> ["CNOT ";  string_of_int x;
                     " "; string_of_int y; "\n"] |> String.concat
    | CIRCUIT (hd::tl) -> [to_string hd; to_string(CIRCUIT(tl))] |> String.concat
    | CIRCUIT [] -> ""
end

module Pauli : sig
  type pauli =
    | I of float * int
    | SX of float * int
    | SY of float * int
    | SZ of float * int
    | ISING of float * int * int
    | P of pauli list
    | S of pauli list

  val exponentiate : pauli -> Quil.gate

  val ( + ) : pauli -> pauli -> pauli
  val ( $* ): float -> pauli -> pauli
end = struct
  type pauli =
    | I of float * int
    | SX of float * int
    | SY of float * int
    | SZ of float * int
    | ISING of float * int * int
    | P of pauli list
    | S of pauli list

  let ( + ) a b = match (a, b) with
    | (S x, S y) -> S (x @ y)
    | (S x, y) -> S (x @ [y])
    | (x, S y) -> S ([x] @ y)
    | (x, y) -> S [x; y]

  let rec ( $* ) f p = match p with
    | I (t, i) -> I (t *. f, i)
    | ISING (w, i, j) -> ISING (f *. w, i, j)
    | SZ (w, i) -> SZ (f *. w, i)
    | SY (w, i) -> SY (f *. w, i)
    | SX (w, i) -> SX (f *. w, i)
    | P [] -> P []
    | P [x] -> f $* x
    | P (hd :: tl) -> P ( [(f $* hd)] @ tl )
    | S [] -> S []
    | S [x] -> f $* x
    | S (hd :: tl) -> (f $* hd) + ( f $* (S tl))

  let exponentiate p =
    let exception NOT_IMPLEMENTED of string in
    match p with
    | I(t, i) -> Quil.PHASE(t, i)
    | SX(t, i) -> Quil.RX(2.0 *. t, i)
    | SY(t, i) -> Quil.RY(2.0 *. t, i)
    | SZ(t, i) -> Quil.RZ(2.0 *. t, i)
    | ISING(t, i, j) -> Quil.CIRCUIT [Quil.CNOT (i, j); Quil.RZ(2.0 *. t, j); Quil.CNOT (i, j)]
    | P _ -> raise (NOT_IMPLEMENTED "exponentiate of Pauli Product not implemented")
    | S _ -> raise (NOT_IMPLEMENTED "exponentiate of Pauli Sum not implemented")
end


module G = Imperative.Graph.ConcreteLabeled(
  struct
    type t = int
    let equal a b = phys_equal a b
    let compare a b = if a <= b then a else b
    let hash a = a
  end
)(
  struct
    type t = float
    let compare a b = if a <= b then -1 else 0
    let default = 0.0
  end
)

let create nodes edges =
  let gr = G.create () in
  let create_edge t =
    match t with
    | (a, b, w) -> G.E.create a w b in
  let g_edges = List.map edges ~f:create_edge in
  List.iter nodes ~f:(G.add_vertex gr);
  List.iter g_edges ~f:(G.add_edge_e gr);
  gr

(** QAOA Reward Hamiltonian as a Circuit *)
let get_qaoa_reward_circuit gamma graph =
  let add edge circuit =
    Quil.(circuit @ Pauli.(exponentiate (ISING (gamma *. (G.E.label edge), G.E.src edge, G.E.dst edge)))) in
  G.fold_edges_e add graph (Quil.CIRCUIT [])


(** QAOA Mixer Circuit *)
let get_qaoa_mixer_circuit beta graph =
  let add vertex circuit =
    Quil.(circuit @ Pauli.(exponentiate (SX (beta, vertex)))) in
  G.fold_vertex add graph (Quil.CIRCUIT [])

(** QAOA Ansatx Circuit *)
let get_qaoa_ansatz graph =
  let add vertex circuit =
    Quil.(circuit @ (H vertex)) in
  G.fold_vertex add graph (Quil.CIRCUIT [])

let calculate_maxcut_reward g bs =
  let arr = Array.of_list bs in
  let add_weight edge accum =
    let src = G.E.src edge in
    let dst = G.E.dst edge in
    if arr.(src) = arr.(dst) then accum +. 0.0
    else accum +. (G.E.label edge) in
  (* Multiply by 1/2 due to edge double counting *)
  0.5 *. G.fold_edges_e add_weight g 0.0

let calculate_maxcut_avg g bs =
  List.map bs ~f:(calculate_maxcut_reward g)
    |> Array.of_list
    |> Owl.Stats.mean

module MultishotMeasurePayload : sig
  type t
  val to_payload : t -> Cohttp_lwt.Body.t
  val create : int list -> int -> Quil.gate -> t
  val quilBody : Cohttp_lwt.Body.t -> Safe.json Lwt.t
end = struct
  type t = {
      measurement_type: string;
      qubits: int list;
      trials: int;
      quil_instructions: Quil.gate
    }

  let create qubits trials prog =
    {
      measurement_type = meas_type_to_string TYPE_MULTISHOT_MEASURE;
      qubits = qubits;
      trials = trials;
      quil_instructions = prog
    }
  let to_json t =
    `Assoc [
        ("type", `String t.measurement_type);
        ("qubits", `List (List.map t.qubits ~f:(fun q -> `Int q)));
        ("trials", `Int t.trials);
        ("compiled-quil", `String (t.quil_instructions |> Quil.to_string))
      ]
  let to_payload t = Cohttp_lwt.Body.of_string (to_json t |> Yojson.Safe.to_string)

  let quilBody payload =
    let header =
      let open Creds in
      let creds = Creds.from_file "/Users/johannes/.pyquil_config" in
      Header.init ()
        |> fun h -> Header.add h "x-api-key" creds.key
        |> fun h -> Header.add h "x-user-id" creds.userid
        |> fun h -> Header.add h "Content-Type" "application/json; charset=utf-8" in
    Client.post ~body:payload ~headers:header uri >>= fun (resp, body) ->
  (*let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);*)
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
  (*Printf.printf "Body of length: %d\n" (String.length body);
  Printf.printf "Body: %s\n" body;*)
    Yojson.Safe.from_string body

end


let run_req trials gate qbts =
  let parse_qvm resp =
    let rec parse smpls = match smpls with
      | hd::tl -> (hd |> Yojson.Safe.Util.to_list
                      |> List.map ~f:(fun x -> x |> Yojson.Safe.Util.to_int)
                  )::(parse tl)
      | [] -> [] in
    parse (Yojson.Safe.Util.to_list resp) in
  Lwt_main.run (MultishotMeasurePayload.create qbts trials gate
                |> MultishotMeasurePayload.to_payload
                |> MultishotMeasurePayload.quilBody
    ) |> parse_qvm

let qaoa_eval g ~inp:param =
  let qbts = G.fold_vertex (fun v a -> a @ [v]) g [] in
  let circ = Quil.((get_qaoa_ansatz g)
                   @ (get_qaoa_reward_circuit param.(0) g)
                   @ (get_qaoa_mixer_circuit param.(1) g)) in
  run_req 1000 circ qbts |> calculate_maxcut_avg g

let simple_test () =
  let g = create [0; 1; 2] [(1, 2, 3.0); (2, 0, 2.0); (0, 1, 1.0)] in
  let circ = Quil.((get_qaoa_ansatz g)
                   @ (get_qaoa_reward_circuit 0.1 g)
                   @ (get_qaoa_mixer_circuit 0.2 g)) in
  run_req 1000 circ [1;2;3] |> calculate_maxcut_avg g

let simple_full_loop () =
    let g = create [0; 1; 2] [(1, 2, 3.0); (2, 0, 2.0); (0, 1, 1.0)] in
    EvoStrat.optimize ~f:(qaoa_eval g) ~lr:0.00001 ~bs:10 ~scale:0.00001 ~epochs:20 ~x0:[|-0.2; 1.03|
