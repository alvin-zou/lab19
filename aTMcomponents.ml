(* Customer account identifiers *)
type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} 

let database : account_spec list ref = ref [] 

let initialize (actlist : account_spec list) : unit =
  database := actlist
  
let acquire_id : unit -> id = 
  fun () -> (
    print_string "Enter customer id: ";
    read_int ())

  (* acquire_amount () -- Requests from the ATM customer and returns an
     amount by prompting for an amount and reading an int from stdin. *)
let acquire_amount : unit -> int = 
  fun () -> (
    print_string "Enter amount: ";
    read_int ())
  
  (* acquire_act () -- Requests from the user and returns an action to
     be performed, as a value of type action *)
let acquire_act : unit -> action = 
  fun () -> (
    print_string "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
    match read_line () with
    | "B" -> Balance
    | "-" -> Withdraw (acquire_amount ())
    | "+" -> Deposit (acquire_amount ())
    | "=" -> Next 
    | "X" -> Finished
    | _ -> raise (Invalid_argument "Invalid action."))
  
  (*....................................................................
    Querying and updating the account database
  
    These functions all raise Not_found if there is no account with the
    given id. 
   *)
    
  (* get_balance id -- Returns the balance for the customer account with
     the given id. *)
let get_balance (i : id) : int = 
  match (List.filter (fun x -> x.id = i) !database) with
  | [] -> raise (Not_found)
  | hd :: _ -> hd.balance
  
  (* get_name id -- Returns the name associated with the customer
     account with the given id. *)
let get_name (i : id) : string = 
  match (List.filter (fun x -> x.id = i) !database) with
  | [] -> raise (Not_found)
  | hd :: _ -> hd.name
  
  (* update_balance id amount -- Modifies the balance of the customer
     account with the given id,setting it to the given amount. *)
let rec update_balance (i : id) (amt : int) : unit = 
  let helper (act : account_spec) : account_spec = 
    if act.id = i 
    then { name = act.name; id = act.id; balance = amt }
    else act in
  database := (List.map helper !database)
  
  (*....................................................................
    Presenting information and cash to the customer
   *)
    
  (* present_message message -- Presents to the customer (on stdout) the
     given message followed by a newline. *)
let present_message (s : string) : unit = print_string (s ^ "\n")
  
  (* deliver_cash amount -- Dispenses the given amount of cash to the
     customer (really just prints to stdout a message to that
     effect). *)
let deliver_cash (amt : int) : unit = 
  let str = ref "" in
  let bill = "[20 @ 20]" in
  let rem = ref amt in
  let _ = while !rem >= 20 
    do
      str := !str ^ bill;
      rem := !rem - 20
    done in
  let _ = (str := !str ^ (" and " ^ string_of_int !rem ^ " more")) in
  present_message !str
  
