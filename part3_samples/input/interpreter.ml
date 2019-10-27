(* Author: Mohammed Samsuddin 
   Program: interpreter.ml 
   Version: 3.0
   Date: 03 - 25 - 2019
   Description: 
   Execution: ocamlc -o interpreter interpreter.ml
   			      ./interpreter
*)

(* Data type to represent Values and commands *)

type values = B of string | S of string | I of int | N of string | UNIT of string | ERROR of string
              | CLOSURE of (values * (commands list) * (values * values) list list) 

and commands = Pushi of values | Pushb of values | Pushn of values | Pushs of values | Push of values | 
				        Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Cat | And | Or | Not | LessThan | Equal | If |
                Bind | Let | End | Call | Return | FunEnd | Function of values * values | InOutFun of values * values | Quit


(* The Main function *)
let interpreter ((input :  string), (output :  string )) :  unit = 

	(* Here we open an input channel for first argument, input, 
     and bind it to a variable ic so that we can refer it 
     later *)
  let ic = open_in input in

	(* Use the second argument as file name to open an output channel,
	   and bind it to variable oc for later reference. *)
	let oc = open_out output in 


	(* Helper function: file input function. that parses the file and generates a command list 
	   It reads file line by line and return the result as a list of string.  *)
  	let rec loop_read commandList =
      (* We use try with to catch the End_of_file exception. *)
      try 
          (* Read a line from ic. Build a new list with l::acc
             and pass to next recursive call. *)
          let l = input_line ic in
          if String.contains l '"' then 
            let s1 = (String.trim (List.hd (String.split_on_char '"'l))) in
            let s2 = "S" ^ (List.nth (String.split_on_char '"'l) 1) in
            loop_read (commandList @ (s1::s2::[]))

          else
            loop_read (commandList @ (String.split_on_char ' ' l))

      with
        (* At the end of file, we will reverse the string since
           the list is building by keeping add new element at the 
           head of old list. *)
      | End_of_file -> commandList in

    (* Helper function: file output function. It writes the final stack value to the output file. *)
    let file_write output_value = Printf.fprintf oc "%s\n" output_value in

    (* check if digit *)
    let is_digit = function '0' .. '9' -> true | _ -> false in

    (* Get the Value associated with name *)
    let rec getValue (name : values) (envList : (values * values) list list) : values = 
      match envList with
      | [] -> name
      | hd::tl -> if (List.mem_assoc name hd) then (List.assoc name hd) else getValue name tl in


	  (* This variable contains the result of input file from helper 
       function, loop_read. Please remember this is a list of string. *)
  	let ls_str = loop_read [] in

    (* List of all the commands of our interpreter *)
    let listOfCommands = ["pushi"; "pushb" ; "pushs" ; "pushn" ; "push" ; "add" ; "sub" ; "mul" ; "div" ; "rem" ; "neg" ; "swap" ; "pop" ; "cat" ; "and" ; "or" ; "not" ; "equal" ; "if" ; "lessThan" ; "bind" ; "let" ; "end" ; "call" ; "return" ; "funEnd" ; "fun" ; "inOutFun" ; "quit"] in

     (* Take a list of string commands and converted to list of commands of type commands *)
  	 let rec string_to_cmdList (myList : string list) (cmdList : commands list) = 
  	 	match myList with
      | [] -> List.rev cmdList
      | "pushi"::y::xs -> string_to_cmdList xs (try (Pushi (I (int_of_string y)) :: cmdList) with Failure _ -> (Push  (ERROR (":error:")) :: cmdList))
      | "pushb"::y::xs -> string_to_cmdList xs (if y = ":true:" then (Pushb (B y) :: cmdList) else if y = ":false:" then (Pushb (B y) :: cmdList) else (Push  (ERROR (":error:")) :: cmdList))
      | "pushs"::y::xs -> string_to_cmdList xs (try (if (String.contains_from y 0 'S') then (Pushs (S (String.sub y 1 (String.length y-1))) :: cmdList) else (Push  (ERROR (":error:")) :: cmdList)) with Failure _ -> (Push  (ERROR (":error:")) :: cmdList))
      | "pushn"::y::xs -> string_to_cmdList xs (if is_digit y.[0] then (Push  (ERROR (":error:")) :: cmdList) else (Pushn (N y) :: cmdList))
      | "push"::y::xs  -> string_to_cmdList xs (if y = ":unit:" then (Push  (UNIT y) :: cmdList) else (Push  (ERROR (":error:")):: cmdList))
      | "add"::xs -> string_to_cmdList xs (Add :: cmdList) 
      | "sub"::xs -> string_to_cmdList xs (Sub :: cmdList) 
      | "mul"::xs -> string_to_cmdList xs (Mul :: cmdList) 
      | "div"::xs -> string_to_cmdList xs (Div :: cmdList) 
      | "rem"::xs -> string_to_cmdList xs (Rem :: cmdList) 
      | "neg"::xs -> string_to_cmdList xs (Neg :: cmdList) 
      | "swap"::xs -> string_to_cmdList xs (Swap :: cmdList) 
      | "pop"::xs -> string_to_cmdList xs (Pop :: cmdList) 
      | "cat"::xs -> string_to_cmdList xs (Cat :: cmdList) 
      | "and"::xs -> string_to_cmdList xs (And :: cmdList) 
      | "or"::xs -> string_to_cmdList xs (Or :: cmdList) 
      | "not"::xs -> string_to_cmdList xs (Not :: cmdList) 
      | "equal"::xs -> string_to_cmdList xs (Equal :: cmdList) 
      | "if"::xs -> string_to_cmdList xs (If :: cmdList) 
      | "lessThan"::xs -> string_to_cmdList xs (LessThan :: cmdList)
      | "bind"::xs -> string_to_cmdList xs (Bind :: cmdList) 
      | "let"::xs -> string_to_cmdList xs (Let :: cmdList)  
      | "end"::xs -> string_to_cmdList xs (End :: cmdList)
      | "call"::xs -> string_to_cmdList xs (Call :: cmdList) 
      | "return"::xs -> string_to_cmdList xs (Return :: cmdList)
      | "funEnd"::xs -> string_to_cmdList xs (FunEnd :: cmdList)
      | "fun"::x::y::xs -> string_to_cmdList xs (if ((x=y) && ((is_digit y.[0]) && (is_digit x.[0])) && ((List.mem x listOfCommands) && (List.mem y listOfCommands))) then (Push  (ERROR (":error:")) :: cmdList) else ((Function (N x, N y))::cmdList))
      | "inOutFun"::x::y::xs -> string_to_cmdList xs (if ((x=y) && ((is_digit y.[0]) && (is_digit x.[0])) && ((List.mem x listOfCommands) && (List.mem y listOfCommands))) then (Push  (ERROR (":error:")) :: cmdList) else ((InOutFun (N x, N y))::cmdList))
      | "quit"::xs -> string_to_cmdList xs (Quit :: cmdList)
      | _::xs -> string_to_cmdList xs (Push  (ERROR (":error:")) :: cmdList) in
      

  	 (* Takes two argument a list of commands and a stack and produces a new stack  *)
  	 let rec evaluate (commandList : commands list) (listOfStack : values list list) (nameValuelist : (values * values) list list list) : values list = 
  	 	match (commandList, listOfStack, nameValuelist) with
  	 	| (Pushi x::rest, hd::listOfStack, nameValuelist) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
  	 	| (Pushs x::rest, hd::listOfStack, nameValuelist) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
  	 	| (Pushb x::rest, hd::listOfStack, nameValuelist) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
  	 	| (Pushn x::rest, hd::listOfStack, nameValuelist) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
  	 	| (Push  x::rest, hd::listOfStack, nameValuelist) -> evaluate rest ((x::hd)::listOfStack) nameValuelist

  	 	| (Add::rest, (I x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest ( (I(x+y)::hd)::listOfStack) nameValuelist
      | (Add::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with | I a, I b ->((I(a+b)::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack)))  nameValuelist
      | (Add::rest, (N x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with | I a->((I(a+y)::hd)::listOfStack) |_->  ((ERROR ":error:")::N x::I y::hd)::listOfStack))) nameValuelist
      | (Add::rest, (I x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with | I a->((I(a+x)::hd)::listOfStack) |_->  ((ERROR ":error:")::I x::N y::hd)::listOfStack)))nameValuelist


      | (Sub::rest, (I y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((I(x-y)::hd)::listOfStack) nameValuelist
      | (Sub::rest, (N y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with | I a, I b ->((I(a-b)::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N y::N x::hd)::listOfStack)))  nameValuelist  
      | (Sub::rest, (N y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with | I a ->((I(x-a)::hd)::listOfStack) |_-> ((ERROR ":error:")::N y::I x::hd)::listOfStack)))  nameValuelist
      | (Sub::rest, (I y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with | I a ->((I(a-y)::hd)::listOfStack) |_-> ((ERROR ":error:")::I y::N x::hd)::listOfStack))) nameValuelist
      

      | (Mul::rest, (I y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((I(x*y)::hd)::listOfStack) nameValuelist
      | (Mul::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with | I a, I b ->((I(a*b)::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack)))  nameValuelist
      | (Mul::rest, (N x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with | I a ->((I(a*y)::hd)::listOfStack) |_-> ((ERROR ":error:")::N x::I y::hd)::listOfStack)))  nameValuelist
      | (Mul::rest, (I x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with | I a ->((I(x*a)::hd)::listOfStack) |_-> ((ERROR ":error:")::I x::N y::hd)::listOfStack)))  nameValuelist


      | (Div::rest, (I (0)::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((ERROR (":error:")::I(0)::I x::hd)::listOfStack) nameValuelist
      | (Div::rest, (I (0)::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((ERROR (":error:")::I(0)::N x::hd)::listOfStack) nameValuelist
      | (Div::rest, (I y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((I(x/y)::hd)::listOfStack) nameValuelist 
      | (Div::rest, (N y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |I a,I 0 -> ((((ERROR ":error:")::N y::N x::hd)::listOfStack)) | I a, I b ->((I(a/b)::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N y::N x::hd)::listOfStack)))  nameValuelist
      | (Div::rest, (N y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with |I 0 -> ((((ERROR ":error:")::N y::I x::hd)::listOfStack))| I b-> ((I(x/b)::hd)::listOfStack) |_-> ((ERROR ":error:")::N y::I x::hd)::listOfStack)))   nameValuelist
      | (Div::rest, (I y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |I a -> ((I(a/y)::hd)::listOfStack) |_-> ((ERROR ":error:")::I y::N x::hd)::listOfStack)))  nameValuelist
      

      | (Rem::rest, (I (0)::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((ERROR (":error:")::I(0)::I x::hd)::listOfStack) nameValuelist
      | (Rem::rest, (I (0)::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((ERROR (":error:")::I(0)::N x::hd)::listOfStack) nameValuelist
      | (Rem::rest, (I y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest ((I(x mod y)::hd)::listOfStack) nameValuelist
      | (Rem::rest, (N y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |I a,I 0 ->(((((ERROR ":error:")::N y::N x::hd)::listOfStack)))   | I a, I b ->((I(a mod b)::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N y::N x::hd)::listOfStack)))  nameValuelist
      | (Rem::rest, (N y::I x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with |I 0 -> ((((ERROR ":error:")::N y::I x::hd)::listOfStack))| I b-> ((I(x mod b)::hd)::listOfStack) |_-> ((ERROR ":error:")::N y::I x::hd)::listOfStack)))  nameValuelist
      | (Rem::rest, (I y::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |I a -> ((I(a mod y)::hd)::listOfStack) |_-> ((ERROR ":error:")::I y::N x::hd)::listOfStack)))  nameValuelist   
      
      | (Neg::rest, (I x::hd)::listOfStack, nameValuelist) ->       evaluate rest ((I(-x)::hd)::listOfStack) nameValuelist
      | (Neg::rest, (N x::hd)::listOfStack, nameValuelist) ->       evaluate rest (((match (getValue (N x) nameValuelist) with | I a ->((I(-a)::hd)::listOfStack) |_->((ERROR":error:")::N x::hd)::listOfStack))) nameValuelist 
   

      | (Swap::rest, (x::y::hd)::listOfStack, nameValuelist) ->     evaluate rest (((y)::(x)::hd)::listOfStack) nameValuelist

      | (Pop::rest, (x::hd)::listOfStack, nameValuelist) ->        evaluate rest (hd::listOfStack) nameValuelist
      
      | (Cat::rest, (S x::S y::hd)::listOfStack, nameValuelist) ->  evaluate rest ((S(y^x)::hd)::listOfStack) nameValuelist
      | (Cat::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |S a, S b ->((S(b^a)::hd)::listOfStack) |_,_ -> ((ERROR ":error:")::S x::S y::hd)::listOfStack))) nameValuelist
      | (Cat::rest, (N x::S y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match(getValue (N x) nameValuelist) with |S a-> ((S(y^a)::hd)::listOfStack) |_ -> ((ERROR ":error:")::N x::S y::hd)::listOfStack))) nameValuelist
      | (Cat::rest, (S x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match(getValue (N y) nameValuelist) with |S a-> ((S(a^x)::hd)::listOfStack) |_ -> ((ERROR ":error:")::S x::N y::hd)::listOfStack))) nameValuelist
      
      | (And::rest, (B (":true:")::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":true:")::hd)::listOfStack) nameValuelist
      | (And::rest, (B (":false:")::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":false:")::hd)::listOfStack) nameValuelist
      | (And::rest, (B (":true:")::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":false:")::hd)::listOfStack) nameValuelist
      | (And::rest, (B (":false:")::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":false:")::hd)::listOfStack) nameValuelist


      | (And::rest, (N x::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with | B ":true:" -> ((B (":true:")::hd)::listOfStack) |B ":false:" -> ((B (":false:")::hd)::listOfStack) |_-> ((ERROR ":error:")::N x::B (":true:")::hd)::listOfStack)))  nameValuelist     
      | (And::rest, (N x::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |B b ->((B (":false:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::N x::B (":false:")::hd)::listOfStack)))  nameValuelist     
      | (And::rest, (B (":true:")::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with | B ":true:" -> ((B (":true:")::hd)::listOfStack) |B ":false:" -> ((B (":false:")::hd)::listOfStack) |_-> ((ERROR ":error:")::B (":true:")::N x::hd)::listOfStack)))  nameValuelist     
      | (And::rest, (B (":false:")::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |B b ->((B (":false:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::B (":false:")::N x::hd)::listOfStack)))  nameValuelist     
      
      | (And::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |B ":true:", B ":true:" ->((B (":true:")::hd)::listOfStack) |B a, B b ->((B (":false:")::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack)))  nameValuelist


      | (Or::rest, (B (":true:")::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":true:")::hd)::listOfStack) nameValuelist
      | (Or::rest, (B (":false:")::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":true:")::hd)::listOfStack) nameValuelist
      | (Or::rest, (B (":true:")::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":true:")::hd)::listOfStack) nameValuelist
      | (Or::rest, (B (":false:")::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest ((B(":false:")::hd)::listOfStack) nameValuelist
      
      | (Or::rest, (N x::B (":true:")::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |B b -> ((B (":true:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::N x::B (":true:")::hd)::listOfStack)))  nameValuelist
      | (Or::rest, (N x::B (":false:")::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist)with |B ":true:" ->((B (":true:")::hd)::listOfStack) | B ":false:" -> ((B (":false:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::N x::B (":false:")::hd)::listOfStack)))  nameValuelist     
      
      | (Or::rest, (B (":true:")::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist)with |B ":true:" ->((B (":true:")::hd)::listOfStack) | B ":false:" -> ((B (":true:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::B (":true:")::N x::hd)::listOfStack)))  nameValuelist
      | (Or::rest, (B (":false:")::N x::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist)with |B ":true:" ->((B (":true:")::hd)::listOfStack) | B ":false:" -> ((B (":false:")::hd)::listOfStack) |_ -> ((ERROR ":error:")::B (":false:")::N x::hd)::listOfStack)))  nameValuelist

      | (Or::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |B ":false:", B ":false:" ->((B (":false:")::hd)::listOfStack) |B a, B b ->((B (":true:")::hd)::listOfStack) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack))) nameValuelist

      
      | (Not::rest, (B (":true:")::hd)::listOfStack, nameValuelist) ->       evaluate rest ((B(":false:")::hd)::listOfStack) nameValuelist
      | (Not::rest, (B (":false:")::hd)::listOfStack, nameValuelist) ->       evaluate rest ((B(":true:")::hd)::listOfStack) nameValuelist
      | (Not::rest, (N x::hd)::listOfStack, nameValuelist) ->    evaluate rest (((match (getValue (N x) nameValuelist) with |B ":true:" ->((B (":false:")::hd)::listOfStack) |B ":false:" ->((B (":true:")::hd)::listOfStack) |_-> ((ERROR ":error:")::N x::hd)::listOfStack))) nameValuelist

      | (Equal::rest, (I x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (if x==y then ((B(":true:")::hd)::listOfStack) else ((B(":false:")::hd)::listOfStack)) nameValuelist
      | (Equal::rest, (N x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |I i -> if (i=y) then (((B (":true:")::hd)::listOfStack)) else (((B (":false:")::hd)::listOfStack)) |_-> ((ERROR ":error:")::N x::I y::hd)::listOfStack))) nameValuelist
      | (Equal::rest, (I x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with |I i -> if (i=x) then (((B (":true:")::hd)::listOfStack)) else (((B (":false:")::hd)::listOfStack)) |_-> ((ERROR ":error:")::I x::N y::hd)::listOfStack)))  nameValuelist
      | (Equal::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |I a, I b -> if (a=b) then (((B(":true:")::hd)::listOfStack)) else (((B (":false:")::hd)::listOfStack)) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack))) nameValuelist


      | (LessThan::rest, (I x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (if y<x then ((B(":true:")::hd)::listOfStack) else ((B(":false:")::hd)::listOfStack)) nameValuelist
      | (LessThan::rest, (N x::I y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N x) nameValuelist) with |I i -> if (y<i) then (((B(":true:")::hd)::listOfStack)) else (((B (":false:")::hd)::listOfStack)) |_-> ((ERROR ":error:")::N x::I y::hd)::listOfStack))) nameValuelist
      | (LessThan::rest, (I x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N y) nameValuelist) with |I i -> if (i<x) then (((B (":true:")::hd)::listOfStack)) else (((B(":false:")::hd)::listOfStack)) |_-> ((ERROR ":error:")::I x::N y::hd)::listOfStack))) nameValuelist
      | (LessThan::rest, (N x::N y::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |I a, I b ->if (b<a) then (((B (":true:")::hd)::listOfStack)) else (((B(":false:")::hd)::listOfStack)) |_,_-> ((ERROR ":error:")::N x::N y::hd)::listOfStack))) nameValuelist


      | (Bind::rest, (I x::N y::hd)::listOfStack, hd2::nameValuelist) ->  evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y, I x)::hd2)::nameValuelist)
      | (Bind::rest, (B x::N y::hd)::listOfStack, hd2::nameValuelist) ->  evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y, B x)::hd2)::nameValuelist)
      | (Bind::rest, (S x::N y::hd)::listOfStack, hd2::nameValuelist) ->  evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y, S x)::hd2)::nameValuelist)
      | (Bind::rest, (N x::N y::hd)::listOfStack, nameValuelist) -> begin 
                                                                        match ((getValue (N x) nameValuelist), nameValuelist) with
                                                                          |(I a, hd2::nameValuelist) -> (evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y,I a)::hd2)::nameValuelist)) 
                                                                          |(B a, hd2::nameValuelist) -> (evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y,B a)::hd2)::nameValuelist)) 
                                                                          |(S a, hd2::nameValuelist) -> (evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y,S a)::hd2)::nameValuelist)) 
                                                                          |(UNIT a, hd2::nameValuelist) -> (evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y,UNIT a)::hd2)::nameValuelist)) 
                                                                          |(_, _) -> ((evaluate rest ((ERROR(":error:")::N x::N y::hd)::listOfStack)) nameValuelist)
                                                                        end

      | (Bind::rest, (UNIT x::N y::hd)::listOfStack, hd2::nameValuelist) ->  evaluate rest ((UNIT(":unit:")::hd)::listOfStack) (((N y, UNIT x)::hd2)::nameValuelist)

      
      | (If::rest, (x::y::B z::hd)::listOfStack, nameValuelist) ->  evaluate rest (if z=":true:" then (((x)::hd)::listOfStack) else (((y)::hd)::listOfStack)) nameValuelist
      | (If::rest, (x::y::N z::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match (getValue (N z) nameValuelist) with |B ":true:" -> ((((x)::hd)::listOfStack)) |B ":false:" -> ((((y)::hd)::listOfStack)) |_-> ((ERROR ":error:")::x::y::N z::hd)::listOfStack))) nameValuelist
      | (If::rest, (N x::N y::N z::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N z) nameValuelist), (getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with |B ":true:", a, c -> ((((a)::hd)::listOfStack)) |B ":false:", a, c-> ((((c)::hd)::listOfStack)) |_,_,_ -> ((ERROR ":error:")::N x::N y::N z::hd)::listOfStack))) nameValuelist
      | (If::rest, (N x:: y::N z::hd)::listOfStack, nameValuelist) ->  evaluate rest  (((match ((getValue (N z) nameValuelist), (getValue (N x) nameValuelist)) with |B ":true:", a ->((((a)::hd)::listOfStack)) |B ":false:", c ->((((y)::hd)::listOfStack)) |_,_ -> ((ERROR ":error:")::N x:: y::N z::hd)::listOfStack))) nameValuelist
      | (If::rest, (x::N y::N z::hd)::listOfStack, nameValuelist) ->  evaluate rest   (((match ((getValue (N z) nameValuelist), (getValue (N y) nameValuelist))with |B ":true:", a -> ((((x)::hd)::listOfStack)) |B ":false:", a->((((a)::hd)::listOfStack)) |_,_ -> ((ERROR ":error:"):: x::N y::N z::hd)::listOfStack))) nameValuelist
      
      | (If::rest, (N x::N y::B z::hd)::listOfStack, nameValuelist) ->  evaluate rest (((match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist), (B z)) with |a, c, B(":true:") -> ((((a)::hd)::listOfStack))  |a, c, B(":false:")-> (((c)::hd)::listOfStack) |_,_,_ -> ((ERROR ":error:")::N x::N y::B z::hd)::listOfStack))) nameValuelist
      | (If::rest, (N x:: y::B z::hd)::listOfStack, nameValuelist) ->  evaluate rest  (((match ((getValue (N x) nameValuelist), (B z)) with |a, B ":true:" ->((((a)::hd)::listOfStack)) |a, B ":false:" ->((((y)::hd)::listOfStack)) |_,_ -> ((ERROR ":error:")::N x:: y::B z::hd)::listOfStack))) nameValuelist
      | (If::rest, (x::N y::B z::hd)::listOfStack, nameValuelist) ->  evaluate rest   (((match ((getValue (N y) nameValuelist), (B z)) with |a, B ":true:" -> ((((x)::hd)::listOfStack)) |a, B ":false:"->((((a)::hd)::listOfStack)) |_,_ -> ((ERROR ":error:"):: x::N y::B z::hd)::listOfStack))) nameValuelist
      
 	 	
      | (Let::rest, listOfStack, nameValuelist) -> evaluate rest ([]::listOfStack) ([]::nameValuelist)
      | (End::rest, hd::listOfStack, hd2::nameValuelist) -> begin 
                                        match (hd, listOfStack) with
                                        | ([], listOfStack) -> evaluate rest (listOfStack) nameValuelist
                                        | (x::tl, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
                                        | (_, _) -> evaluate rest (listOfStack) nameValuelist
                                      end



      (*| (Call::rest, (x::N y::hd)::listOfStack, nameValuelist) -> (match ()) 
      | (Call::rest, (N x::N y::hd)::listOfStack, nameValuelist) -> (match ((getValue (N x) nameValuelist), (getValue (N y) nameValuelist)) with  )   
      | (Return::rest, listOfStack, nameValuelist) -> 
      | (FunEnd::rest, listOfStack, nameValuelist) -> 
      | (Function x y::rest, listOfStack, hd2::nameValuelist) -> evaluate rest (listOfStack) (((x, CLOSURE [[((N, y), [], () ))]; ])::hd2)::nameValuelist)
      | (InOutFun x y::rest, listOfStack, nameValuelist) -> evaluate rest (listOfStack) *)



      | (Quit::rest, hd::listOfStack, nameValuelist) -> hd
      | (_::rest ,   hd::listOfStack, nameValuelist) -> evaluate rest (((ERROR (":error:"))::hd)::listOfStack) nameValuelist
      | (_ , _, _) -> evaluate commandList listOfStack nameValuelist in

  (* Produce a List of Commands *)
  let cmdList = string_to_cmdList ls_str [] in

  (* Produce a Stack of values *)
  let newStack = evaluate cmdList [[]] [[]] in

  (* Takes an stack of values and create a string of values to write to output file *)
  let rec to_Stringlist (l: values list) (stringlist : string list) = 
      match l with
      | [] -> List.rev stringlist
      | I i ::rest-> to_Stringlist rest ((string_of_int i)::stringlist)
      | B b ::rest-> to_Stringlist rest (b::stringlist)
      | S s ::rest-> to_Stringlist rest (s::stringlist)
      | N n ::rest-> to_Stringlist rest (n::stringlist)
      | ERROR e ::rest-> to_Stringlist rest (e::stringlist)
      | UNIT u ::rest-> to_Stringlist rest (u::stringlist) in      


  let finalList =  to_Stringlist newStack [] in

    (* Write to output File *)
    let rec write_to_OutputFile l = 
      match l with 
      | [] -> print_endline ""
      | head::body -> 
        begin
          file_write head;
          write_to_OutputFile body
          end in
      
    write_to_OutputFile finalList;; 

(* Do not include this in your submission but use this line to test your code *)
(* Call the function interpreter with two args *)
interpreter (("input1.txt"), ("output1.txt"))