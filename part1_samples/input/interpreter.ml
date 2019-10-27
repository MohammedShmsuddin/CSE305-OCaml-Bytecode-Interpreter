(* Author: Mohammed Samsuddin 
   Program: interpreter.ml 
   Version: Part 1
   Date: 03 - 25 - 2019
   Description: 
   Execution: ocamlc -o interpreter interpreter.ml
   			  ./interpreter
*)

(* Data type to represent Values and commands *)

type values = B of string | S of string | I of int | N of string | UNIT of string | ERROR of string
type commands = Pushi of values | Pushb of values | Pushn of values | Pushs of values | Push of values | 
				Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Cat | And | Or | Not | LessThan | Equal | If |
        Bind | Quit

(* The Main function *)
let interpreter ( (input :  string), (output :  string )) :  unit = 

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

    (* check if  digit *)
    let is_digit = function '0' .. '9' -> true | _ -> false in

	  (* This variable contains the result of input file from helper 
       function, loop_read. Please remember this is a list of string. *)
  	let ls_str = loop_read [] in


     (* Take a list of string commands and converted to list of commands of type commands *)
  	 let rec string_to_cmdList (myList : string list) (cmdList : commands list) = 
  	 	match myList with
      | [] -> List.rev cmdList
      | "pushi"::y::xs -> string_to_cmdList xs (try (Pushi (I (int_of_string y)) :: cmdList) with Failure _ -> (Push  (ERROR (":error:")) :: cmdList))
      | "pushb"::y::xs -> string_to_cmdList xs (if y = ":true:" then (Pushb (B y) :: cmdList) else if y = ":false:" then (Pushb (B y) :: cmdList) else (Push  (ERROR (":error:")) :: cmdList))
      | "pushs"::y::xs -> string_to_cmdList xs (if String.rcontains_from y 1 'S' then (Pushs (S (String.sub y 1 (String.length y-1))) :: cmdList) else (Push  (ERROR (":error:")) :: cmdList)) 
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
      | "lessThan"::xs -> string_to_cmdList xs (LessThan :: cmdList)
      | "bind"::xs -> string_to_cmdList xs (Bind :: cmdList) 
      | "if"::xs -> string_to_cmdList xs (If :: cmdList)  
      | "quit"::xs -> string_to_cmdList xs (Quit :: cmdList)
      | _::xs -> string_to_cmdList xs (Push  (ERROR (":error:")) :: cmdList) in
      

  	 (* Takes two argument a list of commands and a stack and produces a new stack  *)
  	 let rec evaluate (commandList : commands list) (stack: values list) : values list = 
  	 	match (commandList, stack) with
  	 	| (Pushi x::rest, stack) -> evaluate rest (x::stack)
  	 	| (Pushs x::rest, stack) -> evaluate rest (x::stack)
  	 	| (Pushb x::rest, stack) -> evaluate rest (x::stack)
  	 	| (Pushn x::rest, stack) -> evaluate rest (x::stack)
  	 	| (Push  x::rest, stack) -> evaluate rest (x::stack)
  	 	| (Add::rest, I x::I y::stack) ->  evaluate rest (I(x+y)::stack)
  	 	| (Sub::rest, I y::I x::stack) ->  evaluate rest (I(x-y)::stack)
      | (Mul::rest, I y::I x::stack) ->  evaluate rest (I(x*y)::stack)
      | (Div::rest, I (0)::I x::stack) ->  evaluate rest (ERROR (":error:")::I(0)::I x::stack)
      | (Div::rest, I y::I x::stack) ->  evaluate rest (I(x/y)::stack)
      | (Rem::rest, I (0)::I x::stack) ->  evaluate rest (ERROR (":error:")::I(0)::I x::stack)
      | (Rem::rest, I y::I x::stack) ->  evaluate rest (I(x mod y)::stack)
      | (Neg::rest, I x::stack) ->       evaluate rest (I(-x)::stack)
      | (Swap::rest, x::y::stack) ->     evaluate rest ((y)::(x)::stack)
      | (Pop ::rest, x::stack) ->        evaluate rest (stack)
      | (Cat::rest, S x::S y::stack) ->  evaluate rest (S(y^x)::stack)
      | (And::rest, B (":true:")::B (":true:")::stack) ->  evaluate rest (B(":true:")::stack)
      | (And::rest, B (":false:")::B (":true:")::stack) ->  evaluate rest (B(":false:")::stack)
      | (And::rest, B (":true:")::B (":false:")::stack) ->  evaluate rest (B(":false:")::stack)
      | (And::rest, B (":false:")::B (":false:")::stack) ->  evaluate rest (B(":false:")::stack)
      | (Or::rest, B (":true:")::B (":true:")::stack) ->  evaluate rest (B(":true:")::stack)
      | (Or::rest, B (":false:")::B (":true:")::stack) ->  evaluate rest (B(":true:")::stack)
      | (Or::rest, B (":true:")::B (":false:")::stack) ->  evaluate rest (B(":true:")::stack)
      | (Or::rest, B (":false:")::B (":false:")::stack) ->  evaluate rest (B(":false:")::stack)
      | (Not::rest, B (":true:")::stack) ->       evaluate rest (B(":false:")::stack)
      | (Not::rest, B (":false:")::stack) ->       evaluate rest (B(":true:")::stack)
      | (Equal::rest, I x::I y::stack) ->  evaluate rest (if x==y then (B(":true:")::stack) else (B(":false:")::stack))
      | (LessThan::rest, I x::I y::stack) ->  evaluate rest (if y<x then (B(":true:")::stack) else (B(":false:")::stack))
      | (Bind::rest, x::N y::stack) ->  evaluate rest (stack)
      | (If::rest, x::y::B z::stack) ->  evaluate rest (if z=":true:" then ((x)::stack) else ((y)::stack))
  	 	| (Quit::rest, stack) -> stack
      | (_::rest , stack) -> evaluate rest ((ERROR (":error:"))::stack)
      | (_ , _) -> evaluate commandList stack in

  (* List of Commands *)
  let cmdList = string_to_cmdList ls_str [] in

  (* Stack of values *)
  let newStack = evaluate cmdList [] in

  (* Takes an stack and Write the stack values to an output file *)
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
interpreter (("sample_input1.txt"), ("sample_output1.txt"))