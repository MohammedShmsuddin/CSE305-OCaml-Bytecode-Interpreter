let rec run cmds stack =
  match cmds with
  | Push x :: rest -> run rest (x :: stack)
  | Pop :: rest -> let (a :: stack) = stack in run rest (stack)
  | Add :: rest -> let (a :: b :: stack) = stack in run rest (a + b :: stack)
  | [] -> stack ;;

  let run cmds = run cmds [];;



let start k = k []
let push stack x k = k (x :: stack)
let pop (_ :: stack) k = k stack
let add (a :: b :: stack) k = k (a + b :: stack)
let stop (x :: _) = x



let rec evaluate (commandList : commands list) (stack: values list) : values list = 
      match commandList with
      | Pushi x::rest -> evaluate rest (x::stack)
      | Pushs x::rest -> evaluate rest (x::stack)
      | Pushb x::rest -> evaluate rest (x::stack)
      | Pushn x::rest -> evaluate rest (x::stack)
      | Push  x::rest -> evaluate rest (x::stack)
      | Add::rest -> let (I x::I y::stack)  = stack in evaluate rest (I(x+y)::stack)
      | Sub::rest -> let (I y::I x::stack)  = stack in evaluate rest (I(x-y)::stack)
      | Mul::rest -> let (I y::I x::stack)  = stack in evaluate rest (I(x*y)::stack)
      | Div::rest -> let (I y::I x::stack)  = stack in evaluate rest (I(x/y)::stack)
      | Rem::rest -> let (I y::I x::stack)  = stack in evaluate rest (I(x mod y)::stack)
      | Neg::rest -> let (I x::stack)  = stack in evaluate rest (I(-x)::stack)
      | Swap::rest -> let (x::y::stack)  = stack in evaluate rest ((y)::(x)::stack)
      | Pop ::rest -> let (x::stack)  = stack in evaluate rest (stack)
      | Quit::rest -> stack
      | _ -> evaluate commandList stack



type something = | One | Two;;
type something = One | Two

let s = One;;
val s : something = One

let lst = [One;Two;One;One];;
val lst : something list = [One; Two; One; One]

match (s, lst) with
|(_,[]) -> print_string "empty"
|(_,(hd::[])) -> print_string "not enough elements"
|(One,(hd1::hd2::tl)) -> print_string "One"
|(Two,(hd1::hd2::tl)) -> print_string "Two";;
One- : unit = ()




                            (* Testing File *)



(*let rec string_to_cmdList (myList : string list) (cmdList : commands list) = 
      match myList with
      | [] -> List.rev cmdList
      | "pushi"::y::xs -> string_to_cmdList xs (Pushi (int_of_string y) :: cmdList)
      | "pushb"::y::xs -> string_to_cmdList xs (Pushb (bool_of_string y) :: cmdList)
      | "pushs"::y::xs -> string_to_cmdList xs (Pushs (y) :: cmdList)
      | "pushn"::y::xs -> string_to_cmdList xs (Pushn (y) :: cmdList)
      | "push"::y::xs -> string_to_cmdList xs (Push   (y) :: cmdList)
      | "add"::xs -> string_to_cmdList xs (Add :: cmdList) 
      | "pop"::xs -> string_to_cmdList xs (Pop :: cmdList) 
      | "quit"::xs -> string_to_cmdList xs (Quit :: cmdList)
      | _ -> string_to_cmdList myList cmdList in



let rec evaluate (commandList : commands list) (stack: int list) = 
      match commandList with
      | [] -> List.rev stack
      | Pushi x::rest -> evaluate rest (x::stack)
      | Add::rest -> let (x::y::stack)  = stack in evaluate rest (y + x::stack)
      | Pop ::rest -> let (x::stack)  = stack in evaluate rest (stack)
      | Quit::rest -> evaluate rest stack
      | _ -> evaluate commandList stack in*)



let rec evaluate (commandList : commands list) (stack: values list) : values list = 
      match (commandList, stack) with
      | Pushi x::rest -> evaluate rest (x::stack)
      | Pushs x::rest -> evaluate rest (x::stack)
      | Pushb x::rest -> evaluate rest (x::stack)
      | Pushn x::rest -> evaluate rest (x::stack)
      | Push  x::rest -> evaluate rest (x::stack)
      | Quit::rest -> stack
      | _ -> evaluate commandList stack


interpreter (("sample_input1.txt"), ("sample_output1.txt"))





let rec f l = 
  match l with
  | [] -> 0
  | (z::y::x::_)::tl  -> z+x+y
  | ([]::_)    -> 0
  |  _         -> 42;;
           




let rec evaluate (commandList : commands list) (listOfStack : values list list) (nameValuelist : (values * string) list list) : values list = 
      match (commandList, listOfStack) with
      | (Pushi x::rest, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
      | (Pushs x::rest, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
      | (Pushb x::rest, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
      | (Pushn x::rest, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
      | (Push  x::rest, hd::listOfStack) -> evaluate rest ((x::hd)::listOfStack) nameValuelist
      | (Add::rest, (I x::I y::hd)::listOfStack) ->  evaluate rest ((I(x+y)::hd)::listOfStack) nameValuelist
      | (Quit::rest, hd::listOfStack) -> hd
      | (_::rest , hd::listOfStack) -> evaluate rest (((ERROR (":error:"))::hd)::listOfStack) nameValuelist
      | (_ , _) -> evaluate commandList listOfStack nameValuelist ;;





let getX x l = List.assoc x l



match ("a" ,[[("a",2);("b",3)]; [("c", 14);("d",200)]]) with
   | (hd::tl) -> List.assoc "a" l


let rec getValue (name : values) (envList : (values * string) list list) : string = 
  match envList with
  | [] -> raise Not_found
  | hd::tl -> if (List.mem_assoc name hd) then (List.assoc name hd) else getValue name tl ;;


   match (hd) with
              | (a,b)::hd -> if compare a name = 0 then b else getValue name tl
              | _ -> getValue name tl ;;
  

  let rec assoc x = function
    [] -> ""
  | (a,b)::l -> if compare a x = 0 then b else assoc x l   










let rec getValue (name : values) (envList : (values * values) list list) : values = 
      match envList with
      | [] -> ERROR("")
      | hd::tl -> if (List.mem_assoc name hd) then (List.assoc name hd) else getValue name tl in

