(***************************************************************************************************
Homework 2
==========
CSCI 599, Fall 2023: An Introduction to Programming Languages
Mukund Raghothaman
Due: 10pm PT on 23 October, 2023
***************************************************************************************************)

(***************************************************************************************************
- Your name: Aaron Dardik
- Your partner's name (if any): __________
  Comments: Earlier, the parser in question 3 was able
  to parse all the commands. Currently it can parse assignment
  as well as sequence but can't parse the others (I know the issue
  is in the parser as aeval, beval and ceval work. The error
  I get there is Exception: Impparser.MenhirBasics.Error which
  when I looked it up indicates that the issue is in the parser. I
  have worked on the parser for a LONG time and I cannot figure out
  what I am doing that would cause assignment and sequence to work
  but not something as "simple" as skip - which is working in ceval. )
***************************************************************************************************)

(***************************************************************************************************
Instructions
------------
1. Please setup your programming environment by following the steps outlined in the cheatsheet and
   on the course website.

2. Please feel free to collaborate with a partner, but make sure to write your submission
   independently.

3. This assignment consists of four files: main.ml (this file), imp.ml (describing the abstract
   syntax of the Imp language), and two build scripts: build.sh and clean.sh.

4. Please execute build.sh to be able to load this file into the toplevel:

     $ ./build.sh

5. IMPORTANT: Make sure that I can cleanly import this file into the Ocaml toplevel when I call
   #use "main.ml". Comment out any questions you were unable to solve / have left unanswered.

6. As part of your submission, you will create two additional files, implexer.mll and impparser.mly.
   Place all seven files in a directory with your name, zip the directory, and submit the resulting
   archive using Blackboard.
***************************************************************************************************)
#load "calcparser.cmo";;
#load "calclexer.cmo";; 


#load "impparser.cmo";;
#load "implexer.cmo";; 
(*above two are new for fixes*)


#load "imp.cmo";;

open Imp

exception NotImplemented

(** Distribution of points: 5 + 10 + 20 + 10 + 10 + 15 + 15 = 85 points *)

(**************************************************************************************************)
(* Question 1: Calculators that Multiply (3 + 2 points)

   The calculator that we developed in class was very simple, and only supported addition and
   subtraction. This was convenient because both addition and subtraction have the same precedence:
   Both expressions 2 - 3 + 5 and 2 + 5 - 3 should evaluate to 4. You will find ASTs defined in
   calc.ml, and the lexical analyzer and parser from class in calclexer.mll and calcparser.mly
   respectively.

   Extend the lexical analyzer and parser to also support multiplication.

   For full credit:
   1. Ensure that multiplication binds more tightly than the remaining two operators. This is the
      convention from elementary school which is variously called PEMDAS or BODMAS. For example,
      2 * 3 + 5 should evaluate to (2 * 3) + 5 = 11, and 2 + 5 * 3 should evaluate to 2 + (5 * 3) =
      17.
   2. Ensure that both "calclexer.mll" nor "calcparser.mly" produce no warnings when compiled using
      the respective tools. *)


#load "calcparser.cmo";;
#load "calclexer.cmo";; 


(* Now extend the evaluation function to also support multiplication: *)

let rec eval_of_calc : Calc.expr -> int =
  fun e ->
    match e with
    | Int(c) -> c
    | Mult(e1, e2) -> eval_of_calc e1 * eval_of_calc e2 
    | Plus(e1, e2) -> eval_of_calc e1 + eval_of_calc e2
    | Minus(e1, e2) -> eval_of_calc e1 - eval_of_calc e2

(* If your implementation works, then we should be able to evaluate expressions with: *)

let eval_of_str : string -> int =
  fun str -> str |> Lexing.from_string |> Calcparser.prog Calclexer.read |> eval_of_calc

(* We provide some tests that can help validate your implementation: *)

(* let _ =
  let _ = assert (eval_of_str "2 * 3 + 5" = 11) in
  let _ = assert (eval_of_str "2 + 5 * 3" = 17) in
  ()
*)

(**************************************************************************************************)
(* Question 2: Interpreting an Imperative Language, Part 1 (3 + 3 + 4 points)

   In this question and the next, we will build an end-to-end interpreter for a toy imperative
   language, sometimes called Imp. Here is an example program in Imp:

     x = 5;
     while (0 < x) do
       output x;
       x = x - 1
     done

   Executing this program results in the list of integers [5; 4; 3; 2; 1].

   In this question, we will write the core of the interpreter, which starts from the AST of the
   program and does the actual evaluation. In the next question, we will write the parser which
   takes a string representation of the program and produces the AST. Chaining the two together will
   give us the end-to-end interpreter.

   Constructs in Imp belong to one of three syntactic categories, arithmetic expressions, Boolean
   expressions and commands, and we will represent these using the types Imp.aexp, Imp.bexp and
   Imp.cmd respectively. See their definitions in the supplementary file imp.ml.

   Recall your solution to Question 7b from Homework 1. To evaluate an arithmetic expression, such
   as "x + 5", it is necessary to know the value of the variable "x". We will record the current
   value of each variable in a map / dictionary which associates their names with their values. This
   map, which we will call the "evaluation environment", will be of type env_t, where: *)



module StringMap = Map.Make(String)
type env_t = int StringMap.t

let empty_env = StringMap.empty

(* An Recall from Homework 1 that variables of a map may be set and queried using the StringMap.add
   and StringMap.find_opt functions respectively. The empty map is given by empty_env. To simplify
   matters, all variables which are not explicitly defined in the environment have the initial value
   0.

   Fill in the functions aeval, beval, and ceval. We have included some assertions in comments which
   you may use to test your solutions. *)

let rec aeval (env : env_t) (a : aexp) : int =
    match a with 
    | Int (n) -> n 
    | Var (x) -> (match (StringMap.find_opt x env) with
      | None -> 0
      | Some v -> v)
    | Plus (e1, e2) -> (aeval env e1) + (aeval env e2) 
    | Minus (e1, e2) -> (aeval env e1) - (aeval env e2)
     
     
  
  let _ =
  let env0 = empty_env in
  let env1 = StringMap.add "x" 5 env0 in

  let a1 = Int 3 in
  let a2 = Var "x" in
  let a3 = Plus(a1, a1) in
  let a4 = Minus(a2, a1) in

  let _ = assert ((aeval env0 a1) = 3) in
  let _ = assert ((aeval env0 a2) = 0) in
  let _ = assert ((aeval env1 a2) = 5) in
  let _ = assert ((aeval env0 a3) = 6) in
  let _ = assert ((aeval env1 a4) = 2) in
  () 
  

let rec beval (env : env_t) (b : bexp) : bool =
  match b with 
  | Bool(f) -> f 
  | Lt (e1, e2) -> (if (aeval env e1 < aeval env e2) then true else false)
  | Leq (e1, e2) -> (if (aeval env e1 <= aeval env e2) then true else false)
  | Eq (e1, e2) -> (if (aeval env e1 = aeval env e2) then true else false)
  | And (b1, b2) -> (if ((beval env b1) && (beval env b2)) then true else false)
  | Or (b1, b2) -> (if ((beval env b1) || (beval env b2)) then true else false)
  | Not (b) -> (if (beval env b) then false else true)


 let _ =
  let env0 = empty_env in
  let env1 = StringMap.add "x" 3 env0 in
  let env2 = StringMap.add "y" 5 env1 in

  let ax = Var "x" in
  let ay = Var "y" in

  let b1 = Bool false in
  let b2 = Lt(ax, ay) in
  let b3 = Leq(ax, Plus(ax, ay)) in
  let b4 = Eq(Plus(ax, ay), Plus(ay, ax)) in
  let b5 = And(b2, b3) in
  let b6 = Or(b1, b3) in
  let b7 = Not(b1) in

  let _ = assert (not (beval env2 b1)) in
  let _ = assert (beval env2 b2) in
  let _ = assert (beval env2 b3) in
  let _ = assert (beval env2 b4) in
  let _ = assert (beval env2 b5) in
  let _ = assert (beval env2 b6) in
  let _ = assert (beval env2 b7) in
  () 

  
  let ceval (c : cmd) : int list =
    let rec ceval_exec_helper (env : env_t) (c : cmd) : int list * env_t =
      match c with
      | Output(e) -> 
          let eval_result = aeval env e in
          ([eval_result], env)
      | Asgn(s, e) ->
          let value = aeval env e in
          let new_env = StringMap.add s value env in
          ceval_exec_helper new_env Skip
      | Skip -> ([], env)
      | Seq (e1, e2) ->
          let (result1, env1) = ceval_exec_helper env e1 in
          let (result2, env2) = ceval_exec_helper env1 e2 in
          (result1 @ result2, env2)
      | IfElse (b, e1, e2) ->
          if beval env b then
            ceval_exec_helper env e1
          else
            ceval_exec_helper env e2
      | While (b, e) ->
          let ifeval = beval env b in 
          if ifeval then 
            (
              let (result, updated_env) = ceval_exec_helper env e in
              let (final_result, final_env) = ceval_exec_helper updated_env (While (b, e)) in
              (result @ final_result, final_env)
            )
          else
            ([], env)
    in
    let (result, final_env) = ceval_exec_helper empty_env c in
    result
    

  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  (*
  let ceval (c : cmd) : int list =
    let rec execute_command (env : env_t) (c : cmd) : int list =
      match c with
      | Output(e) -> [aeval env e]
      | Asgn(s, e) ->
        let env = StringMap.add s (aeval env e) env;
        []  (* Since Asgn returns an empty list *)
      | Skip -> []
      | Seq (e1, e2) ->
        let result1 = execute_command env e1 in
        let result2 = execute_command env e2 in
        result1 @ result2  (* Appending the results of e1 and e2 *)
      | IfElse (b, e1, e2) ->
        if beval env b then
          execute_command env e1
        else
          execute_command env e2
      | While (b, e) ->
        if beval env b then
          let _ = execute_command env e in
          execute_command env (While (b, e))
        else
          []
    in
    execute_command empty_env c
    ;
  
    

    
   *) 
  

 let _ =
  let ax = Var "x" in
  let ay = Var "y" in
  let a0 = Int 0 in
  let a1 = Int 1 in
  let a5 = Int 5 in
  let c = Seq(Asgn("x", a5),
              While(Lt(a0, ax),
                    Seq(Output(ax),
                    Seq(Asgn("x", Minus(ax, a1)),
                    Seq(Output(ay), Asgn("y", Plus(ay, a1))))))) in
  assert (ceval c = [5; 0; 4; 1; 3; 2; 2; 3; 1; 4]) 

(**************************************************************************************************)
(* Question 3: Interpreting an Imperative Language, Part 2 (10 + 10 points)


my notes: 3 non-terminals in cmd: comes out of type Imp.cmd 

   We will now write the front-end of the interpreter. Construct a lexical analyzer using Ocamllex
   in a file named "implexer.mll", and construct a parser using Menhir in a file named
   "impparser.mly".

   Here is the grammar describing the syntax of Imp:

     Commands,
     cmd ::= "output" aexp
           | var-name "=" aexp
           | "skip"
           | cmd ";" cmd (* Execute the first command, followed by the second command *)
           | "if" bexp "then" cmd "else" cmd "fi"
           | "while" bexp "do" cmd "done"
           | "(" cmd ")"

     Arithmetic expressions,
     aexp ::= integer-literal (* Any non-empty sequence of digits from '0' to '9', possibly prefixed
                                 with a '-' *)
            | variable-name   (* Variable names begin with an alphabet or an underscore character,
                                 and are followed by zero or more alphabets, digits, or underscore
                                 characters. *)
            | aexp "+" aexp   (* Both "+" and "-" are left-associative, so that "a - b + c" is
                                 parsed as "(a - b) + c". *)
            | aexp "-" aexp
            | "(" aexp ")"

     Boolean expressions,
     bexp ::= "true"
            | "false"
            | aexp "<" aexp
            | aexp "<=" aexp
            | aexp "==" aexp
            | bexp "&&" bexp
            | bexp "||" bexp
            | "!" bexp
            | "(" bexp ")"

   For full credit:
   1. Make sure that to resolve precedence correctly with the Boolean operators. The negation
      operator, "!" binds the tightest of them all, and the disjunction operator "||" is the
      loosest. In particular, "true || false && true" should be parsed as "true || (false && true)",
      "true && false || true" should be parsed as "(true && false) || true", and "!false && true"
      should be parsed as "(!false) && true" respectively.
   2. Ensure that both "implexer.mll" nor "impparser.mly" produce no warnings when compiled using
      the respective tools.

   First, some boilerplate. Uncomment the next two lines once you have finished creating
   implexer.mll and impparser.mly and build.sh does not produce any errors: *)


(*
#load "impparser.cmo";;
#load "implexer.cmo";; 
*)


(* Define the following function parse : string -> int list which takes the textual representation
   of an Imp program as input, and produces its list of integers as output. *)

   (*
   let eval_of_str : string -> int =
    fun str -> str |> Lexing.from_string |> Calcparser.prog Calclexer.read |> eval_of_calc
    *)


let parse : string -> cmd =
  fun str -> str |> Lexing.from_string |> Impparser.prog Implexer.read
   

let impEval : string -> int list =
  fun str -> str |> parse |> ceval 

(* At this point, we will test your implementation with: *)
(*
 let _ = 
  let cs = "x = 5;
            while (0 < x) do
              output x;
              x = x - 1;
              output y;
              y = y + 1
            done" in assert (impEval cs = [5; 0; 4; 1; 3; 2; 2; 3; 1; 4]) 
*)
(**************************************************************************************************)
(* Question 4: Identifying Ambiguous Grammers (5 + 5 points)

   An important part of designing the syntax of the language involves resolving ambiguities while
   preserving convenience. Consider the following grammars.

   4a. A language of (only) conditionals, Version I

         cmd ::= "skip"
               | cmd ";" cmd
               | "if" bexp "then" cmd "else" cmd "fi"
         bexp ::= "true" | "false"

       Examples of commands in this language are "skip", "skip; skip" and
       "if true then skip else skip fi".

       Is the grammar of commands unambiguous? If yes, then explain. If not, present a command which
       can be parsed in two different ways.

       The above language is unambiguous. We will prove this via induction.
       First, note that any terminal symbol has a unique parse tree. Now, assume
       that for any command in this language of length n - there is only one valid
       parse tree. Suppose now we have a command of length n+1. If this command is 
       "skip" it clearly has a unique parse tree. If the command is of the form
       cmd1 ; cmd2 then the parse tree of the new command is the parse tree of cmd1
       followed by cmd2. As each of cmd1 and cmd2 were unambiguous so is command. 
       Similarly, any command of the form If bexp then cmd1 else cmd2 Can be constructed
       by making a parse tree where cmd1 and cmd2 are the 'children' of a node
       bexp, with one path following to cmd1 and the other to cmd2. Since cmd1 and cmd2
       are unambiguous, the tree created by making them the 'children' of a root
       given by bexp is unambiguous. Thus, by induction the the language is unambiguous.

   4b. A language of (only) conditionals, Version II

         cmd ::= "skip"
               | cmd ";" cmd
               | "if" bexp "then" cmd "else" cmd
         bexp ::= "true" | "false"

       Examples of commands in this language are "skip" and "if true then skip; skip else skip".

       Is the grammar of commands unambiguous? If yes, then explain. If not, present a command which
       can be parsed in two different ways.

       grammar two is ambiguous. This is because it doesn't provide a clear rule
       for how to parse sequential expressions. 
       Given the following: if true then skip else skip ; skip
       we have two potential parsings. One possible parse treats 'if true then skip else skip'
       as a single command and '; skip' as another. In this case regardless of 
       truth we will first skip, and a second skip is done after ';'. The 
       second possible parse is 'if true then skip' else 'skip ; skip' which 
       if true has one skip, and if false has two skips sequentially. As the first
       parsing has two skips sequentially in either case, these differ
       in the case of 'true' thus they have different parse trees and
       the grammar is ambiguous. 
       
       As mentioned below, the "fi" which ends if-else blocks is the key that
       differentiates the grammars - making one ambiguous and the other not. 

       


       Note that the only difference between the two grammars in Questions 4a and 4b is the keyword
       "fi" in Version I which delineates the end of a conditional statement. *)

      

(**************************************************************************************************)
(* Question 5: Understanding Regular Expressions (5 * 2 points)

   Consider the following pairs of regular expressions. For each pair, can you provide an example of
   a string which is accepted by one but not accepted by the other? If no such string exists, can
   you explain why not?

   The following answers are written assuming that the 
   regex engine is searching for an exact match (i.e. a* matches
   a string with 0 or more 'a'.) Some engines have the capability
   to 'match for substring' - in other words the string 'bbbb'
   would match a* as it contains as a subsequence 0 or more 'a' -
   in this case exactly zero. For these answers we will be assuming
   the first supposition (i.e. that a* does not match bbbb )
   but one would need to specify which regex engine one is using
   and if the engine is on complete match or substring match mode. 

   5a. a* . b* and (a | b)* 
   Not same - 
   As the symbol string "ba" will match the second sequence
   but not the first, they are not the same. 

   5b. a* . ( b* | c* ) and ( a* . b* ) | ( a* . c* )
   Same - 
   Since | is exclusive-or, the left hand expression
   can generate empty string or any numbe of 'a' (including 0)
   followed by either some number of 'b', some number of 'c' or
   nothing - but not both 'b' and 'c'. This is the same
   as the right hand side which matches empty strings, or ( a*.b* )
   matches a sequence of 'a' then 'b' and similarly
   ( a* . c* ) matches any number of 'a' then any number of 'c.'
    

   5c. a* and (a | aa)* 
   Same - 
   There is no string that can be produced by one of these
   regular expressions and not the other. Both can produce the empty string. 
   If a non-empty string is produced, a* produces a sequence of 'a' with any
   finite length (N). Clearly the same can be produced on the right by "choosing" 
   'a' N times. If, on the right hand production rule someone chooses 'aa'
   M times, then that can be produced via the left hand rule by picking 'a'
   2M times. Thus any string created by one can be created by the other. 

   5d. a* . a* and a*
   Same - 
   Assuming '.' means concatenation as stated on Piazza
   and not 'wildcard' these two regular expressions are the same
   as each regular expression can generate either the 
   empty string or a string of N 'a' for any natural
   number N.

   5e. a* and ( a* )* 
   Same - 
   The answer here depends on whether the empty string
   "exists" inside a string, in other words is the sequence:
   'a' {} 'b' {} 'c' the same as 'a' 'b' 'c' or not. If the
   empty strings don't "collapse" then these regular expressions produce different strings. 
   a* either produces an emoty string, or any finite number of 'a's. The regex
   on the right can produce those, and it can also produce a string that 
   is a sequence of 'a' interspersed with the empty-string. For example,
   the right hand regular expression can produce 'a' '' 'a' '' '' 'a' which the left
   hand cannot.  
   On the other hand, if an internal {} "collapses to nothing"
   then both regular expressions generate either an empty string
   or N 'a' for any natural number N, and thus these expressions are the same. 

   
   *)

(**************************************************************************************************)



(* Question 6: Matching Regular Expressions in Linear Time (5 + 5 + 5 points)

   In class, we outlined a single pass linear time regular expression matching algorithm. While it
   is an educational experience to implement this algorithm in its full generality, it would be
   overwhelming for a homework question. We will instead implement fast matching algorithms for some
   selected regular expressions.

   For full credit, your program must be tail recursive, run in linear time, and make a single
   left-to-right pass over the input string.

   You may find the following function useful: *)

let list_of_string : string -> char list =
  fun str -> str |> String.to_seq |> List.of_seq

(* - Write a program that determines whether a given string matches the pattern "Hi"*: *)

let (* rec? *) matchesHiStar : string -> bool =
  fun str ->
    let rec check chars =
      match chars with
      | 'H' :: 'i' :: rest -> check rest  (* Consume "Hi" *)
      | 'i' :: rest -> check rest         (* Consume 'i' *)
      | [] -> true                            (* Matched successfully with the entire string *)
      | _ -> false                            (* Any other character means it's not a match *)
    in
    match list_of_string str with
    | 'H' :: 'i' :: rest -> check rest  (* Start with "Hi" *)
    | [] -> true                            (* Empty string is considered a match *)
    | _ -> false 

(* - Write a program that determines whether a given string matches the pattern
     ("Hi" | "Hello")*: *)

let (* rec? *) matchesHiOrHelloStar : string -> bool =
  fun str ->
    let rec checkHello chars =
      match chars with 
      | 'H' :: 'e' ::'l' ::'l'::'o':: rest -> checkHello rest
      | 'H' :: 'i' :: rest -> checkHello rest
      | [] -> true
      | _ -> false
    in
    match list_of_string str with
    | 'H' :: 'i' :: rest -> checkHello rest
    | 'H' :: 'e' ::'l' ::'l'::'o':: rest -> checkHello rest
    | [] -> true
    | _ -> false

(* - Write a program that determines whether a given string matches the pattern
     ("Hi" | "Hello")* . "Okay"*: *)

     let matchesHiOrHelloOkayStar : string -> bool =
      fun str ->
        let rec checkOnlyOkay chars =  
          match chars with
          | 'O' :: 'k' :: 'a' :: 'y' :: rest -> checkOnlyOkay rest
          | [] -> true
          | _ -> false      
        in
        let rec checkHiHelloOkay chars =
          match chars with
          | 'H' :: 'i' :: rest -> checkHiHelloOkay rest
          | 'H' :: 'e' :: 'l' :: 'l' :: 'o' :: rest -> checkHiHelloOkay rest
          | 'O' :: 'k' :: 'a' :: 'y' :: rest -> checkOnlyOkay rest
          | [] -> true
          | _ -> false
        in
        checkHiHelloOkay (list_of_string str)

(**************************************************************************************************)
(* Question 7: A Simple Type Checker (15 points)

   We define here the abstract syntax of a simple expression language. Observe that we have
   eliminated the distinction between arithmetic and Boolean expressions, and variables may hold
   values of either type: *)

type expr =
  | Var of string                    (* Variables *)
  | IntLit of int                    (* Integer literals *)
  | Plus of expr * expr              (* Integer addition *)
  | Minus of expr * expr             (* Integer subtraction *)
  | BoolLit of bool                  (* Boolean literals *)
  | And of expr * expr               (* Boolean conjunctions *)
  | Or of expr * expr                (* Boolean disjunctions *)
  | Not of expr                      (* Boolean negations *)
  | Leq of expr * expr               (* Arithmetic inequality *)
  | IfThenElse of expr * expr * expr (* Conditional expressions *)

(* As usual, the eval function evaluates the expression in an environment that maps variable names
   to their corresponding values. The main difference is that expressions may evaluate to either
   integers or to Boolean values, or may be undefined, depending on the environment in which they
   are evaluated. We create a uniform way to represent the two types of values, and are careful
   while defining the evaluation function: *)

type value =
  | IntVal of int
  | BoolVal of bool

let rec eval (env : value StringMap.t) (e : expr) : value option =
  let opt_apply f o1 o2 =
    match (o1, o2) with
    | (Some c1, Some c2) -> f c1 c2
    | _ -> None in

  let zz_opt_apply f o1 o2 =
    let f' c1 c2 =
      match (c1, c2) with
      | (IntVal v1, IntVal v2) -> Some (f v1 v2)
      | _ -> None in
    opt_apply f' o1 o2 in

  let bb_opt_apply f o1 o2 =
    let f' c1 c2 =
      match (c1, c2) with
      | (BoolVal v1, BoolVal v2) -> Some (f v1 v2)
      | _ -> None in
    opt_apply f' o1 o2 in

  let z_inject ov = Option.map (fun v -> IntVal v) ov in
  let b_inject ov = Option.map (fun v -> BoolVal v) ov in

  match e with
  | Var v -> StringMap.find_opt v env
  | IntLit c -> Some (IntVal c)
  | Plus(e1, e2) -> z_inject (zz_opt_apply (+) (eval env e1) (eval env e2))
  | Minus(e1, e2) -> z_inject (zz_opt_apply (-) (eval env e1) (eval env e2))
  | BoolLit c -> Some (BoolVal c)
  | And(e1, e2) -> b_inject (bb_opt_apply (&&) (eval env e1) (eval env e2))
  | Or(e1, e2) -> b_inject (bb_opt_apply (||) (eval env e1) (eval env e2))
  | Not e1 ->
      (match eval env e1 with
       | Some (BoolVal v1) -> Some (BoolVal (not v1))
       | _ -> None)
  | Leq(e1, e2) -> b_inject (zz_opt_apply (<=) (eval env e1) (eval env e2))
  | IfThenElse(e1, e2, e3) ->
      (match eval env e1 with
       | Some (BoolVal v1) -> if v1 then eval env e2 else eval env e3
       | _ -> None)

(* Let's perform some sanity checks: *)

let _ =
  let env0 = StringMap.empty in
  let env1 = StringMap.add "x" (IntVal 5) env0 in
  let env2 = StringMap.add "y" (BoolVal true) env1 in

  let e1 = IntLit 3 in
  let e2 = Var "x" in
  let e3 = Var "y" in
  let e4 = Plus(e2, e2) in
  let e5 = Plus(e2, e3) in
  let e6 = And(e3, e3) in
  let e7 = And(e3, e2) in

  let _ = assert (eval env0 e1 = Some (IntVal 3)) in
  let _ = assert (eval env0 e2 = None) in
  let _ = assert (eval env2 e2 = Some (IntVal 5)) in
  let _ = assert (eval env2 e3 = Some (BoolVal true)) in
  let _ = assert (eval env2 e4 = Some (IntVal 10)) in
  let _ = assert (eval env2 e5 = None) in
  let _ = assert (eval env2 e6 = Some (BoolVal true)) in
  let _ = assert (eval env2 e7 = None) in
  ()

(* Notice that whether an expression results in an output depends on the environment being provided.
   For example, the expression e2 can be successfully evaluated in the environment e1, but fails to
   produce an output in environment e2. *)

let _ =
  let env0 = StringMap.empty in
  let env1 = StringMap.add "x" (IntVal 5) env0 in
  let env2 = StringMap.add "x" (BoolVal true) env1 in

  let e1 = Var "x" in
  let e2 = Plus(e1, e1) in

  let _ = assert (eval env1 e2 = Some (IntVal 10)) in
  let _ = assert (eval env2 e2 = None) in
  ()

(* Two determine whether an expression can be successfully evaluated in an environment, we create a
   simple type system for the language, consisting of two types: *)

type expr_type = IntType | BoolType

(* An environment type is a mapping from variable names to their corresponding types. One can regard
   this as a guarantee from the user that each variable is of the type being asserted.
   Alternatively, think of this as the symbol table maintained by a compiler. *)

type env_type = expr_type StringMap.t

(* Write a function get_type which takes an environment type and an expression, and determines
   whether the expression can be successfully evaluated or not. In particular, if your function
   evaluates to (Some IntType), then the expression should always evaluate to a result of the form
   (IntVal _) in conforming environments, and if it returns (Some BoolType), then evaluating the
   expression should always result in a value of type (BoolVal _). *)

   let rec get_type (gamma : env_type) (e : expr) : expr_type option =
    match e with
    | Var v -> StringMap.find_opt v gamma
    | IntLit _ -> Some IntType
    | Plus (e1, e2) | Minus (e1, e2) | Leq (e1, e2) ->
      (match (get_type gamma e1, get_type gamma e2) with
      | (Some IntType, Some IntType) -> Some IntType
      | _ -> None)
    | BoolLit _ -> Some BoolType
    | And (e1, e2) | Or (e1, e2) ->
      (match (get_type gamma e1, get_type gamma e2) with
      | (Some BoolType, Some BoolType) -> Some BoolType
      | _ -> None)
    | Not e1 ->
      (match get_type gamma e1 with
      | Some BoolType -> Some BoolType
      | _ -> None)
    | IfThenElse (e1, e2, e3) ->
      (match (get_type gamma e1, get_type gamma e2, get_type gamma e3) with
      | (Some BoolType, Some t2, Some t3) when t2 = t3 -> Some t2
      | _ -> None)
   


(* If your solution is correct, then the following assertions will hold for all
   expressions e and environments env: *)

let get_type_test (env : value StringMap.t) (e : expr) =
  let val_type (v : value) : expr_type =
    match v with
    | IntVal _ -> IntType
    | BoolVal _ -> BoolType in

  let gamma = StringMap.map val_type env in

  match (get_type gamma e, eval env e) with
  | (Some IntType, Some (IntVal _)) -> assert true
  | (Some IntType, _) -> assert false
  | (Some BoolType, Some (BoolVal _)) -> assert true
  | (Some BoolType, _) -> assert false
  | _ -> assert true



(* Disclaimer: The above tests are not exhaustive! Some of you may notice that defining:

   let get_type env_type e = None

   will pass the test cases, and also passes the requirements in a technical sense. However they do
   not perform any interesting analysis on the expressions in question. *)

(**************************************************************************************************)
(* Question 8: Conclusion (0 points)

   Did you use an LLM to solve this assignment? How did it help?
   I used an LLM to help me with syntax a few times, 
   but overall did not use it as much as the previous assignment. *)
