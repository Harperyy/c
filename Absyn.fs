(* File MicroC/Absyn.fs
   Abstract syntax of micro-C, an imperative language.
   sestoft@itu.dk 2009-09-25

   Must precede Interp.fs, Comp.fs and Contcomp.fs in Solution Explorer
 *)

module Absyn

type typ =
    | TypI (* Type int                    *)
    | TypC (* Type char                   *)
    | TypA of typ * int option (* Array type                  *)
    | TypP of typ (* Pointer type                *)

and exn = Exception of string

and expr =
    | DecInit of typ * string * expr
    | Access of access (* x    or  *p    or  a[e]     *)
    | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
    | PlusAssign of access * expr (* x+=e or  *p+=e or  a[e]+=e  *)
    | MinusAssign of access * expr (* x-=e or  *p-=e or  a[e]-=e  *)
    | TimesAssign of access * expr (* x*=e or  *p*=e or  a[e]*=e  *)
    | DivAssign of access * expr (* x/=e or  *p/=e or  a[e]/=e  *)
    | ModAssign of access * expr (* x%=e or  *p%=e or  a[e]%=e  *)
    | Addr of access (* &x   or  &*p   or  &a[e]    *)
    | CstI of int (* Constant                    *)
    | Prim1 of string * expr (* Unary primitive operator    *)
    | Prim2 of string * expr * expr (* Binary primitive operator   *)
    | Prim3 of expr * expr * expr (* a?b:c                       *)
    | Andalso of expr * expr (* Sequential and              *)
    | Orelse of expr * expr (* Sequential or               *)
    | Call of string * expr list (* Function call f(...)        *)
    | PreInc of access (* ++x        *)
    | PreDec of access (* --x        *)

and access =
    | AccVar of string (* Variable access        x    *)
    | AccDeref of expr (* Pointer dereferencing  *p   *)
    | AccIndex of access * expr (* Array indexing         a[e] *)

and stmt =
    | If of expr * stmt * stmt (* Conditional                 *)
    | While of expr * stmt (* While loop                  *)
    | For of expr * expr * expr * stmt (* for(i = 0;i < n;i++)*)
    | Switch of expr * (expr * stmt) list
    | SwitchDefault of expr * (expr * stmt) list * stmt (* switch with default           *)
    | Expr of expr (* Expression statement   e;   *)
    | Return of expr option (* Return from method          *)
    | Block of stmtordec list (* Block: grouping and scope   *)
    | Throw of exn (* throw exn*)
    | TryCatch of stmt * exn * stmt (* try stmt1 catch(exn) {stm2}*)

and stmtordec =
    | Dec of typ * string (* Local variable declaration  *)
    | DecAssign of typ * string * expr (* int i  = 0 *)
    | Stmt of stmt (* A statement                 *)

and topdec =
    | Fundec of typ option * string * (typ * string) list * stmt
    | Vardec of typ * string

and program = Prog of topdec list
