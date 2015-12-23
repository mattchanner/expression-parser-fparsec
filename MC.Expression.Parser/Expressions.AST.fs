namespace MC.Expression

module Ast =

    /// Operators
    type Op =
        | Minus | Plus | Subtract | Divide | Multiply | Power | Modulo
        | Concat
        | LT | GT | Eq | NotEq | LTEq | GTEq
        with 
            override x.ToString() =
                match x with
                | Minus -> "-"
                | Plus -> "+"
                | Subtract -> "-"
                | Divide -> "/"
                | Multiply -> "*"
                | Power -> "^"
                | Modulo -> "%"
                | Concat -> "&"
                | LT -> "<"
                | GT -> ">"
                | Eq -> "="
                | NotEq -> "<>"
                | LTEq -> "<="
                | GTEq -> ">="

    /// Records the type of quotes used in a string literal.  Important when converting
    /// the AST back to a string representation
    type QuoteType = Double | Single
        with override x.ToString() = 
                match x with
                | Double -> "\""
                | Single -> "'"
        
    type identifier = string

    type Value =
        // Primitives
        | String of QuoteType * string (* 'Single Quote' or "Double Quotes" *)
        | Bool   of bool   (* true | false *)
        | Float  of float  (* 1.2 1E-5 *)

    type Expr =

        // Primitives
        | Literal of Value
        | Ident  of identifier (* ident *)
        | Var    of identifier (* [VAR] *)

        // Expressions
        | Args       of Expr list
        | Func       of identifier (* identifier *) * Expr (* arguments *)
        | Lamda      of Expr list (* expressions (possibly an array initializer *)
        | Arithmetic of Expr * Op * Expr
        | Comparison of Expr * Op * Expr
        | Logical    of Expr * Op * Expr
        | UnaryOp    of Op * Expr
        
        // Logical
        | IfThenElse of Expr (* IF *) * Expr (* THEN *) * Expr (* ELSE *)
        | Not        of Expr
        | Or         of Expr
        | And        of Expr
        | ForEach    of Expr (* Argument list *) * Expr (* Lamda with implicit variable naming *)        
        | Filter     of Expr (* Var *) * Expr (* Lamda with filter predicate *)
        | Assign     of identifier (* identifier *) * Expr (* rhs expression *)
        with
            /// Provides a string representation of the syntax tree
            override x.ToString() =
                // produces a comma delimited list of AST expressions
                let commaList ls =
                    let buffer = System.Text.StringBuilder()
                    let rec commaListRec ls =                    
                        match ls with
                        | x::xs -> 
                            buffer.Append(x.ToString()) |> ignore
                            if not xs.IsEmpty then
                                buffer.Append(",")  |> ignore
                            commaListRec xs
                        | _ -> buffer.ToString()
                    commaListRec ls

                match x with
                | Literal(String(quote, value)) -> sprintf "%O%s%O" quote value quote
                | Literal(Bool(value)) -> sprintf "%b" value
                | Literal(Float(value)) -> sprintf "%g" value
                | Ident(value) -> sprintf "%s" value
                | Var(value) -> sprintf "[%s]" value
                | Args(values) -> sprintf "(%s)" <| commaList values
                | Func(name, args) -> sprintf "%s%s" name (args.ToString())
                | Lamda(expressions) -> sprintf " {%s} " <| commaList expressions
                | Arithmetic(left, op, right) -> sprintf "(%O %O %O)" left op right
                | Comparison(left, op, right) -> sprintf "(%O %O %O)" left op right
                | Logical(left, op, right) -> sprintf "(%O %O %O)" left op right
                | UnaryOp(op, right) -> sprintf "%O%O" op right
                | IfThenElse(ifexpr, thenexpr, elseexpr) -> sprintf "if(%O, %O, %O)" ifexpr thenexpr elseexpr
                | Not(expr) -> sprintf "not%O" expr
                | Or(expr) ->  sprintf "or%O" expr
                | And(expr) ->  sprintf "and%O" expr
                | ForEach(args, lamda) -> sprintf "foreach(%O, %O)" args lamda
                | Filter(arg, lamda) -> sprintf "filter(%O, %O)" arg lamda
                | Assign(ident, expr) -> sprintf "let %s = %O" ident expr

    
    (* Helpers used for determining the type of an AST node *)
    let isString = function | String(_, _) -> true | _ -> false
    let isBool = function | Bool(_) -> true | _ -> false
    let isFloat = function | Float(_) -> true | _ -> false
    let isIdent = function | Ident(_) -> true | _ -> false
    let isVar = function | Var(_) -> true | _ -> false
    let isArgs = function | Args(_) -> true | _ -> false
    let isFunc = function | Func(_, _) -> true | _ -> false
    let isLamda = function | Lamda(_) -> true | _ -> false
    let isArithmetic = function | Arithmetic(_, _, _) -> true | _ -> false
    let isComparison = function | Comparison(_, _, _) -> true | _ -> false
    let isLogical = function | Logical(_, _, _) -> true | _ -> false
    let isUnary = function | UnaryOp(_, _) -> true | _ -> false
    let isIfThenElse = function | IfThenElse(_, _, _) -> true | _ -> false
    let isNot = function | Not(_) -> true | _ -> false
    let isOr = function | Or(_) -> true | _ -> false
    let isAnd = function | And(_) -> true | _ -> false
    let isForEach = function | ForEach(_, _) -> true | _ -> false
    let isFilter = function | Filter(_, _) -> true | _ -> false
    let isAssign = function | Assign(_, _) -> true | _ -> false   
   

