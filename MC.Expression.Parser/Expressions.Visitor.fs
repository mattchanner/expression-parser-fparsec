namespace MC.Expression

open MC.Expression.Ast

module Visitors =
    
    /// A visitor, providing an object oriented API for traversing the syntax tree.
    [<AbstractClass>]
    type Visitor<'State>() =
        
        // Each abstract member comes with a default, do nothing implementation.  This enables
        // sub classes to extend only the methods they care about

        abstract member VisitString: QuoteType -> string -> 'State -> unit
        default x.VisitString (q:QuoteType) (s:string) (state:'State) = ()

        abstract member VisitBool: bool -> 'State -> unit
        default x.VisitBool (b:bool) (state:'State) = ()

        abstract member VisitFloat: float -> 'State -> unit
        default x.VisitFloat (f:float) (state:'State) = ()

        abstract member VisitVar: string -> 'State -> unit
        default x.VisitVar (name:string) (state:'State) = ()

        abstract member VisitIdentifier: string -> 'State -> unit
        default x.VisitIdentifier (name:string) (state:'State) = ()

        abstract member VisitArgs: Expr list -> 'State -> unit
        default x.VisitArgs (args: Expr list) (state:'State) = ()

        abstract member VisitFunc: string -> Expr -> 'State -> unit
        default x.VisitFunc (name:string) (args:Expr) (state:'State) = ()

        abstract member VisitArithmetic: Expr -> Op -> Expr -> 'State -> unit
        default x.VisitArithmetic (left:Expr) (op: Op) (right:Expr) (state:'State) = ()

        abstract member VisitComparison: Expr -> Op -> Expr -> 'State -> unit
        default x.VisitComparison (left:Expr) (op: Op) (right:Expr) (state:'State) = ()
        
        abstract member VisitLogical: Expr -> Op -> Expr -> 'State -> unit
        default x.VisitLogical (left:Expr) (op: Op) (right:Expr) (state:'State) = ()

        abstract member VisitUnary: Op -> Expr -> 'State -> unit
        default x.VisitUnary (op: Op) (right:Expr) (state:'State) = ()

        abstract member VisitIfThenElse: Expr -> Expr -> Expr -> 'State -> unit
        default x.VisitIfThenElse (ifexpr:Expr) (thenexpr:Expr) (elseexpr:Expr) (state:'State) = ()

        abstract member VisitNot: Expr -> 'State -> unit
        default x.VisitNot (expr:Expr) (state:'State) = ()

        abstract member VisitOr: Expr -> 'State -> unit
        default x.VisitOr (expr:Expr) (state:'State) = ()

        abstract member VisitAnd: Expr -> 'State -> unit
        default x.VisitAnd (expr:Expr) (state:'State) = ()

        abstract member VisitForEach: Expr -> Expr -> 'State -> unit
        default x.VisitForEach (vars:Expr) (lamda:Expr) (state:'State) = ()

        abstract member VisitLamda: Expr list -> 'State -> unit
        default x.VisitLamda (expressions:Expr list) (state:'State) = ()

        abstract member VisitFilter: Expr -> Expr -> 'State -> unit
        default x.VisitFilter (var:Expr) (filter:Expr) (state:'State) = ()

        abstract member VisitAssign: string -> Expr -> 'State -> unit
        default x.VisitAssign (name:string) (expr:Expr) (state:'State) = ()

    /// Applies a visitor to the current node and any sub nodes recursively
    let rec Visit (visitor: Visitor<'State>) (state: 'State) node =
        match node with
        | Literal(String(quote, value)) -> 
            visitor.VisitString quote value state
        | Literal(Bool(value)) -> 
            visitor.VisitBool value state
        | Literal(Float(value)) -> 
            visitor.VisitFloat value state
        | Var(name) -> 
            visitor.VisitVar name state
        | Ident(identifier) -> 
            visitor.VisitIdentifier identifier state
        | Args(args) -> 
            visitor.VisitArgs args state; args 
            |> List.iter (fun x -> Visit visitor state x)
        | Func(name, args) -> 
            visitor.VisitFunc name args state
            Visit visitor state args
        | Arithmetic(left, op, right) -> 
            visitor.VisitArithmetic left op right state
            Visit visitor state left
            Visit visitor state right
        | Comparison(left, op, right) -> 
            visitor.VisitArithmetic left op right state
            Visit visitor state left
            Visit visitor state right
        | Logical(left, op, right) -> 
            visitor.VisitArithmetic left op right state
            Visit visitor state left
            Visit visitor state right
        | UnaryOp(op, right) -> 
            visitor.VisitUnary op right state
            Visit visitor state right
        | IfThenElse(ifexpr, thenexpr, elseexpr) ->
            visitor.VisitIfThenElse ifexpr thenexpr elseexpr state
            Visit visitor state ifexpr
            Visit visitor state thenexpr
            Visit visitor state elseexpr
        | Not(expr) -> 
            visitor.VisitNot expr state
            Visit visitor state expr
        | Or(expr) -> 
            visitor.VisitOr expr state
            Visit visitor state expr
        | And(expr) -> 
            visitor.VisitAnd expr state
            Visit visitor state expr
        | ForEach(vars, lamda) ->
            visitor.VisitForEach vars lamda state
            Visit visitor state vars
            Visit visitor state lamda
        | Filter(var, lamda) ->
            visitor.VisitFilter var lamda state
            Visit visitor state var
            Visit visitor state lamda
        | Assign(ident, expr) ->
            visitor.VisitAssign ident expr state
            Visit visitor state expr
        | Lamda(expressions) ->
            visitor.VisitLamda expressions state
            expressions |> List.iter (Visit visitor state)