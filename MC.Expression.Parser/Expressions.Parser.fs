namespace MC.Expression

open FParsec
open Ast

/// A private module containing the main parser implementation
module private ParserImpl =

    let ASSIGN = "let"

    let all, allRef = createParserForwardedToRef()
    
    (* Helpers *)
    /// Shorthand for parsing an expected string
    let str s = pstring s

    /// Represents a line comment
    let comment =  str "#" >>. skipRestOfLine true .>> spaces

    /// Shorthand for skipping 0 to many white space characters
    let ws = comment <|> spaces

    /// Parses a string literal, including escaped characters
    let stringLiteral =
        let normalCharNoDoubleQuote = satisfy (fun c -> c <> '\\' && c <> '"')
        let normalCharNoSingleQuote = satisfy (fun c -> c <> '\\' && c <> '\'')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
        
        // double quoted strings
        (between (pstring "\"") (pstring "\"") 
                 (manyChars (normalCharNoDoubleQuote <|> escapedChar)) 
                 |>> fun x -> (String(Double, x))
        // single quoted strings
        <|> (between (pstring "\'") (pstring "\'") 
                     (manyChars (normalCharNoSingleQuote <|> escapedChar))  
                     |>> fun x -> (String(Single, x))))

    // succeeds when p is applied between strings s1 and s2
    let betweenStrings s1 s2 p = p |> between (str s1) (str s2)
        
    /// Applies a parser to return a list of items between the open and close strings.
    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose) 
                (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)

    /// Applies a parser to return a list of items between the open and close strings.
    let listBetweenStrings1 sOpen sClose pElement f =
        between (str sOpen) (str sClose) 
                (ws >>. sepBy1 (pElement .>> ws) (str "," .>> ws) |>> f)

    /// Creates a parser to parse an identifier - this must start with a letter or underscore, but can then be followed
    /// by any number of letters, numbers of underscores
    let ident: Parser<identifier, _> = 
        let isValidStart c = isLetter c || c = '_'
        let isValidIdent c = isLetter c || isDigit c || c = '_'
        (many1Satisfy2L isValidStart isValidIdent "identifier")

    /// Creates a parser to parse any character up until the end char.  This can include whitespaces
    let anyCharsUntil charEnd = manySatisfy (fun c -> c <> charEnd)

    // treat everything between square brackets as a variable
    let enclosedIdentifier = betweenStrings "[" "]" (anyCharsUntil ']')

    (* Expression Parsers  *)

    /// Creates a parser to parse boolean expressions (true or false).  This is case insensitive
    let xbool =     (stringCIReturn "true" (Bool true)   |>> Literal)
                <|> (stringCIReturn "false" (Bool false) |>> Literal)

    /// Creates a parser to parse a string literal, returning the result as a String
    let xstring = stringLiteral |>> Literal

    /// Creates a parser to parse a floating point number (supporting scientific notation).  The result
    /// is returned as an Float
    let xfloat = ws >>. (pfloat .>> ws) |>> Float |>> Literal        

    /// Creates a parser to parse an identifier wrapped in square brackets (white space in identifier is allowed)
    /// This returns the result as an Var
    let xvar = enclosedIdentifier |>> Var

    /// Creates a parser to parse any valid identifier, returning the result as an Ident
    let xident = parse {
        let ws1 = nextCharSatisfiesNot isLetter >>. ws
        let index = nextCharSatisfies isDigit >>. ws

        let! ident = regex "item[0-9]*"
        return Ident(ident)
    }

    /// A combined parser to parse all primitive types
    let xprim = choice [xfloat; xbool; xstring; xvar; xident]

     /// Creates a parser to parse a list of expressions enclosed in round brackets, returning the
    /// result as an Args type
    let argList = ws >>. listBetweenStrings "(" ")" all Args

    /// Creates a parser to parse a list of expressions enclosed in round brackets, returning the
    /// result as an Args type. This parser will only succeed when the arg list has at least one element
    let argList1 = ws >>. listBetweenStrings1 "(" ")" all Args

    /// Applies f to each argument parsed between open and close brackets
    let argListFn f = ws >>. listBetweenStrings "(" ")" all f

    /// Arbitrary lambda expression
    let lamda = 
        ws >>. between (str "{") (str "}") (sepBy all (str "," .>> ws)) |>> Lamda

    /// IF(condition, then, else)
    let ifThenElse = parse {
        do! skipStringCI "if" .>> ws
        do! skipString "(" .>> ws
        let! condition = all
        do! (skipString "," .>> ws) <?> "Expected `then` condition"
        let! thenExpr = all
        do! (skipString "," .>> ws) <?> "Expected `else` condition"
        let! elseExpr = all
        do! ws
        do! skipString ")"
        return IfThenElse(condition, thenExpr, elseExpr)
    }

    /// Factory  for parsing an operator with an expression list
    let plist name factory = parse {
        do! skipStringCI name
        do! spaces
        let! args = argList1        
        return factory args
    }

    /// Parser for NOT expressions
    let pnot = plist "not" Not

    /// Parser for OR(cond1, cond2, ...) expressions
    let por = plist "or" Or

    /// Parser for AND(cond1, cond2, ...) expressions
    let pand = plist "and" And

    /// All logical expressions
    let logicals = choice[ifThenElse; por; pand; pnot]

    /// Fall through function call
    let funcList = parse {
        let! identifier = ident
        let! args = argList
        return Func(identifier, args)
    }

    /// parses an assignment of the form:
    /// let [VARNAME] = [EXPR]
    let assignment = parse {
        do! ws >>. str ASSIGN >>. ws
        let! name = ident <?> "identifier"
        do! ws >>. str "=" >>. ws
        let! expr = all <?> "expression"
        return Assign(name, expr)
    }        

    /// Parser for foreach expressions aka map
    let forEach = parse {
        do! skipStringCI "foreach"
        let! args = argListFn (fun x -> x)
        let argArray = args |> Array.ofList
        if argArray.Length < 2 then
            return! fail "Incorrect number of arguments to foreach"
        else
            let inputs = Seq.take (argArray.Length - 1) argArray |> List.ofSeq
            let lamda = argArray.[argArray.Length - 1]

            if not <| isLamda lamda then
                return! fail "Expected a lamda expression as the final argument"
            else
                return ForEach(Args(inputs), lamda)        
    }

    /// Parser for filter expressions
    let filter = parse {
        do! skipStringCI "filter"
        let! args = argListFn (fun x -> x)
        if args.Length = 2 then
            let var = Seq.nth 0 args
            let pred = Seq.nth 1 args            
            if not <| isLamda pred then return! fail "Expected filter expression as second argument" 
            else return Filter(var, pred)
        else
            return! fail "Incorrect number of arguments to filter function - 2 arguments expected"
    }

    let functions = logicals <|> forEach <|> filter <|> funcList <|> lamda

    /// Configure the operator precedence parser to handle complex expressions
    let oppa = new OperatorPrecedenceParser<Expr, unit, unit>()

    let parithmetic = oppa.ExpressionParser

    oppa.TermParser <- (xprim .>> ws) <|> functions .>> ws <|> between (str "(" .>> ws) (str ")" .>> ws) parithmetic

    type Assoc = Associativity
    
    /// Binary Operators
    oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left,  fun x y -> Arithmetic(x, Plus,     y)))
    oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left,  fun x y -> Arithmetic(x, Minus,    y)))    
    oppa.AddOperator(InfixOperator("/", ws, 2, Assoc.Left,  fun x y -> Arithmetic(x, Divide,   y)))
    oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left,  fun x y -> Arithmetic(x, Multiply, y)))
    oppa.AddOperator(InfixOperator("&", ws, 3, Assoc.Left,  fun x y -> Arithmetic(x, Concat,   y)))
    oppa.AddOperator(InfixOperator("^", ws, 4, Assoc.Right, fun x y -> Arithmetic(x, Power,    y)))
    oppa.AddOperator(InfixOperator("%", ws, 5, Assoc.Right, fun x y -> Arithmetic(x, Modulo,   y)))

    /// Unary Operators
    oppa.AddOperator(PrefixOperator("-", ws, 6, true, fun x -> UnaryOp(Minus, x)))

    // Essentially a no-op
    oppa.AddOperator(PrefixOperator("+", ws, 6, true, fun x -> x))

    /// Logical Binary Operators
    let oppc = new OperatorPrecedenceParser<Expr, unit, unit>()
    let pcomparison = oppc.ExpressionParser
    let termc = (parithmetic .>> ws) <|> between (str "(" .>> ws) (str ")" .>> ws) pcomparison
    oppc.TermParser <- termc

    oppc.AddOperator(InfixOperator("=",  ws, 1, Assoc.Left, fun x y -> Comparison(x, Eq,    y)))
    oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, NotEq, y)))
    oppc.AddOperator(InfixOperator("!=", ws, 1, Assoc.Left, fun x y -> Comparison(x, NotEq, y)))
    oppc.AddOperator(InfixOperator(">",  ws, 1, Assoc.Left, fun x y -> Comparison(x, GT,    y)))
    oppc.AddOperator(InfixOperator("<",  ws, 1, Assoc.Left, fun x y -> Comparison(x, LT,    y)))
    oppc.AddOperator(InfixOperator(">=", ws, 1, Assoc.Left, fun x y -> Comparison(x, GTEq,  y)))
    oppc.AddOperator(InfixOperator("<=", ws, 1, Assoc.Left, fun x y -> Comparison(x, LTEq,  y)))

    // do exprRef := opp.ExpressionParser
    allRef := oppc.ExpressionParser

    let statements = assignment <|> all

    // The full parser, terminating with an EOF marker
    let xparser = ws >>. statements .>> ws .>> eof


/// The public parser module
module Parser =
    
    let private parser = ParserImpl.xparser

    /// Represents the result of the parse operation
    type ParseResult(expr: Expr option, msg: string option) =

        /// Gets the expression as an option type
        member x.Expression = expr

        /// Gets the parser error message as an option type
        member x.ParseError = msg

        /// Returns a value indicating whether the parse result is OK or not.
        member x.Ok = expr.IsSome

    /// Parse the given expression, returning a ParseResult
    let ParseString expr = 
        match run parser expr with
        | Success(expr, _, _) -> ParseResult(Some(expr), None)
        | Failure(msg, _, _)  -> ParseResult(None, Some(msg))