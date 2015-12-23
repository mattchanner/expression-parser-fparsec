namespace MC.Expression

open FParsec.CharParsers
open MC.Expression.Ast
open MC.Expression.Parser

[<AutoOpen>]
module Program =
    
    type TestResult = {
        Passed: int
        Failed: int
        FailedExpressions: string list
    }

    let mutable results = { Passed = 0; Failed = 0; FailedExpressions = List.empty }

    let passed() = 
        let current = results
        results <- { current with Passed = (current.Passed + 1) }
        ()

    let failed(expr: string) = 
        let current = results
        results <- { current with Failed = (current.Failed + 1); FailedExpressions = expr :: current.FailedExpressions }
        ()

    let summarize() =
        printfn "****************************************\r\n"
        printfn " Test Summary"
        printfn " Passed: %A" results.Passed
        printfn " Failed: %A" results.Failed
        printfn " Failed Expressions: "
        results.FailedExpressions |> List.iter (fun x -> printfn " %O" x)
        printfn "****************************************\r\n"

    /// Helper used for parsing a given expression and printing the result.
    let pass (exprString: string) expected =
        let result = Parser.ParseString exprString
        match (result.Expression, result.ParseError) with
        | (Some(expr), None) when expr = expected -> printfn "Success:%s = %O" exprString expr; passed()
        | (Some(expr), None) -> printfn "Failure for  %s %A" (expected.ToString()) expr; failed(exprString)
        | (None, Some(err))  -> printfn "Failure: %s" err; failed(exprString)
        | _                  -> printfn "Unknown error"; failed(exprString)

    let doesNotError expr =
        let result = Parser.ParseString expr
        if result.Ok then 
            printfn "Success: %s = %O" expr result.Expression.Value
            passed()
        else 
            printfn "Failure: %s" result.ParseError.Value
            failed(result.ParseError.Value)

    let fail (exprString) =
        let result = Parser.ParseString exprString
        match (result.Expression, result.ParseError) with
        | (Some(expr), None) -> printfn "Expected failure, got success!: %A" expr; failed(exprString)
        | (None, Some(err))  -> printfn "Correct Parse Error for string %s" exprString; passed()
        | _                  -> printfn "Unknown error"; failed(exprString)

    [<EntryPoint>]
    let main (args: string array) =

        VisitorTests.test()
       
        printf "Expected Failures\n"
        printf "*******************************************\n"
        fail "let x"
        fail "let x = "
        fail "let x"
        fail "if(1 + 2, 3)"
        fail "if(1 + 2)"
        fail "fn)("
        fail "filter(1)"
        fail "filter(1, 'not a lamda')"
        fail "foreach(1)"
        fail "foreach(1, 'not a lamda')"

        printf "\r\n"
        printf "Expected Passes\n"
        printf "*******************************************\n"

        pass "let x = 10" (Assign("x", Literal(Float(10.0))))
        pass "let x = 10 * 20" (Assign("x", Arithmetic(Literal(Float(10.0)), Multiply, Literal(Float(20.0)))))

        doesNotError "(1 + 2) * 3"
        doesNotError "-2 - -3"
        doesNotError "average(list(1,2,3) + array(4,5,6)) + [var]"
        doesNotError "fn( 3 * x( 1, -( 2 - 1)))"
        doesNotError "foreach([History_Count], {if(or(item0 = '', item0 = 0,item0 = -1, item0 = 1), '', cdbl(item0))})"
        doesNotError "a([c] + -1)"
        doesNotError "fn([a], [b], fn2(1, 2 + 3))"

        pass "filter([var], {item % 2 = 0})"
             (Filter(Var("var"), Lamda([Comparison(Arithmetic(Ident("item"), Modulo, Literal(Float(2.0))), Eq, Literal(Float(0.0)))])))

        pass "{item1 * Power( [Dilution], -( item0 - 1))}"
             (Lamda([Arithmetic(Ident("item1"), 
                                Multiply, 
                                Func("Power", 
                                     Args([Var("Dilution"); 
                                           UnaryOp(Minus, 
                                                   Arithmetic(Ident("item0"), 
                                                              Minus, 
                                                              Literal(Float(1.0))))])))]))

        pass "{1 * 2, 3}" (Lamda([Arithmetic(Literal(Float(1.0)), Multiply,Literal(Float(2.0))); Literal(Float(3.0))]))
        pass "{2 <> 3, 1 > 2}" (Lamda([Comparison(Literal(Float(2.0)), NotEq, Literal(Float(3.0)));Comparison(Literal(Float(1.0)),GT,Literal(Float(2.0)))]))
        pass "((0 + (6 / ( 1 +2 ) )- 123456 ) / 2+123 + 877) * 3^2 / 3" 
            (Arithmetic(Arithmetic
                (Arithmetic
                    (Arithmetic
                        (Arithmetic
                            (Arithmetic
                                (Arithmetic(Literal(Float(0.0)),
                                            Plus, 
                                            Arithmetic(Literal(Float(6.0)),
                                                       Divide,
                                                       Arithmetic(Literal(Float(1.0)),
                                                                  Plus,
                                                                  Literal(Float(2.0))))), 
                                 Minus, 
                                 Literal(Float(123456.0))),
                            Divide,
                            Literal(Float(2.0))),
                        Plus,
                        Literal(Float(123.0))),
                    Plus,
                    Literal(Float(877.0))),
                    Multiply,
                    Arithmetic (Literal(Float(3.0)),
                                Power,
                                Literal(Float(2.0)))),
                    Divide, 
                    Literal(Float(3.0))))

        pass "(item0 * item1)"
            (Arithmetic(Ident("item0"), Multiply, Ident("item1")))

        pass "{'var1', 'var2', ('item0' * 'item1')}" 
             (Lamda([Literal(String(Single, "var1"))
                     Literal(String(Single, "var2"))
                     Arithmetic(Literal(String(Single, "item0")), 
                                Multiply, 
                                Literal(String(Single, "item1")))]))

        pass "1 # a comment" (Literal(Float(1.0)))

        pass "filter(array(1, 2, 3), {item % 2 = 0})"
             (Filter(Func("array", 
                          Args([Literal(Float(1.0))
                                Literal(Float(2.0))
                                Literal(Float(3.0))])), 
                     Lamda([Comparison(Arithmetic(Ident("item"), Modulo, Literal(Float(2.0))),
                                       Eq,
                                       Literal(Float(0.0)))])))

        pass "foreach([var1], [var2], {10 + 20 / 2})" 
             (ForEach(Args([Var("var1")
                            Var("var2")]), 
                      Lamda([Arithmetic(Literal(Float(10.0)), 
                                      Plus, 
                                      Arithmetic(Literal(Float(20.0)),
                                               Divide,
                                               Literal(Float(2.0))))])))


        // primitives
        pass "1.0" (Literal(Float 1.0))
        pass "1E2" (Literal(Float 100.0))
        pass "\"a string\"" (Literal(String(Double, "a string")))
        pass "'a string'" (Literal(String(Single, "a string")))
        pass @"""an escaped \r string""" (Literal(String(Double, "an escaped \r string")))
        pass @"'an escaped \r string'" (Literal(String(Single, "an escaped \r string")))
        pass "true" (Literal(Bool true))
        pass "false" (Literal(Bool false))
        pass "[a variable]" (Var "a variable")
        pass "'thing 1' & 'thing 2'" (Arithmetic(Literal(String(Single, "thing 1")), Concat, Literal(String(Single, "thing 2"))))
        
        // functions
        pass "func(1, true)"   (Func("func", Args([Literal(Float(1.0)); Literal(Bool(true))])))
        pass "func ( 1, true)" (Func("func", Args([Literal(Float(1.0)); Literal(Bool(true))])))
        pass "func()"          (Func("func", Args(List.empty)))

        // operators
        pass "-1.0" (UnaryOp(Minus, Literal(Float 1.0)))
        pass "+1.0" (Literal(Float 1.0))
        pass "1.0 + 2.0" (Arithmetic(Literal(Float 1.0), Plus, Literal(Float 2.0)))

        pass "1 + 2 / 3" (Arithmetic(Literal(Float(1.0)), 
                                   Plus, 
                                   Arithmetic(Literal(Float(2.0)), 
                                            Divide, 
                                            Literal(Float(3.0)))))

        // logical
        fail "not()"
        pass "not(1 = 2)" (Not(Args([Comparison(Literal(Float(1.0)), Eq, Literal(Float(2.0)))])))
        pass "or(1 >= 2, true, false)" (Or(Args([Comparison(Literal(Float(1.0)), GTEq, Literal(Float(2.0))); Literal(Bool(true)); Literal(Bool(false))])))
        pass "and(1 <= 2, true, false)" (And(Args([Comparison(Literal(Float(1.0)), LTEq, Literal(Float(2.0))); Literal(Bool(true)); Literal(Bool(false))])))
        pass "if(1 > 2, 'hello', \"goodbye\")" (IfThenElse(Comparison(Literal(Float(1.0)), 
                                                                    GT, 
                                                                    Literal(Float(2.0))), 
                                                           Literal(String(Single, "hello")), 
                                                           Literal(String(Double, "goodbye"))))

        // functions with more complex expressions
        pass "fn(1 + 2, 3/2)" (Func("fn", 
                                    Args([Arithmetic(Literal(Float(1.0)), 
                                                   Plus, 
                                                   Literal(Float(2.0))); 
                                          Arithmetic(Literal(Float(3.0)), 
                                                   Divide, 
                                                   Literal(Float(2.0)))])))

        pass "fn(1, 3/2)" (Func("fn", 
                                Args([Literal(Float(1.0)); 
                                      Arithmetic(Literal(Float(3.0)), 
                                               Divide, 
                                               Literal(Float(2.0)))])))

        // Lamda implicit args
        pass "{1, 2, 3}" (Lamda([Literal(Float(1.0)); Literal(Float(2.0)); Literal(Float(3.0))]))
        pass "{1, 'a', [var], true}" (Lamda([Literal(Float(1.0)); Literal(String(Single, "a")); Var("var"); Literal(Bool(true))]))   

        // *should* allow for multiline expressions
        pass @"1 +
                2" (Arithmetic(Literal(Float(1.0)), Plus, Literal(Float(2.0))))

        pass @"1 +
                2 # trailing comment" (Arithmetic(Literal(Float(1.0)), Plus, Literal(Float(2.0))))

        pass "1 + 2 # a comment\r\n / 20" (Arithmetic(Literal(Float(1.0)), Plus, Arithmetic(Literal(Float(2.0)), Divide, Literal(Float(20.0)))))

        summarize()
        System.Console.ReadKey() |> ignore
        0