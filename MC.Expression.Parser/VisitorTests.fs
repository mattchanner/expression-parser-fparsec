namespace MC.Expression

open MC.Expression
open MC.Expression.Ast
open MC.Expression.Visitors

module VisitorTests =

    let test() =
        let v = { new Visitor<unit>() with 
            override x.VisitString q v () =
                printfn "%O%s%O\r\n" q v q

            override x.VisitArithmetic left op right () =
                printf "%O %O %O\r\n" left op right

            override x.VisitFloat f () =
                printf "%g\r\n" f
        }

        Visit v () <| Literal(String(QuoteType.Single, "a single quoted string"))
        Visit v () <| Arithmetic(Literal(Float(2.0)), Minus, Arithmetic(Literal(Float(3.0)), Power, Literal(Float(10.0))))

        printfn "\r\n"
