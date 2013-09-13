module Aether.ParsingUtilities

open Piglet.Parser
open Piglet.Parser.Configuration


type ProductionWrapperBase (production : IProduction<obj>) =
    member x.Production = production
    member x.SetReduceToFirst () = production.SetReduceToFirst()
    member x.SetPrecedence(precedenceGroup) = production.SetPrecedence(precedenceGroup)

type ProductionWrapper<'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : (unit -> 'T)) =
        production.SetReduceFunction (fun o -> box (f ()))

type ProductionWrapper<'a,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])))

type ProductionWrapper<'a,'b,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0]) (unbox o.[1])))

type ProductionWrapper<'a,'b,'c,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])))

type ProductionWrapper<'a,'b,'c,'d,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])))

type ProductionWrapper<'a,'b,'c,'d,'e,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])))

type ProductionWrapper<'a,'b,'c,'d,'e,'f,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])))

type ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])
                                                      (unbox o.[6])))

type ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'T> (production : IProduction<obj>) =
    inherit ProductionWrapperBase(production)
    member x.SetReduceFunction (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'T)) =
        production.SetReduceFunction (fun o -> box (f (unbox o.[0])
                                                      (unbox o.[1])
                                                      (unbox o.[2])
                                                      (unbox o.[3])
                                                      (unbox o.[4])
                                                      (unbox o.[5])
                                                      (unbox o.[6])
                                                      (unbox o.[7])
                                                      (unbox o.[8])
                                                      (unbox o.[9])
                                                      (unbox o.[10])
                                                      (unbox o.[11])
                                                      (unbox o.[12])
                                                      (unbox o.[13])
                                                      (unbox o.[14])
                                                      (unbox o.[15])))

type SymbolWrapper<'T> (symbol : ISymbol<obj>) =
    member x.Symbol = symbol

type TerminalWrapper<'T> (terminal : ITerminal<obj>) =
    inherit SymbolWrapper<'T>(terminal)

type NonTerminalWrapper<'T> (nonTerminal : INonTerminal<obj>) =
    inherit SymbolWrapper<'T>(nonTerminal)

    member x.AddProduction () =
        let production = nonTerminal.AddProduction()
        new ProductionWrapper<'T>(production)
    
    member x.AddProduction (part : SymbolWrapper<'a>) =
        let production = nonTerminal.AddProduction(part.Symbol)
        new ProductionWrapper<'a,'T>(production)
    
    member x.AddProduction((part1 : SymbolWrapper<'a>), (part2 : SymbolWrapper<'b>)) =
        let production = nonTerminal.AddProduction(part1.Symbol, part2.Symbol)
        new ProductionWrapper<'a,'b,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol)
        new ProductionWrapper<'a,'b,'c,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>),
                           (part6 : SymbolWrapper<'f>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'f,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>),
                           (part6 : SymbolWrapper<'f>),
                           (part7 : SymbolWrapper<'g>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol,
                                                   part7.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'T>(production)

    member x.AddProduction((part1 : SymbolWrapper<'a>),
                           (part2 : SymbolWrapper<'b>),
                           (part3 : SymbolWrapper<'c>),
                           (part4 : SymbolWrapper<'d>),
                           (part5 : SymbolWrapper<'e>),
                           (part6 : SymbolWrapper<'f>),
                           (part7 : SymbolWrapper<'g>),
                           (part8 : SymbolWrapper<'h>),
                           (part9 : SymbolWrapper<'i>),
                           (part10 : SymbolWrapper<'j>),
                           (part11 : SymbolWrapper<'k>),
                           (part12 : SymbolWrapper<'l>),
                           (part13 : SymbolWrapper<'m>),
                           (part14 : SymbolWrapper<'n>),
                           (part15 : SymbolWrapper<'o>),
                           (part16 : SymbolWrapper<'p>)) =
        let production = nonTerminal.AddProduction(part1.Symbol,
                                                   part2.Symbol,
                                                   part3.Symbol,
                                                   part4.Symbol,
                                                   part5.Symbol,
                                                   part6.Symbol,
                                                   part7.Symbol,
                                                   part8.Symbol,
                                                   part9.Symbol,
                                                   part10.Symbol,
                                                   part11.Symbol,
                                                   part12.Symbol,
                                                   part13.Symbol,
                                                   part14.Symbol,
                                                   part15.Symbol,
                                                   part16.Symbol)
        new ProductionWrapper<'a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'T>(production)