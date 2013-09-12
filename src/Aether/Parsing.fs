module Aether.Parsing

open Nexus
open Piglet.Parser
open Aether.ParsingUtilities


[<AutoOpen>]
module Ast =
    
    type SceneFile = Directive list

    and ParamSet = Param list

    and Param = string * ParamValue

    and ParamValue =
        | IntegerValue of int
        | FloatValue of single
        | PointValue of Point3D
        | VectorValue of Vector3D
        | NormalValue of Normal3D
        | SpectrumValue // TODO
        | BoolValue of bool
        | StringValue of string
        | ArrayValue of ParamValue list

    and Directive =
        | LookAtDirective of eye : Point3D * lookAt : Point3D * up : Vector3D
        | FilmDirective of filmType : string * parameters : ParamSet


let configurator = ParserFactory.Configure<obj>()

// Non-terminals

let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())

let sceneFile       = nonTerminal<SceneFile>()
let directiveList   = nonTerminal<Directive list>()
let directive       = nonTerminal<Directive>()
let paramSet        = nonTerminal<ParamSet>()
let param           = nonTerminal<Param>()
let paramValueList  = nonTerminal<ParamValue list>()
let paramValue      = nonTerminal<ParamValue>()
let point3D         = nonTerminal<Point3D>()
let vector3D        = nonTerminal<Vector3D>()

// Terminals

let terminalParse<'T> regex (onParse : (string -> 'T)) =
    new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

let terminal regex = new TerminalWrapper<string>(configurator.CreateTerminal(regex))

let floatLiteral   = terminalParse @"-?\d*\.\d+"        (fun s -> single s)
let integerLiteral = terminalParse @"-?\d+"             (fun s -> int s)
let quotedString   = terminalParse "\"(\\\\.|[^\"])*\"" (fun s -> s.Substring(1, s.Length - 2))

// Productions

sceneFile.AddProduction(directiveList).SetReduceToFirst()

directiveList.AddProduction(directiveList, directive).SetReduceFunction (fun a b -> a @ [b])
directiveList.AddProduction(directive).SetReduceFunction (fun a -> [a])

directive.AddProduction(terminal "LookAt", point3D, point3D, vector3D).SetReduceFunction (fun _ b c d -> LookAtDirective(b, c, d))
directive.AddProduction(terminal "Film", quotedString, paramSet).SetReduceFunction (fun _ b c -> FilmDirective(b, c))

paramSet.AddProduction(paramSet, param).SetReduceFunction (fun a b -> a @ [b])
paramSet.AddProduction(param).SetReduceFunction (fun a -> [a])

let getParam (typeAndName : string) (value : ParamValue) =
    let splitNameAndType = typeAndName.Split(' ')
    let paramType = splitNameAndType.[0]
    let paramName = splitNameAndType.[1]

    let newValue = match paramType with
                   | "float" ->
                        match value with
                        | IntegerValue(i) -> FloatValue(single i)
                        | _ -> value
                   | "integer" ->
                        match value with
                        | FloatValue(f) -> IntegerValue(int f)
                        | _ -> value
                   | _ -> value

    (paramName, newValue)

let getParamList typeAndName (value : ParamValue list) =
    let newValue = if List.length value = 1 then List.head value else ArrayValue(value)
    getParam typeAndName newValue

param.AddProduction(quotedString, terminal "\[", paramValueList, terminal "\]").SetReduceFunction (fun a _ c _ -> getParamList a c)
param.AddProduction(quotedString, paramValue).SetReduceFunction (fun a b -> getParam a b)

paramValueList.AddProduction(paramValueList, paramValue).SetReduceFunction (fun a b -> a @ [b])
paramValueList.AddProduction(paramValue).SetReduceFunction (fun a -> [a])

paramValue.AddProduction(floatLiteral).SetReduceFunction (fun a -> FloatValue(a))
paramValue.AddProduction(integerLiteral).SetReduceFunction (fun a -> IntegerValue(a))
paramValue.AddProduction(quotedString).SetReduceFunction (fun a -> StringValue(a))



point3D.AddProduction(floatLiteral, floatLiteral, floatLiteral)
    .SetReduceFunction (fun a b c -> Point3D(a, b, c))
vector3D.AddProduction(floatLiteral, floatLiteral, floatLiteral)
    .SetReduceFunction (fun a b c -> Vector3D(a, b, c))

// Ignore whitespace and comments
configurator.LexerSettings.Ignore <- [| @"\s+"; @"#[^\n]*\n" |]

let parser = configurator.CreateParser()

module Parser =
    let parse (s : string) =
        try
            parser.Parse(s) :?> SceneFile
        with
            | :? Piglet.Lexer.LexerException as ex ->
                failwith ex.Message
            | :? Piglet.Parser.ParseException as ex ->
                failwith ex.Message