module Aether.Parsing

open Piglet.Parser
open Aether
open Aether.Geometry
open Aether.Transforms
open Aether.ParsingUtilities


[<AutoOpen>]
module Ast =
    
    type SceneFile = Directive list

    and ParamSet =
        | ParamSet of Param list

        member private this.Find(name : string, defaultValue : 'a) =
            let findValue (parameter : Param) =
                match parameter with
                | (name, v) when (v :? 'a = true) -> Some(v :?> 'a)
                | _ -> None

            match this with
            | ParamSet(parameters) ->
                match List.tryPick findValue parameters with
                | Some(v) -> v
                | _ -> defaultValue

        member private this.FindMany name findFunc =
            match this with
            | ParamSet(parameters) ->
                match List.tryPick findFunc parameters with
                | Some(v) -> v
                | _ -> []

        member this.FindString name defaultValue =
            this.Find<string>(name, defaultValue)

        member this.FindInt name defaultValue =
            this.Find<int>(name, defaultValue)

        member this.FindSingle name defaultValue =
            this.Find<single>(name, defaultValue)

        member this.FindSingles name =
            this.Find<single list>(name, [])

    and Param = string * obj

    and Directive =
        | StandardDirective of directiveType : StandardDirectiveType * implementationType : string * parameters : ParamSet option
        | Texture of name : string * textureType : string * textureClass : string * parameters : ParamSet option
        | Identity
        | Translate of Vector
        | Scale of Vector
        | Rotate of angle : single * axis : Vector
        | LookAt of eye : Point * lookAt : Point * up : Vector
        | CoordinateSystem of string
        | CoordSysTransform of string
        | Transform of Matrix4x4
        | ConcatTransform of Matrix4x4
        | WorldBegin
        | WorldEnd
        | AttributeBegin
        | AttributeEnd

    and StandardDirectiveType =
        | Film = 0
        | Camera = 1
        | PixelFilter = 2
        | Sampler = 3
        | LightSource = 4
        | Material = 6
        | Shape = 7


let configurator = ParserFactory.Configure<obj>()

// Non-terminals

let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())

let sceneFile             = nonTerminal<SceneFile>()
let directiveList         = nonTerminal<Directive list>()
let directive             = nonTerminal<Directive>()
let standardDirectiveType = nonTerminal<StandardDirectiveType>()
let paramSet              = nonTerminal<Param list>()
let param                 = nonTerminal<Param>()
let paramValueList        = nonTerminal<obj list>()
let paramValue            = nonTerminal<obj>()
let point3D               = nonTerminal<Point>()
let vector3D              = nonTerminal<Vector>()
let matrix3D              = nonTerminal<Matrix4x4>()
let floatOrInt            = nonTerminal<single>()

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

directive.AddProduction(standardDirectiveType, quotedString, paramSet).SetReduceFunction (fun a b c -> StandardDirective(a, b, Some(ParamSet(c))))
directive.AddProduction(standardDirectiveType, quotedString).SetReduceFunction (fun a b -> StandardDirective(a, b, None))
directive.AddProduction(terminal "Texture", quotedString, quotedString, quotedString, paramSet).SetReduceFunction (fun _ b c d e -> Texture(b, c, d, Some(ParamSet(e))))
directive.AddProduction(terminal "Texture", quotedString, quotedString, quotedString).SetReduceFunction (fun _ b c d -> Texture(b, c, d, None))
directive.AddProduction(terminal "Identity").SetReduceFunction (fun _ -> Identity)
directive.AddProduction(terminal "Translate", vector3D).SetReduceFunction (fun _ b -> Translate(b))
directive.AddProduction(terminal "Scale", vector3D).SetReduceFunction (fun _ b -> Scale(b))
directive.AddProduction(terminal "Rotate", floatOrInt, vector3D).SetReduceFunction (fun _ b c -> Rotate(b, c))
directive.AddProduction(terminal "LookAt", point3D, point3D, vector3D).SetReduceFunction (fun _ b c d -> LookAt(b, c, d))
directive.AddProduction(terminal "CoordinateSystem", quotedString).SetReduceFunction (fun _ b -> CoordinateSystem(b))
directive.AddProduction(terminal "CoordSysTransform", quotedString).SetReduceFunction (fun _ b -> CoordSysTransform(b))
directive.AddProduction(terminal "Transform", matrix3D).SetReduceFunction (fun _ b -> Transform(b))
directive.AddProduction(terminal "ConcatTransform", matrix3D).SetReduceFunction (fun _ b -> ConcatTransform(b))
directive.AddProduction(terminal "WorldBegin").SetReduceFunction (fun _ -> WorldBegin)
directive.AddProduction(terminal "WorldEnd").SetReduceFunction (fun _ -> WorldEnd)
directive.AddProduction(terminal "AttributeBegin").SetReduceFunction (fun _ -> AttributeBegin)
directive.AddProduction(terminal "AttributeEnd").SetReduceFunction (fun _ -> AttributeEnd)

standardDirectiveType.AddProduction(terminal "Film")       .SetReduceFunction (fun _ -> StandardDirectiveType.Film)
standardDirectiveType.AddProduction(terminal "Camera")     .SetReduceFunction (fun _ -> StandardDirectiveType.Camera)
standardDirectiveType.AddProduction(terminal "PixelFilter").SetReduceFunction (fun _ -> StandardDirectiveType.PixelFilter)
standardDirectiveType.AddProduction(terminal "Sampler")    .SetReduceFunction (fun _ -> StandardDirectiveType.Sampler)
standardDirectiveType.AddProduction(terminal "LightSource").SetReduceFunction (fun _ -> StandardDirectiveType.LightSource)
standardDirectiveType.AddProduction(terminal "Material")   .SetReduceFunction (fun _ -> StandardDirectiveType.Material)
standardDirectiveType.AddProduction(terminal "Shape")      .SetReduceFunction (fun _ -> StandardDirectiveType.Shape)

paramSet.AddProduction(paramSet, param).SetReduceFunction (fun a b -> a @ [b])
paramSet.AddProduction(param).SetReduceFunction (fun a -> [a])

let getParam (typeAndName : string) (value : obj) =
    let splitNameAndType = typeAndName.Split(' ')
    let paramType = splitNameAndType.[0]
    let paramName = splitNameAndType.[1]

    let getThreeSingles() =
        let list = unbox<obj list> value
        match list with
        | [ x; y; z ] when x :? single = true && y :? single = true && z :? single = true ->
            (unbox x, unbox y, unbox z)
        | _ -> failwith "Expected three floats in an array."

    let newValue =
        match paramType with
        | "integer" ->
            match value with
            | :? single as i -> box (int i)
            | _ -> value
        | "point"  -> box (Point(getThreeSingles()))
        | "vector" -> box (Vector(getThreeSingles()))
        | "normal" -> box (Normal(getThreeSingles()))
        | "rgb"    ->
            let (r, g, b) = getThreeSingles()
            box (Spectrum([| r; g; b |]))
        | _ -> value

    (paramName, newValue)

let getParamList typeAndName (value : obj list) =
    let newValue = if List.length value = 1 then List.head value else box value
    getParam typeAndName newValue

param.AddProduction(quotedString, terminal "\[", paramValueList, terminal "\]").SetReduceFunction (fun a _ c _ -> getParamList a c)
param.AddProduction(quotedString, paramValue).SetReduceFunction (fun a b -> getParam a b)

paramValueList.AddProduction(paramValueList, paramValue).SetReduceFunction (fun a b -> a @ [b])
paramValueList.AddProduction(paramValue).SetReduceFunction (fun a -> [a])

paramValue.AddProduction(floatOrInt).SetReduceFunction (fun a -> box a)
paramValue.AddProduction(quotedString).SetReduceFunction (fun a -> box a)

point3D.AddProduction(floatOrInt, floatOrInt, floatOrInt).SetReduceFunction (fun a b c -> Point(a, b, c))
vector3D.AddProduction(floatOrInt, floatOrInt, floatOrInt).SetReduceFunction (fun a b c -> Vector(a, b, c))
matrix3D.AddProduction(floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt, floatOrInt)
    .SetReduceFunction (fun m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33 -> Matrix4x4(m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33))
floatOrInt.AddProduction(floatLiteral).SetReduceToFirst()
floatOrInt.AddProduction(integerLiteral).SetReduceFunction (fun a -> single a)

// Ignore whitespace and comments
configurator.LexerSettings.Ignore <- [| @"\s+"; @"#[^\n]*\n" |]

let parser = configurator.CreateParser()


type ParserException(message : string) =
    inherit System.Exception(message)


[<AutoOpen>]
module ParserErrors =
    let create m = ParserException m

    let lexerError a                  = create (sprintf "Lexer error: %s" a)
    let parserError a                 = create (sprintf "Parser error: %s" a)


module Parser =
    let parse (s : string) =
        try
            parser.Parse(s) :?> SceneFile
        with
            | :? Piglet.Lexer.LexerException as ex ->
                raise (lexerError ex.Message)
            | :? Piglet.Parser.ParseException as ex ->
                raise (parserError ex.Message)