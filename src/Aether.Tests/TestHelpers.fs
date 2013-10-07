[<AutoOpen>]
module TestHelpers

open FsUnit.Xunit
open Aether.Transforms


let checkArraysRoughlyMatch (a1 : single[]) (a2 : single[]) =
    for i = 0 to a1.Length-1 do
        a1.[i] |> should (equalWithin 0.01f) a2.[i]

let checkListsRoughlyMatch (a1 : 'a list) (a2 : 'a list) =
    for i = 0 to a1.Length-1 do
        a1.[i] |> should (equalWithin 0.01f) a2.[i]

let checkMatricesRoughlyMatch (m1 : Matrix4x4) (m2 : Matrix4x4) =
    for i = 0 to 3 do
        for j = 0 to 3 do
            m1.[i,j] |> should (equalWithin 0.01f) m2.[i,j]