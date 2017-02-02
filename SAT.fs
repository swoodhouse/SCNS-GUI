// code adapted from Z3Fs - https://github.com/dungpa/Z3Fs

module SAT

open System.Collections.Generic
open Microsoft.Z3
open System

[<AbstractClass>]
type Theory() =
    abstract member Expr: Expr

type Bool(e: BoolExpr) =
    inherit Theory()
    override x.Expr = e :> Expr
    override x.ToString() = sprintf "%O" e
    static member FromExpr (e: Expr) = Bool(e :?> BoolExpr)

let BoolExpr expr = Bool(expr)
let (|BoolExpr|) (b: Bool) = b.Expr :?> BoolExpr

type BitVec(expr: BitVecExpr) =
    inherit Theory()
    override x.Expr = expr :> Expr
    override x.ToString() = sprintf "%O" expr
    static member FromExpr (e: Expr) = BitVec(e :?> BitVecExpr)

let BitVecExpr expr = BitVec(expr)
let (|BitVecExpr|) (bv: BitVec) = bv.Expr :?> BitVecExpr

type SATSolver() = class
  let mutable context = Some <| new Context(Dictionary())
  let mutable solver = Some <| (Option.get context).MkSolver("QF_BV")

  let mkBool b = (Option.get context).MkBool(b)
  let mkAnd x y = (Option.get context).MkAnd(x, y) |> BoolExpr
  let mkOr x y = (Option.get context).MkOr(x, y) |> BoolExpr
  let mkNot x = (Option.get context).MkNot(x) |> BoolExpr
  let mkImplies x y = (Option.get context).MkImplies(x, y) |> BoolExpr
  let mkEquiv x y = (Option.get context).MkEq(x, y) |> BoolExpr
  let mkTrue() = (Option.get context).MkTrue() |> BoolExpr
  let mkFalse() = (Option.get context).MkFalse() |> BoolExpr

  let mkITE b expr1 expr2 = (Option.get context).MkITE(b, expr1, expr2) :?> BoolExpr |> BoolExpr

  let mkBitVec (v: int) (size: uint32) = (Option.get context).MkBV(v, size)
  let mkAdd x y = (Option.get context).MkBVAdd(x, y) |> BitVecExpr
  let mkEq x y = (Option.get context).MkEq(x, y) |> BoolExpr
  let mkGe x y = (Option.get context).MkBVSGE(x, y) |> BoolExpr
  let mkNotEq x y = (Option.get context).MkDistinct(x, y) |> BoolExpr
  let mkLe x y = (Option.get context).MkBVSLE(x, y) |> BoolExpr

  member x.Bool (name : string) =
    (Option.get context).MkBoolConst name |> BoolExpr

  member x.True = mkTrue()
  member x.False = mkFalse()  
  member x.And (BoolExpr p, BoolExpr q) = mkAnd p q
  member x.And (BoolExpr p, q) = mkAnd p (mkBool q)
  member x.And (p, BoolExpr q) = mkAnd (mkBool p) q
  member x.Or (BoolExpr p, BoolExpr q) = mkOr p q
  member x.Or (BoolExpr p, q) = mkOr p (mkBool q)
  member x.Or (p, BoolExpr q) = mkOr (mkBool p) q
  member x.Not (BoolExpr p) = mkNot p
  member x.Implies (BoolExpr p, BoolExpr q) = mkImplies p q
  member x.Implies (BoolExpr p, q) = mkImplies p (mkBool q)
  member x.Implies(p, BoolExpr q) = mkImplies (mkBool p) q
  member x.Eq(BoolExpr p, BoolExpr q) = mkEquiv p q
  member x.Eq(BoolExpr p, q) = mkEquiv p (mkBool q)
  member x.Eq(p, BoolExpr q) = mkEquiv (mkBool p) q
  member x.NotEq (BoolExpr p, BoolExpr q) = mkNotEq p q
  member x.NotEq (BoolExpr p, q) = mkNotEq p (mkBool q)
  member x.NotEq (p, BoolExpr q) = mkNotEq (mkBool p) q
  member x.If(BoolExpr b, BoolExpr expr1, BoolExpr expr2) = mkITE b expr1 expr2
 
  member x.And args =
    let f (a : Bool) (b : Bool) = x.And (a, b)
    Seq.reduce f args
    
  member x.Or args =
    let f (a : Bool) (b : Bool) = x.Or (a, b)
    Seq.reduce f args

  member x.BitVec (name : string, size : uint32) =
    (Option.get context).MkBVConst(name, size) |> BitVecExpr

  member s.Plus(BitVecExpr x, BitVecExpr y) = mkAdd x y
  member s.Plus(BitVecExpr x, y) = mkAdd x (mkBitVec y x.SortSize)
  member s.Plus(x, BitVecExpr y) = mkAdd (mkBitVec x y.SortSize) y
  member s.Eq(BitVecExpr x, BitVecExpr y) = mkEq x y
  member s.Eq(BitVecExpr x, y) = mkEq x (mkBitVec y x.SortSize)
  member s.Eq(x, BitVecExpr y) = mkEq (mkBitVec x y.SortSize) y
  member s.Ge(BitVecExpr x, BitVecExpr y) = mkGe x y
  member s.Ge(BitVecExpr x, y) = mkGe x (mkBitVec y x.SortSize)
  member s.Ge(x, BitVecExpr y) = mkGe (mkBitVec x y.SortSize) y
  member s.NotEq(BitVecExpr x, BitVecExpr y) = mkNotEq x y
  member s.NotEq(BitVecExpr x, y) = mkNotEq x (mkBitVec y x.SortSize)
  member s.NotEq(x, BitVecExpr y) = mkNotEq (mkBitVec x y.SortSize) y
  member s.Le(BitVecExpr x, BitVecExpr y) = mkLe x y
  member s.Le(BitVecExpr x, y) = mkLe x (mkBitVec y x.SortSize)
  member s.Le(x, BitVecExpr y) = mkLe (mkBitVec x y.SortSize) y

  member x.Add([<ParamArray>] xs: _ []) =
      for (BoolExpr expr) in xs do
          (Option.get solver).Assert expr

  member x.Check() =
      (Option.get solver).Check()
  member x.Model =
      (Option.get solver).Model
  member x.Reset() =
      (Option.get solver).Reset()

  interface IDisposable with
      member x.Dispose() =
          x.Reset()
          (Option.get solver).Dispose()
          (Option.get context).Dispose()
          solver <- None
          context <- None
          
  member x.Dispose() = (x :> IDisposable).Dispose()
end

let inline (++) xs ys = Array.append xs ys 

type Result =
| Const of Expr
| Func of FuncInterp
with override x.ToString() =
      match x with
      | Const expr -> sprintf "%O" expr
      | Func f -> sprintf "%O" f

type Microsoft.Z3.Model with
    member x.Item (index: Expr) = x.Eval(index, true)
    member x.Item (index: FuncDecl) =
        if index.DomainSize = 0u && index.Range.SortKind <> Z3_sort_kind.Z3_ARRAY_SORT
        then x.ConstInterp(index) |> Const
        else x.FuncInterp(index) |> Func
    member x.Evaluate(v: Theory, ?modelCompletion) =
        x.Evaluate(v.Expr, defaultArg modelCompletion false)
