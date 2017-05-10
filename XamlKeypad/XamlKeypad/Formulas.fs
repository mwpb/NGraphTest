namespace Formulas
open System
open System.Diagnostics

// add a vector type and a 2D vector type (Formula,Formula
// consider checked arithmetic

[<AutoOpen>]
module HelperFunctions =
    let maybeInt (x:float) =
        let y = Convert.ToInt32 x
        if x = float y then Some(y) else None
    let approxEq (x:float) y = (Math.Abs(x-y) < 0.0001)
    let veryApproxEq (x:float) y = (Math.Abs(x-y) < 0.000001)
    let veryClose a b =
        let absA, absB, diff = abs a, abs b, abs (a-b)
        a = b || (absA < 1E-12 && absB < 1E-12) || diff/(absA+absB) < 1E-12

    let modulo n m = ((n % m) + m) % m // n % m sometimes returns a negative number
    
    let rec gcd =
        function
        | (0,x) | (x,0) -> Math.Abs x
        | (a,b) -> gcd(b, modulo a b)

    let delete (m:'a[,]) i j =
        Array2D.init (m.GetLength(0)-1) (m.GetLength(1)-1) (fun r c -> m.[(if r<i then r else r+1),(if c<j then c else c+1)])

    let rec fact n =
        if n < 0 then failwith "negative factorial input"
        elif n = 0 then 1 else n * fact (n-1)

//type Matrix<'T when 'T : (static member Zero : 'T) and 'T : (static member One : 'T) and 'T : (static member (+) : 'T * 'T -> 'T) and 'T : (static member (-) : 'T * 'T -> 'T) and 'T : (static member (*) : 'T * 'T -> 'T) and 'T : (static member (/) : 'T * 'T -> 'T)>

type Bivariate =
    | Plus | Minus | Times | Divide | Power | LogBase //add root
    member t.FloatFunc : float * float -> float =
        match t with
        | Plus -> fun (x,y) -> x+y
        | Minus -> fun (x,y) -> x-y
        | Times -> fun (x,y) -> x*y
        | Divide -> fun (x,y) -> x/y
        | Power -> Math.Pow
        | LogBase -> Math.Log

type Univariate =
    | Log  | Exp  | Sqrt | Neg  | Abs  | Fact
    | Sin  | Cos  | Tan  | Asin | Acos | Atan | Csc  | Sec  | Cot
    | Sinh | Cosh | Tanh
    override t.ToString() = //LaTeX
        match t with
        | Log  -> "log" | Exp -> "exp" | Sin -> "sin" | Cos -> "cos" | Tan -> "tan" | Asin -> "arcsin"
        | Acos -> "arccos" | Atan -> "arctan" | Csc -> "csc"  | Sec -> "sec"  | Cot -> "cot"
        | Sinh -> "sinh" | Cosh -> "cosh" | Tanh -> "tanh"
        | Neg  -> "neg"   | Abs -> "abs"   | Fact -> "fact" | Sqrt -> "sqrt" 
    member t.FloatFunc: float -> float =
        match t with
        | Log  -> log  | Exp  -> exp  | Sqrt -> sqrt
        | Neg  -> (~-) | Abs  -> abs
        | Sin  -> sin  | Cos  -> cos  | Tan  -> tan
        | Asin -> asin | Acos -> acos | Atan -> atan
        | Csc  -> fun x -> 1./(sin x)
        | Sec  -> fun x -> 1./(cos x)
        | Cot  -> fun x -> (cos x)/(sin x)
        | Sinh -> sinh | Cosh -> cosh | Tanh -> tanh
        | Fact -> fun x -> match maybeInt x with Some(n) -> (float) (fact n) | None -> Double.NaN // can use non-integer definition

type Relation = Equals | LEq | Less

type Result = //add complex // float can be NaN
    | FL of float
    | RA of int * int // second positive, no common factor
    | BL of bool
    | MAT of Result[,]
    | ERR
    override t.ToString() =
        match t with
        | FL f -> "FL "+ f.ToString()
        | RA(a,b) -> "RA "+ a.ToString() + "/" + b.ToString()
        | ERR -> "ERR"
        | BL b -> "BL " + b.ToString()
        | MAT m -> "MAT " + m.ToString()

[<AutoOpen>]
///constructors with checked and simplified inputs
module Constructors =
    let simpRA(a,b) =
        let g = gcd(a, b)
        if b>0 then (a/g,b/g) else (-a/g,-b/g)
    let ra = function (_,0) -> ERR | x -> x |> simpRA |> RA
    let fl (f:float) = if Double.IsNaN(f) || Double.IsNegativeInfinity(f) || Double.IsPositiveInfinity(f) then ERR else FL(f)
    let (|AsFloat|_|) r =
        match r with
        | FL f -> Some(f)
        | RA(a,b) -> Some((float a)/(float b))
        | _ -> None
    let (|AsInt|_|) r =
        match r with
        | RA(a,1) -> Some a
        | FL(x)   -> maybeInt x
        | _       -> None

type Result with //error handling for RAs (e.g. int overflow exception)

    member t.Det =
        match t with
        | MAT(M)  ->
            if M.GetLength(0)<>M.GetLength(1) then ERR
            elif M.GetLength(0)=1 then M.[0,0]
            else
                Array.init (M.GetLength(1)) (fun j -> (MAT(delete M 0 j)).Det * RA(pown (-1) j,1)) |> Array.sum
        | _       -> ERR
    ///Fails if not convertible to float!
    member t.toFl =
        match t with
        | FL x -> x
        | RA(a,b) -> (float a) / (float b)
        | _ -> failwith "not a float"
    member t.inverse =
        match t with
        | MAT(M)  -> 
            if M.GetLength(0)<>M.GetLength(1) then ERR
            else
                let d = t.Det
                let f i j = (MAT(delete M i j)).Det * RA(pown (-1) (i+j),1) / d
                Array2D.init (M.GetLength(0)) (M.GetLength(1)) f |> MAT
        | RA(a,b) -> if a=0 then ERR else simpRA(b,a) |> RA
        | FL(x)   -> if x=0. then ERR else 1./x |> FL
        | _       -> ERR
    static member IdentityMatrix n = Array2D.init n n (fun i j -> if i = j then RA(1,1) else RA(0,1)) |> MAT
    static member Zero = RA(0,1)
    static member approx x y = // true if approximately equal, false if not, false if not comparable
        match x,y with
        | MAT (A), MAT(B) ->
            if (A.GetLength(0), A.GetLength(1)) <> (B.GetLength(0), B.GetLength(1)) then false
            else Seq.forall2 Result.approx (Seq.cast<Result>(A)) (Seq.cast<Result>(B))
        | RA(a,b), RA(c,d) -> (a,b) = (c,d)
        | RA(a,b), AsFloat f | AsFloat f, RA(a,b) -> float (a/b) = f
        | AsFloat a, AsFloat b -> approxEq a b
        | _ -> false

    static member (~-) x =
        match x with
        | MAT(M)  -> M |> Array2D.map (fun x -> -x) |> MAT
        | RA(a,b) -> (-a,b) |> RA
        | FL(x)   -> FL(-x)
        | _       -> ERR
    static member (+) (x,y) =
        match x,y with
        | MAT(A) , MAT(B)  ->
            if (A.GetLength(0),A.GetLength(1)) = (B.GetLength(0),B.GetLength(1))
            then A |> Array2D.mapi (fun i j x -> x + B.[i,j]) |> MAT
            else ERR
        | RA(a,b), RA(c,d) -> ra(a*d+b*c,b*d)
        | AsFloat a, AsFloat b -> FL(a+b)
        | _ -> ERR
    static member (-) (x:Result,y:Result) = x + -y
    static member (*) (x,y) =
        match x,y with
        | MAT(A) , MAT(B)  ->
            if A.GetLength(1) <> B.GetLength(0) then ERR
            else
                let f i j = Array.init (A.GetLength(1)) (fun k -> A.[i,k]*B.[k,j]) |> Array.sum
                Array2D.init (A.GetLength(0)) (B.GetLength(1)) f |> MAT
        | MAT(A) , y | y, MAT(A) -> A |> Array2D.map (fun x -> x * y) |> MAT // is matrix * number legal?
        | RA(a,b), RA(c,d) -> simpRA(a*c,b*d) |> RA
        | AsFloat a, AsFloat b -> FL(a*b)
        | _ -> ERR
    static member (/) (x:Result,y:Result) =
        match y with
        | FL _ | RA _ -> x * y.inverse | _ -> ERR
    static member Pow (x:Result,y:Result) =
        match x,y with
        | MAT M , RA(c,1) ->
            if c = 0 then
                Result.IdentityMatrix (M.GetLength(1)) // If A has m columns then A^0 acts as the identity function on R^n.
            elif c>0 then x ** RA(c-1,1)
            else (x.inverse ** RA(-c,1))
        | RA(a,b), RA(c,1) ->
            if c>=0 then simpRA(pown a c, pown b c) |> RA else simpRA(pown b -c, pown a -c) |> RA
        | AsFloat a, RA(c,d) when a<0. -> if modulo d 2 = 0 then ERR else -((-x) ** y)
        | AsFloat a, AsFloat b -> a ** b |> fl // if a = 0. and b<0. or a<0. then a**b is nan and fl gives ERR
        | _ -> ERR
    
    static member Apply u (x:Result) =
        match u with 
        | Neg  -> -x
        | Abs  -> match x with | FL x -> FL (abs x)  | RA(a,b) -> RA(abs a, b) | _ -> ERR
        | Fact -> match x with | AsInt a when a >=0 -> RA(fact a, a) | _ -> ERR
        | _ -> match x with | AsFloat x -> x |> u.FloatFunc |> fl | _ -> ERR
    
    static member Log x = match x with AsFloat a -> fl(log a) | _ -> ERR

type Formula =
    | Int of int | Float of float
    | Variable of char
    | Mat of Formula[,]
    | Pi | E //add i
    | F1 of Univariate * Formula
    | F2 of Bivariate * Formula * Formula
    | Rel of Relation * Formula * Formula

    static member (~-) (x)  = F1(Neg,x)
    static member (+) (x,y) = F2(Plus,x,y)
    static member (-) (x,y) = F2(Minus,x,y)
    static member (*) (x,y) = F2(Times,x,y)
    static member (/) (x,y) = F2(Divide,x,y)
    static member Pow (x,y) = F2(Power,x,y)
    static member Sqrt x = F1(Sqrt,x)
    static member Log x = F1(Log,x)
    static member Exp x = F1(Exp,x)
    static member Abs x = F1(Abs,x)
    static member Sin x = F1(Sin,x)
    static member Cos x = F1(Cos,x)
    static member Tan x = F1(Tan,x)
    static member Asin x = F1(Asin,x)
    static member Acos x = F1(Acos,x)
    static member Atan x = F1(Atan,x)
    static member Sinh x = F1(Sinh,x)
    static member Cosh x = F1(Cosh,x)
    static member Tanh x = F1(Tanh,x)
    
    member t.Ev(args: Map<char,Result>) = //catch errors?
        match t with
        | Int a      -> RA(a,1)
        | Float a    -> FL a
        | Variable c -> args.[c]
        | Mat M      -> M |> Array2D.map (fun x -> x.Ev(args)) |> MAT
        | Pi         -> FL Math.PI
        | E          -> FL Math.E
        | F2(f,a,b)  ->
            match f, a.Ev(args),b.Ev(args) with
                | Plus,   a,b -> a+b
                | Minus,  a,b -> a-b
                | Times,  a,b -> a*b
                | Divide, a,b -> a/b
                | Power,  a,b -> a ** b
                | LogBase, x, y -> (log x) / (log y) // log_{y}(x)
        | F1(f,a) -> a.Ev(args) |> Result.Apply f
        | Rel(r,a,b) ->
            let inline rel x y = match r with | Equals -> x = y | LEq -> x <= y | Less -> x < y
            match a.Ev(args),b.Ev(args) with //matrices missing
                | RA(d,e), RA(f,g)  -> rel (d*g) (f*e) |> BL
                | BL x, BL y        -> rel x y |> BL
                | AsFloat a, AsFloat b -> rel a b |> BL  // use veryClose?
                | _ -> ERR
    ///fails unless convertible to float
    member t.toFl(args) = t.Ev(args).toFl
    member t.Variables =
        match t with
        | Variable c  -> Set.singleton c
        | Mat m       -> m |> Seq.cast<Formula> |> Seq.fold (fun x y -> Set.union x y.Variables) Set.empty
        | F2(_,a,b)   -> Set.union a.Variables b.Variables
        | F1(_,a)     -> a.Variables
        | _ -> Set.empty
    
    member t.Complexity =
        match t with
        | Mat m     -> m |> Seq.cast<Formula> |> Seq.sumBy (fun x -> x.Complexity)
        | F2(_,a,b) -> 1 + a.Complexity + b.Complexity
        | F1(_,a)   -> 1 + a.Complexity
        | _         -> 1

    member t.FL =
        match t with
        | Float x -> x|> float32
        | _ -> failwith "not a float"

module NumericLiteralZ = 
   let FromZero () = Int(0)
   let FromOne  () = Int(1)
   let FromInt32 a = Int(a)
   let FromInt64 a = Int(int(a))

//type FuzzLog =
//    | False = 0
//    | Maybe = -1
//    | True  = 1

[<AutoOpen>]
module Equality =
    // f1 is the official answer. Range is taken from f1.
    let rec probablyEq (f1:Formula) (f2: Formula) = // add matrices //CATCH ERRORS
        let vars = Set.union f1.Variables f2.Variables
        let args =
            let random = new Random()
            let mapList = Set.map (fun (c:char) -> (c, 10. * random.NextDouble() - 5. |> FL )) vars
            Map.ofSeq mapList
    //    Debug.WriteLine("Formula 1 evaluation = "+string(f1.Ev(args)))
    //    Debug.WriteLine("Formula 1 latex = "+f1.TeX)
    //    Debug.WriteLine("Formula 2 evaluation = "+string(f2.Ev(args)))
    //    Debug.WriteLine("Formula 2 latex = "+f2.TeX)
        match f1.Ev(args), f2.Ev(args) with
            | ERR, _ -> probablyEq f1 f2
            | x,y -> Result.approx x y

[<AutoOpen>]
module Differentiation =
    let D f (x:char) =
        let rec Dx g =
            match g with
            | Int _ | Float _ | Pi | E -> 0Z
            | Variable c -> if c = x then 1Z else 0Z
            | Mat m -> m |> Array2D.map Dx |> Mat
            | F2(f,a,b) ->
                match f with
                    | Plus -> Dx a + Dx b
                    | Minus -> Dx a - Dx b
                    | Times  -> a * Dx(b) + Dx(a) * b
                    | Divide  ->  (Dx(a) * b - a * Dx(b)) / (b ** 2Z)
                    | Power -> (a ** b) * (Dx(b) * log a + b * Dx(a) / a) // a^b = e^(b log a) -> a^b * (b' log a + b a'/a)
                    | LogBase -> Dx(log b) / (log a)
            | F1(f,a) ->
                Dx(a)*
                match f with
                    | Log  -> 1Z/a  | Exp  -> exp a | Sqrt -> 1Z/(2Z*sqrt(a))
                    | Sin  -> cos a | Cos  -> -sin a    | Tan  -> F1(Sec,a) ** 2Z
                    | Asin -> 1Z / sqrt(1Z-a*a)   | Acos -> -1Z / sqrt(1Z-a*a)   | Atan -> 1Z / (1Z+a*a)
                    | Csc  -> -F1(Csc,a)*F1(Cot,a)   | Sec  -> F1(Sec,a)* tan a | Cot  -> -F1(Csc,a) ** 2Z
                    | Sinh -> cosh a   | Cosh -> sinh a   | Tanh -> 1Z - (tanh a) ** 2Z
                    | Neg -> -1Z
                    | Abs | Fact -> failwith "can't"
            | Rel(r,a,b) -> failwith "not a real-valued function"
        Dx f

module RandomFormulas =
    let r = new Random()
    let rec randomVariable() =
        let c = ('a'+(char)(r.Next(0,23)))
        if c = 'e' then randomVariable() else Variable(c)

    let rec randomFormula() = // does not produce matrices or relations
        match r.Next(1,10) with
        | 1     -> Int(r.Next(-1000,1000))
        | 2     -> Float (r.NextDouble())
        | 3 | 4 -> randomVariable()
        | 5     -> Pi
        | 6     -> E
        | 7 | 8 -> // no LogBase
            let bv = match r.Next(1,5) with | 1 -> Plus | 2 -> Minus | 3 -> Times | 4 -> Divide | _ -> Power
            F2(bv,randomFormula(),randomFormula())
        | _     -> // no logs, sqrt, Fact
            let uv =
                match r.Next(1,7) with
                | 1 -> Exp | 2 -> Sin | 3 -> Cos | 4 -> Tan | 5 -> Asin | 6 -> Acos | 7 -> Atan
                | 8 -> Csc | 9 -> Sec | 10 -> Cot | 11 -> Sinh | 12 -> Cosh | 13 -> Tanh | 14 -> Neg | _ -> Abs
            F1(uv,randomFormula())


    let randomSum() =
        let a = r.Next(1,10)
        let b = r.Next(1,10)
        Int(a)+Int(b)

    let randomSquare() =
        let a = r.Next(1,10)
        (Int a)**2Z

    let specificRandomFormula() =
        let a = r.Next(1,10) |> Int
        let b = r.Next(1,10)|> Int
        
        1Z+2Z*((a+b)**(a+b))

    let rec randomFormula2() =
        match r.Next(1,8) with
            | 1 -> randomFormula() ** randomFormula()
            | 2 -> randomFormula() * randomFormula()
            | 3 -> randomFormula() + randomFormula()
            | _ -> Int(r.Next(1,10))
    
    let rec nonTrivialRandomFormula() =
        let rf = randomFormula()
        match rf with
            | Int x -> nonTrivialRandomFormula()
            | _ -> rf