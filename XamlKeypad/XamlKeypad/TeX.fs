namespace TeX
open Formulas
open System
open System.Text.RegularExpressions
open System.Diagnostics

//convert results to TeX too


//type Result<'a> = // this is in F# 4.1 so delete this when we update to F# 4.1
//    | Ok of 'a
//    | Error of string

[<AutoOpen>]
module Display =
    ///Returns a TeX string given a formula, possibly protected by brackets to protect the formula from products (protectlevel = 1) and powers (protectlevel = 2).
    let rec private teX protectLevel g =
        let (inner, protectWhen) =
            match g with
            | Int a       ->  (a.ToString() , 3)
            | Float a     ->  (a.ToString() , 3)
            | Variable x  ->  (x.ToString() , 3) //greek?
            | Mat m       ->
                let s = new Text.StringBuilder("\left(\begin{matrix}")
                for i in 0..m.GetLength(0)-1 do
                    if i > 0 then s.Append("\\") |> ignore
                    for j in 0..m.GetLength(1)-1 do
                        if j > 0 then s.Append("&") |> ignore
                        s.Append( teX 3 m.[i,j] :string) |> ignore
                s.Append("\end{matrix}\right)") |> ignore
                (s.ToString(),3)
            | Pi          ->  ("\pi " , 3)
            | E           ->  ("e" , 3)
            | F2(f,a,b)   ->
                match f with
                | Plus    -> ( teX 0 a + "+" + teX 0 b, 1)
                | Minus   -> ( teX 0 a + "-" + teX 0 b, 1)
                | Times   -> ( teX 1 a + @"\times " + teX 1 b , 2)
                | Divide  -> ( @"\frac{"+teX 0 a+"}{"+teX 0 b+"}" , 2)
                | Power   -> ( "{" + teX 2 a + "}^{" + teX 0 b + "}" , 2)
                | LogBase -> ( @"\log_{" + teX 0 b + "}" + teX 3 a , 2)
            | F1(u,a)     -> 
                match u with
                | Neg     -> ("-" + teX 2 a , 2)
                | Abs     -> ( @"\left|" + teX 0 a + @"\right|" , 3)
                | Fact    -> ( teX 2 a + "!" , 2)  // check -x!
                | Sqrt    -> ( @"\sqrt{" + teX 0 a + "}", 3)
                | _       -> (@"\" + u.ToString() + teX 3 a , 2)
            | Rel(f,a,b)   ->
                match f with
                | Equals  -> ( teX 0 a + "=" + teX 0 b , 1)
                | LEq     -> ( teX 0 a + "\leq " + teX 0 b , 1)
                | Less    -> ( teX 0 a + "<" + teX 0 b , 1)
        if protectLevel >= protectWhen then @"\left(" + inner + @"\right)" else inner

    let TeXfromFormula = teX 0


//    let TeXtoHtml teX =
//        let mathJaxPath = @"file:///" + FileTools.appBaseFolderPath + @"/miniMathJax-master/MathJax.js?config=TeX-AMS_CHTML"
//        """
//    <!DOCTYPE html><html>
//    <head>
//    <script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML"></script>
//    <script>MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]},imageFont: null});</script>
//    </head>
//    <body><p>""" + teX + """</p></body></html>"""

    //function dummy () {window.location.href = "http://height:" + height();}
    //var placeholder = window.setTimeout(dummy,2000);

[<AutoOpen>]
module Parse = //parse \sqrt[n]{}
    
    //let maybe = Model.MaybeBuilder()

    let private levelsFn (s:string) openBs closeBs =
        let mutable l = 0
        [ for i in 0 .. s.Length-1 do
            if openBs  |> List.contains s.[i] then l <- l+1
            yield (s.[i],l)
            if closeBs |> List.contains s.[i] then l <- l-1 ]

    let private matchCurly s pos = //mut have curlybracket at pos
        let levels = levelsFn s ['{'] ['}']
        let (_,h) = levels.[pos]
        let (index, sought) =
            match s.[pos] with
            | '{' -> ([pos+1..s.Length-1], ('}',h))
            | '}' -> ([pos-1..-1..0], ('{',h))
            | _   -> failwith "curly bracket expected"
        index |> List.tryFind (fun i -> levels.[i] = sought)

    let greeks =
       ["alpha","α";   "beta","β";     "gamma","γ";    "Gamma","Γ";    "delta","δ";    "Delta","Δ";
        "epsilon","ε"; "zeta","ζ";     "eta","η";      "theta","θ";    "Theta","Θ";    "iota","ι";
        "kappa","κ";   "lambda","λ";   "Lambda","Λ";   "mu","μ";       "nu","ν";       "xi","ξ";
        "Xi","Ξ";      "pi","π";       "Pi","Π";       "rho","ρ";      "sigma","σ";    "Sigma","Σ";
        "tau","τ";     "upsilon","υ";  "Upsilon","Υ";  "phi","φ";      "Phi","Φ";      "chi","χ";
        "psi","ψ";     "Psi","Ψ";      "omega","ω";    "Omega","Ω" ]

    //When conciser is defined above preprocess, it fails sometimes
    let private conciser =
      [ ( [@"\log";"log";"Log";"LOG"; @"\ln"; "ln"],          "\u1000")
        ( ["\u1000_"],                                        "\u1001")
        ( [@"\sqrt";"sqrt";"Sqrt";"SQRT"],                    "\u1002")
        ( [@"\exp";"exp";"Exp";"EXP"],                        "\u1003")
        ( [@"\frac"],                                         "\u1004")
        ( [@"\csc";"csc";"Csc";"CSC";"cosec";"Cosec";"COSEC"],"\u1016")
        ( [@"\sec";"sec";"Sec";"SEC"],                        "\u1017")
        ( [@"\cot";"cot";"Cot";"COT"],                        "\u1018")
        ( [@"\sinh";"sinh";"Sinh";"SINH"],                    "\u1019")
        ( [@"\cosh";"cosh";"Cosh";"COSH"],                    "\u101A")
        ( [@"\tanh";"tanh";"Tanh";"TANH"],                    "\u101B")
        ( [@"\arcsin";"asin";"Asin";"ASIN"],                  "\u1013")
        ( [@"\arccos";"acos";"Acos";"ACOS"],                  "\u1014")
        ( [@"\arctan";"atan";"Atan";"ATAN"],                  "\u1015")
        ( [@"\sin";"sin";"Sin";"SIN"],                        "\u1010")
        ( [@"\cos";"cos";"Cos";"COS"],                        "\u1011")
        ( [@"\tan";"tan";"Tan";"TAN"],                        "\u1012")
        ( [@"\times"; @"\cdot"; "×"],                         "*"     )
        ( [@"\left|"], "\u0990"); ( [@"\right|"], "\u0991")
        ( [@"\left";@"\right";" "],                           ""      )
        ( [@"\begin{matrix}"],"\u1020");  ( [@"\end{matrix}"],"\u1021")
        ]

    //add * after } followed by ... comes from ^ or _ or } (to cope with \frac) (but not fun^ or fun_ which is dealt with above)
    let rec private curlyTimes (s:string) =
        let index =
            [0..(s.Length-1)] |> List.tryFindIndex (fun i ->
                s.[i] = '}' && // short-circuit eval.
                    match matchCurly s i with
                    | Some(n) -> 
                        n>0 && i<s.Length-1 &&
                            let cd = s.[n-1].ToString() + s.[i+1].ToString()
                            Regex.IsMatch(cd,"[\^_}][a-zA-Zα-ωΓ-Ω\(\u0990\u1000-\u1030]")
                    | None -> false )
        match index with
            | None -> s
            | Some i -> s.[..i]+"*"+s.[i+1..] |> curlyTimes
    
    let private preprocess (s:string) = // possibly add unary +
        let mutable m = s
        Debug.WriteLine("before preprocessing: " + m)

        //add {} after ^ or _ if not there already
        m <- Regex.Replace(m, "([\^_])([^{])", "$1{$2}")

        for (glatex,gletter) in greeks do
            m <- m.Replace(@"\"+glatex,gletter)
            m <- m.Replace(@"\var"+glatex,gletter)
    
        for (fstrs,repl) in conciser do
            for fstr in fstrs do m <- m.Replace(fstr,repl)
    
        for c in '\u1010'..'\u101A' do
            let d = ((c + (char)0x10)).ToString()
            m <- m.Replace(c.ToString() + "^", d)

        //add multiplication signs. { letter or number or ) } * { letter or ( or function }
        for i = 1 to 2 do
            m <- Regex.Replace(m,
                    "([a-zA-Zα-ωΓ-Ω0-9\)\u0991])([a-zA-Zα-ωΓ-Ω\(\u0990\u1000-\u1030])",
                    "$1*$2")
    
        m <- curlyTimes m

        //find some negative signs 
        m <- Regex.Replace(m,"([*/+\-^])-","$1\u100F")

        m

    //let private require b = if not b then failwith "condition failed" //like assert but without a messagebox
    
//    let optmap2 (f:'a -> 'b -> 'c) (x: 'a option) (y: 'b option) =
//        match x,y with Some u, Some v -> Some(f u v) | _ -> None
    
    let rec private parse2 (f:Formula -> Formula -> Formula) (l: string) (r: string) =
        match pp l with
        | Some u -> match pp r with Some v -> Some(f u v) | None -> None
        | None -> None
    
    /// parse preprocessed string
    and pp (s2:string) : Formula option = //"-a^2" gets interpreted as (-a)^2
        //Debug.WriteLine("after preprocessing: " + s2)
        let sLen = s2.Length
        let nEmpty = (sLen > 0)

        // Various functions defined which return Ok(Some(f)) if they parse a formula, Ok(None) if they don't but don't find an error, and Error(...) if they find a problem.
        // Alternative: ignore the Error part, ignore any problems found on the way and just report Some(f) or None. Then if there are problems it will mean that ultimately there will be a failure to parse that formula.

//        let initialTests () = // 
//            //since I think any backslash symbols from this point are an error
//            if s2.Contains(@"\") then Error("TeX operator not supported")
//            elif sLen=0 then Error("empty formula")
//            else Ok(None:Formula option)

        let s = if nEmpty && s2.[0] = '-' then "\u100F"+s2.[1..] else s2
    
        Debug.WriteLine("Parsing "+s2)

        let levels = levelsFn s ['('; '{'; '\u0990'] [')';'}';'\u0991'] 
    
        let findInfix (opchar:char, op) () =
            match levels |> Seq.tryFindIndex ((=)(opchar,0)) with
            | Some(n) ->
                (s.[0..n-1], s.[n+1..]) ||> parse2 (fun l r -> F2(op,l,r))
            | None -> None
    
        let findDivide () =
            if sLen>2 && s.[0] = '\u1004' && s.[1] = '{' then
                matchCurly s 1
                    |> Option.bind (fun n ->
                        if n+1<sLen-1 && s.[n+1]='{' && s.[sLen-1]='}'
                        then (s.[2..n-1], s.[n+2..sLen-2]) ||> parse2 (fun l r -> l/r)
                        else None)
            else None
        
        let findLogBase () =
            if sLen>1 && s.[0] = '\u1001' && s.[1] = '{'
            then matchCurly s 1
                |> Option.bind (fun n -> (s.[n+1..], s.[2..n-1]) ||> parse2 (fun x b -> F2(LogBase,x,b)))
            else None
        
        let findBrackets () =
            if sLen<=2 then None
            else
                let inner = s.[1..sLen-2]
                match s.[0], s.[sLen-1] with
                    | '(',')' | '{','}' -> pp inner
                    | '\u0990','\u0991' -> pp inner |> Option.map (fun p -> F1(Abs, p))
                    | _ -> None
                
        let findNegative () =
            if nEmpty && s.[0] = '\u100F'
            then pp s.[1..] |> Option.map (function Int(x) -> Int(-x) | Float(x) -> Float(-x) | p -> -p)
            else None

            
        let acceptSimple f = match f with Int(_) | Float(_) | Variable(_) -> Some(f) | _ -> None
        
        let findUnaryOp (opchar, op) () =
            if nEmpty && s.[0] = opchar then
                if sLen>2 && s.[1] = '(' && s.[sLen-1]= ')' then pp s.[2..sLen-2]
                else pp s.[1..sLen-1] |> Option.bind acceptSimple
                |> Option.map (fun p -> F1(op, p) )
            else None

        let findTrigOp (opchar, op, invOpt: Univariate option) () =
            if   nEmpty && s.[0] = opchar then findUnaryOp(opchar,op)()
            elif sLen>1 && s.[0] = opchar + (char)0x10 && s.[1] = '{' then
                match matchCurly s 1 with
                | Some (n) ->
                    let arg =
                        if n+1<sLen-1 && s.[n+1] = '(' && s.[sLen-1]= ')' then pp s.[n+2..sLen-2]
                        else pp s.[n+1..sLen-1] |> Option.bind acceptSimple
                    match pp s.[2..n-1] with
                        | Some(Int -1)  ->
                            match invOpt with
                                | Some inv  -> arg |> Option.map (fun p -> F1(inv, p))
                                | None -> None
                        | Some(Int n) when n>0 ->
                           arg |> Option.map (fun p -> F1(op, p) ** Int n)
                        | _ -> None
                | None -> None
            else None
    
        let findFactorial () =
            if nEmpty && s.[sLen-1] = '!' then pp s.[0..sLen-2] |> Option.map (fun p -> F1(Fact, p)) else None
    
        let findVariable () =
            if sLen = 1 then
                let c = s.[0]
                if (c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='α' && c<='ω') || (c>='Γ' && c<='Ω') then
                    (match c with 'e' -> E | 'π' -> Pi | _   -> Variable c) |> Some
                    else None
            else None
    
        let findInt () = match Int32.TryParse s with (true, r) -> Some(Int r) | _ -> None
                
        let findFloat () = match Double.TryParse s with (true, r) -> Some(Float r) | _ -> None

        //untested; place somewhere above. No idea if this will be the right matrix or the transpose
//        let findMatrix () =
//            if s.[0] = '\u1020' && s.[sLen-1] = '\u1021' then
//                s.[1 .. sLen-2]
//                    .Split([|@"\\"|],StringSplitOptions.None)
//                    |> Array.map (fun x -> x.Split([|'&'|]) )
//                    |> array2D |> Array2D.map pp |> Mat |> Some
//            else None

        let l = [ findInfix('+', Plus);                 findInfix('-', Minus)
                  findNegative
                  findInfix('*', Times);                findInfix('/', Divide)
                  findInfix('^', Power)
                  findUnaryOp('\u1000', Log);           findUnaryOp('\u1002', Sqrt);          findUnaryOp('\u1003', Exp)
                  findTrigOp('\u1010', Sin, Some Asin); findTrigOp('\u1011', Cos, Some Acos); findTrigOp('\u1012', Tan, Some Atan)
                  findTrigOp('\u1013', Asin, Some Sin); findTrigOp('\u1014', Acos, Some Cos); findTrigOp('\u1015', Atan, Some Tan)
                  findTrigOp('\u1016', Csc, None);      findTrigOp('\u1017', Sec, None);      findTrigOp('\u1018', Cot, None)
                  findTrigOp('\u1019', Sinh, None);     findTrigOp('\u101A', Cosh, None);     findTrigOp('\u101B', Tanh, None)
                  findDivide
                  findLogBase
                  findFactorial
                  findBrackets
                  findVariable
                  findInt
                  findFloat   ]

        (None,l)
            ||> List.fold (fun found f -> if found.IsSome then found else f())

    let parse = pp << preprocess // should have parse and tryparse

    //let test = parse "tan(cos(log(sin(x))))+\frac{1}{y+1.5}"

    let probablyEqStr l1 l2 =
        match (parse l1, parse l2) with
            | Some f1, Some f2 -> probablyEq f1 f2
            | _ -> failwith "could not parse a formula"

    ///l1 is probably simpler than l2
    let probablySimplerStr l1 l2 =
        match (parse l1, parse l2) with
            | Some f1, Some f2 -> f1.Complexity < f2.Complexity
            | _ -> failwith "could not parse a formula"