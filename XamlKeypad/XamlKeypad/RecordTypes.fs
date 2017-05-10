namespace Model
open Formulas
open Xamarin.Forms
open System
open TeX
open System.Diagnostics

[<RequireQualifiedAccess>]
type Ans =
    | Str of string
    | Choice of int option
    | Mat of string[,]
    ///LaTeX formula
    | Fa of string

[<RequireQualifiedAccess>]
type Parsed =
    ///float
    | Fl of float
    | Int of int
    | Str of string
    | Mat of float[,]
    ///LaTeX formula
    | Fa of Formula
    | Failed
    | Empty

[<Struct>]
type Score(marks:int,outOf:int) =
    member x.Marks = marks
    member x.OutOf = outOf
    static member (+) (a:Score, b:Score) = Score (a.Marks+b.Marks,a.OutOf+b.OutOf)
    static member Zero = Score(0,0)

type Outcome(score: Score, ?comm: string) = 
    member val Score = score with get,set
    member t.comment = defaultArg comm (if (score.Marks >= score.OutOf) then "right" else "wrong")
    new(corr, ?out) =
        let outOf = defaultArg out 1
        Outcome(Score((if corr then outOf else 0), outOf))

[<RequireQualifiedAccess>]
type Evaluation =
    | Right
    | Wrong
    | Invalid
    member t.Score =
        match t with
        | Right -> Score(1,1)
        | Wrong -> Score(0,1)
        | Invalid -> Score(0,0)

///Description of question part.
[<RequireQualifiedAccess>]
type QP =
    | Int of int
    | Fl of float
    | Str of string
    | Mat of float[,]
    //multiple choice
    | MC of string list * int
    ///formula
    | Fa of Formula
    member t.SolString =
        match t with
        | Int i -> "$$"+i.ToString()+"$$"
        | Fl f -> "$$"+f.ToString()+"$$"
        | Str s -> s
        | Mat fm ->
            let x = fm |> Array2D.map Float |> Formula.Mat |> TeX.Display.TeXfromFormula
            "$$"+x+"$$"
        | Fa f -> "$$"+TeX.Display.TeXfromFormula(f)+"$$"
        | MC (l,i) -> l.[i]
    static member fa s =  
        TeX.Parse.parse s |> Option.get |> QP.Fa // fails if s is invalid
    member t.Parse(inputAns:Ans) = 
        match t, inputAns with
        | QP.Int(_), Ans.Str(s)    ->
            match Int32.TryParse s with (true, x) -> Parsed.Int(int x) | _ -> Parsed.Failed
        | QP.Str(_), Ans.Str(s)    -> Parsed.Str(string s)
        | QP.Fl(_),  Ans.Str(s)    ->
            match Double.TryParse s with (true, x) -> Parsed.Fl x | _ -> Parsed.Failed
        | QP.Mat(_), Ans.Mat(m)    ->
            let fom = m |> Array2D.map (fun s -> match Double.TryParse s with (true, x) -> Some(x) | _ -> None)
            if fom |> Seq.cast<float option> |> Seq.forall Option.isSome
            then fom |> Array2D.map Option.get |> Parsed.Mat
            else Parsed.Failed
        //Parsed.Mat(Array2D.map (fun s -> (float s)) m)
        | QP.MC(_),  Ans.Choice(Some n) -> Parsed.Int(n)
        | QP.Fa(_),  Ans.Fa(s) ->
            match parse(s) with Some(f) -> Parsed.Fa(f) | None -> Parsed.Failed
        | _ -> Parsed.Failed

    member t.Evaluate(inputAns:Ans) =
        let p = t.Parse(inputAns)
        if p = Parsed.Failed then Evaluation.Invalid else
        match t, p with
            | QP.Int a,   Parsed.Int s -> s = a
            | QP.Str a,   Parsed.Str s -> s = a
            | QP.Fl a,    Parsed.Fl s  -> approxEq s a
            | QP.Mat a,   Parsed.Mat s -> Seq.forall2 approxEq (Seq.cast<float>(a)) (Seq.cast<float>(s))
            | QP.MC(_,a), Parsed.Int s -> s=a
            | QP.Fa a,    Parsed.Fa f  -> probablyEq a f
            | _ -> failwith "Q&A type mismatch"
        |> (function | true -> Evaluation.Right | false -> Evaluation.Wrong)


type Difficulty = Easy | Medium | Hard | Extra

type ContentType =
    ///Test
    | T
    ///Video
    | V
    ///Video and Test
    | VT

[<Struct>]
type Content(id:string, title:string, ct:ContentType, ?diff:Difficulty) =
    member t.ID = id
    member t.Title = title
    member t.Difficulty = defaultArg diff Difficulty.Medium
    member t.ContentType = ct
    override t.ToString() = t.Title


type MetaData = string * Difficulty

type ItemType = Q | G | QG
type Item = ItemType * string

/// video break
type VBreak =
    /// Chapter
    | C
    /// Pause
    | P
    /// Item
    | I

type Lesson =
    { Title:string
      Description: string
      Parts: System.Collections.Generic.List<Content> }
    override t.ToString() = t.Title
    member t.Add(a,b,c,d) = t.Parts.Add(Content(a,b,c,d))

type Course = 
    { Title:string
      Description: string
      Lessons: Lesson list }
    override t.ToString() = t.Title

[<Struct>]
type Question(tex: string, defs: QP list, ?hints: string list, ?sol: string, ?svg:string) =
    member t.TeX = tex
    member t.Defs = defs
    member t.Sol =
        match sol with
        | Some(s) -> s
        | None ->
            defs |> List.map (fun qp -> qp.SolString) |> List.fold (fun s t -> s + @"<br>" + t) "" // |> List.fold (fun s t -> s + @"\par" + t) ""
    member t.Hints = defaultArg hints []
    member t.SvgName = svg
    member t.QParts = defs
    member t.Evaluate(answers: Ans list) = 
        let evaluations = List.map2 (fun (p:QP) a -> p.Evaluate(a)) (t.QParts) answers
        //Debug.WriteLine(sprintf "given answers: %A; official answers: %A; evaluations: %A" answers t.QPDefs evaluations)
        answers, evaluations

type AttemptRecord = {answers: Ans list; evaluations: Evaluation list} with
    member t.Score = t.evaluations |> List.map (fun x -> x.Score) |> List.sum

[<RequireQualifiedAccess>]
type EduNode =
    | Programme
    | Course of Course
    | Lesson of Lesson
    | Content of Content

[<RequireQualifiedAccess>]
type Page =
    | Content of Content
    | Info of EduNode
    //| Login