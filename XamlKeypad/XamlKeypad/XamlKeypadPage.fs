namespace XamlKeypad

open System
open System.Diagnostics
open Xamarin.Forms
open Xamarin.Forms.Xaml
open NControl.Abstractions
open NGraphics
open Formulas
open Xamarin.RangeSlider.Forms
open XamarinHelpers

type P(x:Formula,y:Formula) =
    member t.X = x
    member t.Y = y
    member t.Ev (evalMap:Map<char,Result>) =
        let xnew = t.X.toFl(evalMap)
        let ynew = t.Y.toFl(evalMap)
        P(xnew,ynew)
    new(x,y) = P(Float x, Float y)

type Graphic =
    | Plot of Formula
    //| Implicit of Formula //tbd
    //| Point of Formula*Formula
    //| Line of Point * Point
    | Segment of P*P
    //| Polygon of Point list
    //| Circle of (Formula*Formula)* Formula
    static member plot s = Plot (TeX.Parse.parse s).Value



type BoundingBox() =
    member val XMin = Float -10.0 with get, set
    member val XMax = Float +10.0 with get, set
    member val YMin = Float -(10.0) with get, set
    member val YMax = Float +10.0 with get, set
    member t.Width = t.XMax - t.XMin
    member t.Height = t.YMax - t.YMin

type GraphGrid() =
    member val ShowAxes = true with get, set
    member val ShowGrid = true with get, set
    member val GridXSep = 0.1 with get, set
    member val GridYSep = 0.1 with get, set
    member val AxisXSep = 1.0 with get, set
    member val AxisYSep = 1.0 with get, set

type GSlider(variable: char,min:float,initial:float,max:float) =
    member t.Variable = variable
    member t.Min = min
    member t.Max = max
    member t.Initial = initial
    static member Default = GSlider('k',0.,0.,1.)

type MathSlider(g:GSlider) =
    inherit ContentView()
    let mutable currentValue = g.Initial
    let s = Slider(Minimum = 0.0,Maximum = 1000.0)
    //let changed = new Event<char*float>()
    //Event.add (fun )
    //changed.add
    //[<CLIEvent>]
    do s.ValueChanged.Add(fun e ->
        let s = e.NewValue/1000.
        currentValue <- (1.-s)* g.Min + s * g.Max)

    do base.Content <- s
    // actually expose an event with eventargs = Variable * value
    member t.Changed = s.ValueChanged
    member t.Variable = g.Variable
    member t.CurrentValue = currentValue
    // disable label
    //let stack = StackLayout(Orientation = Horizontal)

//type Line(startPoint:P,endPoint:P) =
//    member val StartPoint = startPoint with get, set
//    member val EndPoint = endPoint with get, set
//    member t.Ev (evalMap:Map<char,Result>) =
//        let s,e = t.StartPoint, t.EndPoint
//        Line(s.Ev(evalMap),e.Ev(evalMap))

type Graph(graphics: Graphic list, bb: BoundingBox, grid: GraphGrid) =
    member t.Graphics = graphics
    member t.BB = bb
    member t.Grid = grid

type NGraphView(initParams: (char*float) list, graphDef: Graph) as self =
    inherit NControlView(BackgroundColor = Color.White)
    let graphics, bb, grid = graphDef.Graphics, graphDef.BB, graphDef.Grid
//    let mutable evalMal = ['k',slider.Value |> FL]
//    let mutable evalMap = evalList |> Map.ofList
    let mutable paramMap = initParams
//    do  slider.ValueChanged.Add(
//            fun x ->
//                let input = (x.NewValue |> float )/1000.0
//                evalList <- ['k',input |> FL]
//                evalMap <- evalList |> Map.ofList
//                self.Invalidate()
//                )
    let evFl (f:Formula) (m:(char * float) list) =
        m
        |> List.map (function (c, f) -> (c, FL f))
        |> Map.ofList
        |> f.toFl // put in Formulas type, perhaps replacing toFl
    
    member t.SetParams (m:(char*float) list) =
        paramMap <- m
        self.Invalidate()
    override this.Draw (canvas:NGraphics.ICanvas,rect:NGraphics.Rect) =
        let toPoint (p:P) =
            let newX = (Float rect.Left) + ((p.X-bb.XMin)/(bb.Width))*(Float (rect.Right-rect.Left))
            let newY = (Float rect.Top) + ((bb.YMax-p.Y)/(bb.Height))*(Float (rect.Bottom-rect.Top))
            Point(evFl newX paramMap, evFl newY paramMap)
        
        let draw = function
            | Plot f ->
                let wholeMap (input:float) = (['x',input] @ paramMap)
                let pathOpList = System.Collections.Generic.List<PathOp>()
                let stepSize = (evFl bb.Width paramMap)/100.0
                let xmin = evFl bb.XMin paramMap
                pathOpList.Add(MoveTo(toPoint(P(xmin, evFl f (wholeMap xmin)))))
                let xs = [0..100] |> List.map (fun i -> xmin + (float i)*stepSize)
                for x in xs do
                    pathOpList.Add(LineTo(toPoint(P(x, evFl f (wholeMap x)))))
                canvas.DrawPath(pathOpList,Pens.DarkGray.WithWidth(3.0))
            | Segment (p1,p2) ->
                canvas.DrawLine(toPoint(p1), toPoint(p2),NGraphics.Colors.Blue)

        let createAxes() =
            let midLeft = P(bb.XMin,Float 0.0) |> toPoint
            let midRight = P(bb.XMax,Float 0.0) |> toPoint
            let midBottom = P(Float 0.0,bb.YMin) |> toPoint
            let midTop = P(Float 0.0,bb.YMax) |> toPoint
            canvas.DrawLine(midLeft,midRight, NGraphics.Colors.Black);
            canvas.DrawLine(midBottom,midTop, NGraphics.Colors.Black);
        createAxes()
        graphics |> List.iter draw

type NDisplay(graphDefs: Graph list, sliderDefs: GSlider list) =
    inherit ContentView()
    let sliders = sliderDefs |> List.map MathSlider
    let initParams =
        sliderDefs |> List.map (fun sd -> (sd.Variable, sd.Initial))
    let graphs = graphDefs |> List.map (fun g ->
        NGraphView(initParams,g, HeightRequest = 500., WidthRequest = 500.)
        )
    do  sliders |> List.iter (fun s ->
        s.Changed.Add(fun e ->
            graphs |> List.iter (fun g -> g.SetParams(sliders |> List.map (fun s -> s.Variable, s.CurrentValue)))
            )
        )
    do  base.Content <-
        StackLayout(
            Views =
                [Grid(FirstRow = (graphs |> List.map (fun g -> g :> View)))]
                @ (sliders |> List.map (fun s -> s :> View))
        )

type App() = 
    inherit Application()

    let mainPage = ContentPage()
    let grid = Grid()
    
    
    let g1 =
        Graph(
            [
                Plot(Variable 'x' * Variable 'x' * Variable 'k');
                Plot(sin(Variable 'x'*Variable 'k'))
            ],
            BoundingBox(),
            GraphGrid())
    let g2 = Graph([Plot(cos(Variable 'x'*Variable 'k'))],BoundingBox(), GraphGrid())
    let ncontrolView =
        NDisplay([g1;g2],[GSlider.Default])
        
    do mainPage.Content <- ncontrolView

    do base.MainPage <- mainPage