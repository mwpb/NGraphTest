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
open GraphTypes

[<AutoOpen>]
module Timer =
    let time s f =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        f()
        stopWatch.Stop()
        Debug.WriteLine (s + " took: "+stopWatch.Elapsed.TotalMilliseconds.ToString())

type MathSlider(g:GSlider) =
    inherit ContentView()
    let mutable currentValue = g.Initial
    let s = Slider(Minimum = 0.0,Maximum = 1000.0)

    do base.Content <- s
    // actually expose an event with eventargs = Variable * value
    member t.Changed = s.ValueChanged
    member t.Variable = g.Variable
    member t.CurrentValue = currentValue
    member t.Min = g.Min
    member t.Max = g.Max

type NGraphView(initParams: (char*float) list, graphDef: Graph) as self =
    inherit NControlView(BackgroundColor = Color.White)
    let graphics, bb, grid = graphDef.Graphics, graphDef.BB, graphDef.Grid
    let mutable paramMap = initParams

    let mutable axesPath, gridPath = Path(), Path()
    let (graphPaths:Path[]) = Array.zeroCreate graphics.Length

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
        
        let getPath i g =
            graphPaths.[i] <-
            match g with
            | GPlot (f,c) ->
                let path = Path(Pen(Color = c, Width= 3.))
                let wholeMap (input:float) = (['x',input] @ paramMap)
                let stepSize = (evFl bb.Width paramMap)/100.0
                let xmin = evFl bb.XMin paramMap
                path.MoveTo(toPoint(P(xmin, evFl f (wholeMap xmin))))
                for i = 0 to 100 do
                    let x = xmin + (float i)*stepSize
                    path.LineTo(toPoint(P(x, evFl f (wholeMap x))))
                path
            | GSegment (p1,p2,c) ->
                let path = Path(Pen(Color = c, Width= 3.))
                path.MoveTo (toPoint(p1))
                path.LineTo (toPoint(p2))
                path

        let getAxes() =
            let path = Path(Pen(Color = Colors.DarkGray ))
            if grid.ShowAxes then
                let midLeft = P(bb.XMin,Float 0.0) |> toPoint
                let midRight = P(bb.XMax,Float 0.0) |> toPoint
                let midBottom = P(Float 0.0,bb.YMin) |> toPoint
                let midTop = P(Float 0.0,bb.YMax) |> toPoint
                path.MoveTo(midLeft);   path.LineTo(midRight)
                path.MoveTo(midBottom); path.LineTo(midTop)
            if grid.ShowGrid then
                let path = Path(Pen(Color = NGraphics.Color.FromRGB(0.9,0.9,0.9) ))
                let xmin = evFl bb.XMin initParams
                let xmax = evFl bb.XMax initParams
                for s in [Math.Floor xmin..grid.AxisXSep..Math.Floor xmax] do
                    P(s,bb.YMin) |> toPoint |> path.MoveTo
                    P(s,bb.YMax) |> toPoint |> path.LineTo
                let ymin = evFl bb.YMin initParams
                let ymax = evFl bb.YMax initParams
                for y in [Math.Floor ymin..grid.AxisYSep..Math.Floor ymax] do
                    P(bb.XMin,y) |> toPoint |> path.MoveTo
                    P(bb.XMax,y) |> toPoint |> path.LineTo
                gridPath <- path
            if grid.ShowAxes then
                let xmin = evFl bb.XMin initParams
                let xmax = evFl bb.XMax initParams
                for x in [Math.Floor xmin..grid.AxisXSep..Math.Floor xmax] do
                    path.MoveTo(P(x,-0.1) |> toPoint)
                    path.LineTo(P(x,0.1) |> toPoint)
                let ymin = evFl bb.YMin initParams
                let ymax = evFl bb.YMax initParams
                for y in [Math.Floor ymin..grid.AxisYSep..Math.Floor ymax] do
                    path.MoveTo(P(-0.1,y) |> toPoint)
                    path.LineTo(P(0.1,y) |> toPoint)
//                Debug.WriteLine grid.GridXSep
//                Debug.WriteLine (sprintf "%A" xs)
//                canvas.DrawText("HI",NGraphics.Rect(100.0,100.0,50.0,150.0),NGraphics.Font(),NGraphics.TextAlignment.Left,NGraphics.Colors.Black)
            axesPath <- path
            //canvas.DrawText("1",toPoint(P(1.,-0.5)),Font("Georgia",10.0),Colors.Black)        
        (fun () ->
            getAxes(); axesPath.Draw(canvas); gridPath.Draw(canvas)
            ) |> time "creating axes"
        (fun () ->
            graphics |> List.iteri (fun i g -> getPath i g)
            graphPaths |> Array.iter (fun p -> p.Draw(canvas))
            ) |> time "plotting functions"

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
            let a = e.NewValue/1000.
            graphs |> List.iter (fun g -> 
                let currentValue = (1.-a)* s.Min + a * s.Max
                g.SetParams(sliders |> List.map (fun s -> s.Variable, currentValue)))
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
                G.Plot(Variable 'x' * Variable 'x' * Variable 'k');
                G.Plot(sin(Variable 'x'*Variable 'k'),Colors.Red)
                G.Segment(P(1.0,1.0),P(2.0,2.0))
                G.Segment(P(1.0,1.0),P(Float 3.0,Variable 'k'),Colors.Green)
            ],
            BoundingBox(),
            GraphGrid())
    let g2 = Graph([G.Plot(cos(Variable 'x'*Variable 'k'))],BoundingBox(), GraphGrid())
    let ncontrolView =
        NDisplay([g1;g2],[GSlider.Default])
        
    do mainPage.Content <- ncontrolView

    do base.MainPage <- mainPage