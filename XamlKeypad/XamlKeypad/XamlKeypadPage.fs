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
            | GPlot (f,c) ->
                let wholeMap (input:float) = (['x',input] @ paramMap)
                let pathOpList = System.Collections.Generic.List<PathOp>()
                let stepSize = (evFl bb.Width paramMap)/100.0
                let xmin = evFl bb.XMin paramMap
                pathOpList.Add(MoveTo(toPoint(P(xmin, evFl f (wholeMap xmin)))))
                let xs = [0..100] |> List.map (fun i -> xmin + (float i)*stepSize)
                for x in xs do
                    pathOpList.Add(LineTo(toPoint(P(x, evFl f (wholeMap x)))))
                let pen = Pen(c).WithWidth(3.0)
                canvas.DrawPath(pathOpList,pen)
            | GSegment (p1,p2,c) ->
                let pen = Pen().WithWidth(3.0).WithColor(c)
                canvas.DrawLine(toPoint(p1), toPoint(p2),pen)

        let createAxes() =
            if grid.ShowAxes then
                let midLeft = P(bb.XMin,Float 0.0) |> toPoint
                let midRight = P(bb.XMax,Float 0.0) |> toPoint
                let midBottom = P(Float 0.0,bb.YMin) |> toPoint
                let midTop = P(Float 0.0,bb.YMax) |> toPoint
                canvas.DrawLine(midLeft,midRight, NGraphics.Colors.Black);
                canvas.DrawLine(midBottom,midTop, NGraphics.Colors.Black);
            if grid.ShowGrid then
                let xmin = evFl bb.XMin initParams
                let xmax = evFl bb.XMax initParams
                let xs = [Math.Floor xmin..grid.AxisXSep..Math.Floor xmax]
//                Debug.WriteLine grid.GridXSep
//                Debug.WriteLine (sprintf "%A" xs)
                let pen = Pen().WithColor(Colors.LightGray)
                for i in xs do
                    canvas.DrawLine(toPoint(P(i,bb.YMin)),toPoint(P(i,bb.YMax)),pen)
                let ymin = evFl bb.YMin initParams
                let ymax = evFl bb.YMax initParams
                let ys = [Math.Floor ymin..grid.AxisYSep..Math.Floor ymax]
//                Debug.WriteLine grid.GridXSep
//                Debug.WriteLine (sprintf "%A" xs)
                let pen = Pen().WithColor(Colors.LightGray)
                for j in ys do
                    canvas.DrawLine(toPoint(P(bb.XMin,j)),toPoint(P(bb.XMax,j)),pen)
            if grid.ShowAxes then
                let xmin = evFl bb.XMin initParams
                let xmax = evFl bb.XMax initParams
                let xs = [Math.Floor xmin..grid.AxisXSep..Math.Floor xmax]
//                Debug.WriteLine grid.GridXSep
//                Debug.WriteLine (sprintf "%A" xs)
//                Debug.WriteLine (xs.Length.ToString())
                let pen = Pen().WithColor(Colors.Black)
                for i in xs do
                    canvas.DrawLine(toPoint(P(i,-0.1)),toPoint(P(i,0.1)),pen)
                let ymin = evFl bb.YMin initParams
                let ymax = evFl bb.YMax initParams
                let ys = [Math.Floor ymin..grid.AxisYSep..Math.Floor ymax]
//                Debug.WriteLine grid.GridXSep
//                Debug.WriteLine (sprintf "%A" xs)
                let pen = Pen().WithColor(Colors.Black)
//                canvas.DrawText("HI",NGraphics.Rect(100.0,100.0,50.0,150.0),NGraphics.Font(),NGraphics.TextAlignment.Left,NGraphics.Colors.Black)
                for j in ys do
                    canvas.DrawLine(toPoint(P(-0.1,j)),toPoint(P(0.1,j)),pen)
//                    canvas.DrawText("1",toPoint(P(-0.2,j)),Font("Georgia",10.0),Colors.Black)
                
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