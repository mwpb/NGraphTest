namespace XamlKeypad

open System
open System.Diagnostics
open Xamarin.Forms
open Xamarin.Forms.Xaml
open NControl.Abstractions
open NGraphics
open Formulas
open Xamarin.RangeSlider.Forms

type Point(x:Formula,y:Formula) =
    member val X = x with get, set
    member val Y = y with get, set
    member t.Ev (evalMap:Map<char,Result>) =
        let xnew = t.X.toFl(evalMap)
        let ynew = t.Y.toFl(evalMap)
        Point(xnew,ynew)
    new(x,y) = Point(Float x, Float y)

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

type Line(startPoint:Point,endPoint:Point) =
    member val StartPoint = startPoint with get, set
    member val EndPoint = endPoint with get, set
    member t.Ev (evalMap:Map<char,Result>) =
        let s,e = t.StartPoint, t.EndPoint
        Line(s.Ev(evalMap),e.Ev(evalMap))

type NGraph(slider:Slider,lines:Line list,plots:Formula list,bb:BoundingBox,grid:GraphGrid) as self =
    inherit NControlView(BackgroundColor = Color.White)
    let mutable sliderValue = slider.Value |> float
    let getLines (evalMap) = lines |> List.map (fun x -> x.Ev(evalMap))
    do  slider.ValueChanged.Add(
            fun x ->
                Debug.WriteLine (sprintf "slider changed to: %f" x.NewValue)
                sliderValue <- (x.NewValue |> float )/1000.0
                self.Invalidate()
                )
    override this.Draw (canvas:NGraphics.ICanvas,rect:NGraphics.Rect) =
        let evalMap = ['k',sliderValue |> FL] |> Map.ofList
        let encodePoint (p:Point) =
            let x,y = p.X,p.Y
            let newX = (Float rect.Left) + ((x-bb.XMin)/(bb.Width))*(Float (rect.Right-rect.Left))
            let newY = (Float rect.Top) + ((bb.YMax-y)/(bb.Height))*(Float (rect.Bottom-rect.Top))
            NGraphics.Point(newX.toFl(evalMap),newY.toFl(evalMap))        
        let tests() =
            Debug.WriteLine (sprintf "rect.top: %f" rect.Top)
            Debug.WriteLine (sprintf "rect.bottom: %f" rect.Bottom)
            Debug.WriteLine (sprintf "rect.left: %f" rect.Left)
            Debug.WriteLine (sprintf "rect.right: %f" rect.Right)
        let plotFunctions() =
            let xEval (input:float) = ['x',input |> FL;'k',sliderValue |> FL] |> Map.ofList
            plots
            |> List.iter (
                fun plot ->
                    let pathOpList = System.Collections.Generic.List<PathOp>()
                    let stepSize = bb.Width.toFl(evalMap)/100.0
                    let xmin = bb.XMin.toFl(evalMap)
                    pathOpList.Add(MoveTo(encodePoint(Point(xmin,plot.toFl(xEval xmin)))))
                    let xs = [0..100] |> List.map (fun i -> xmin + (float i)*stepSize)
                    for x in xs do
                        pathOpList.Add(LineTo(encodePoint(Point(x,plot.toFl(xEval x)))))
                    canvas.DrawPath(pathOpList,Pens.DarkGray.WithWidth(3.0))
                    )
        let drawLines() = 
            lines
            |> List.map (fun x -> x.Ev(evalMap))
            |> List.iter (
                fun x -> 
                    let newStart, newEnd = encodePoint(x.StartPoint),encodePoint(x.EndPoint)
                    canvas.DrawLine(newStart,newEnd,NGraphics.Colors.Blue))
        let createAxes() =
            let midLeft = Point(bb.XMin,Float 0.0) |> encodePoint
            let midRight = Point(bb.XMax,Float 0.0) |> encodePoint
            let midBottom = Point(Float 0.0,bb.YMin) |> encodePoint
            let midTop = Point(Float 0.0,bb.YMax) |> encodePoint
            canvas.DrawLine(midLeft,midRight, NGraphics.Colors.Black);
            canvas.DrawLine(midBottom,midTop, NGraphics.Colors.Black);
        //tests()
        createAxes()
        drawLines()
        plotFunctions()

type NGraphView(lines:Line list,plots:Formula list,bb:BoundingBox,grid:GraphGrid) =
    let slider = Slider(Maximum = 1000.0,Minimum = 0.0)
    member t.NGraph = 
        let graph = NGraph(slider,lines,plots,bb,grid)
        graph.HeightRequest <- 500.0
        graph.WidthRequest <- 500.0
        graph
    member t.SliderControl = slider
    member t.View = 
        let s = StackLayout()
        s.Children.Add t.NGraph
        s.Children.Add slider
        s

type App() = 
    inherit Application()

    let mainPage = ContentPage()
    let grid = Grid()
    
    let ncontrolView = 
        new NGraphView(
            [Line(Point(1.0,1.0),Point(Float 2.0,Variable 'k'))],
            [Variable 'x' * Variable 'x' * Variable 'k';sin(Variable 'x'*Variable 'k')],
            BoundingBox(),
            GraphGrid())
    
    do  grid.ColumnDefinitions.Add (ColumnDefinition(Width = GridLength 500.0))
        grid.RowDefinitions.Add (RowDefinition(Height = GridLength 500.0))
        grid.Children.Add (ncontrolView.View,0,0)
        
    do mainPage.Content <- grid

    do base.MainPage <- mainPage