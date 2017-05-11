module GraphTypes

open Formulas
open NGraphics

type P(x:Formula,y:Formula) =
    member t.X = x
    member t.Y = y
    new(x:float,y:float) = P(Float x, Float y)
    new(x:Formula,y:float) = P(x, Float y)
    new(x:float,y:Formula) = P(Float x,y)

[<AutoOpen>]
type G =
    | GPlot of Formula*Color
    //| Implicit of Formula //tbd
    //| Point of Formula*Formula
    //| Line of Point * Point
    | GSegment of P*P*Color
    //| Polygon of Point list
    //| Circle of (Formula*Formula)* Formula
//    static member plot s = Plot (TeX.Parse.parse s).Value
//    member t.Color = Colors.Blue
    static member Plot(f:Formula,?c:Color) =
        match c with
        | Some color -> GPlot(f,color)
        | None -> GPlot(f,Colors.Blue)
    static member Segment(p1,p2,?c:Color) =
        match c with
        | Some color -> GSegment(p1,p2,color)
        | None -> GSegment(p1,p2,Colors.Blue)

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

type Graph(graphics: G list, bb: BoundingBox, grid: GraphGrid) =
    member t.Graphics = graphics
    member t.BB = bb
    member t.Grid = grid