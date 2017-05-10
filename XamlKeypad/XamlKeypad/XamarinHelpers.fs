﻿module XamarinHelpers

open Xamarin.Forms

type Grid with
    member t.ColumnWidths
        with set l =
            let cd = ColumnDefinitionCollection()
            for gl in l do cd.Add(ColumnDefinition(Width = gl))
            t.ColumnDefinitions <- cd
    member t.RowHeights
        with set l =
            let rd = RowDefinitionCollection()
            for gl in l do rd.Add(RowDefinition(Height = gl))
            t.RowDefinitions <- rd
    member t.Cells // rename? Add e.g. Array2D too
        with set l =
            t.Children.Clear()
            for (v,c,r) in l do t.Children.Add(v,c,r)
    /// NB doesn't clear previous items
    member t.FirstRow //check indexed property
        with set l =
            l |> Seq.iteri (fun i v -> t.Children.Add(v,i,0))
    /// NB doesn't clear previous items
    member t.SecondRow
        with set l =
            l |> Seq.iteri (fun i v -> t.Children.Add(v,i,1))
    /// NB doesn't clear previous items
    member t.FirstColumn
        with set l =
            l |> Seq.iteri (fun i v -> t.Children.Add(v,0,i))

type StackLayout with
    member t.Views //rename?
        with set (l: seq<View>) =
            t.Children.Clear()
            for c in l do t.Children.Add c
        and get() = t.Children |> seq

type View with
    /// NB does not remove any previous handlers
    member t.Tapped
        with set f =
            let tgr = TapGestureRecognizer()
            tgr.Tapped.Add(f)
            t.GestureRecognizers.Add(tgr)

type Button with
    member t.Click
        with set f = t.Clicked.Add (fun _ -> f())

type Switch with
    member t.Toggle
        with set f = t.Toggled.Add (fun x -> f(x.Value))