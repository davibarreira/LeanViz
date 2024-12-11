import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay
-- import Std.Data.HashMap

open ProofWidgets.Svg

inductive StyleSize where
  | px   (size : Nat)
  | abs  (size : Float)

structure Style where
  strokeColor := (none : Option Color)
  strokeWidth := (none : Option StyleSize)
  fillColor   := (none : Option Color)

def styToSize (s : StyleSize) (fr : Frame) : Size fr :=
  match s with
  | StyleSize.px x => Size.px x
  | StyleSize.abs x => Size.abs x

private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

def x : StyleSize := .px 10
#eval styToSize x frame

def y : Size frame := Size.px 10

def exampleSizeAbs : Size frame := Size.abs 5.5
def z : Style := {}
