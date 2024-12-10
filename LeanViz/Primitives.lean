import SciLean
import ProofWidgets.Data.Svg
import ProofWidgets.Component.HtmlDisplay

set_option autoImplicit true
set_default_scalar Float

class PrimInterface (α : Type) where
  draw : α → (fr : ProofWidgets.Svg.Frame) → ProofWidgets.Svg.Element fr

structure Circle where
  r : Float
  c : Float^[2]
instance : ToString Circle where
  toString c := "Circle (r: " ++ toString c.r ++ ", c: " ++ toString c.c ++ ")"
def Circle.o : Circle := Circle.mk 1 ⊞[0,0]

open ProofWidgets Svg in
def drawCircle (c : Circle) (fr : Frame) : Element fr :=
  circle (c.c[1],c.c[2]) (.abs c.r) |>.setStroke (0.,0.,0.) (.px 2) |>.setFill (0.,1.,1.) |>.setId "point1"
#eval Circle.o
instance : PrimInterface Circle where
  draw := drawCircle

structure Line where
  pts : (Float^[2]) × (Float^[2])
instance : ToString Line where
  toString l := "Line (l: " ++ toString l.pts ++ ")"
def Line.o : Line := Line.mk (⊞[0,0], ⊞[1,1])
open ProofWidgets Svg in
def drawLine (l : Line) (fr : Frame) : Element fr :=
  line (l.pts.fst[1],l.pts.fst[2]) (l.pts.snd[1],l.pts.snd[2]) |>.setStroke (1.,0.,0.) (.px 2)
#eval Line.o
instance : PrimInterface Line where
  draw := drawLine

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400
def x := @ProofWidgets.Svg.line frame (0.,0.) (1.,0.)



-- #check PrimInterface.mk Circle.o

#check (PrimInterface.draw)
def d := PrimInterface.draw Circle.o frame

#check d
#html d.toHtml

  -- @ProofWidgets.Svg.line fr (0.,0.) (1.,0.)
--   open ProofWidgets Svg in
--   fun fr : Frame => @line fr (0.,0.) (1.,0.)

structure Prim where
  {T : Type}
  [inst : PrimInterface T]
  [strg : ToString T]
  val : T

-- def Prim.draw : Prim → Int := fun p => p.inst.fm m.val
def Prim.draw (p : Prim) (fr : ProofWidgets.Svg.Frame) : ProofWidgets.Svg.Element fr :=
  p.inst.draw p.val fr

instance : ToString Prim where
  toString p := @ToString.toString p.T p.strg p.val

#eval Prim.mk Circle.o
def prim {α : Type} [PrimInterface α] [ToString α] (a : α) : Prim := Prim.mk a
#eval prim Line.o

def fooo : Array Prim := #[⟨Circle.o⟩, ⟨Line.o⟩]

-- def foo : List Prim := [prim Circle.o, prim Line.o]
def foo : Array Prim := #[prim Circle.o, prim Line.o]

open ProofWidgets Svg in
private def svg : Svg frame :=
  { elements := Array.map (λx => Prim.draw x frame) foo}

def drawsvg (a : Array Prim) (fr : Frame) : ProofWidgets.Html :=
  let svg : ProofWidgets.Svg frame := { elements := Array.map (λx => Prim.draw x frame) a}
  svg.toHtml

#html svg.toHtml
#check svg.toHtml

-- #check Array.map prim #[prim Circle.o, prim Line.o]

-- def Prim.comp {α β : Type} [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] (p1 : α) (p2 : β) : Array Prim :=
--   #[prim p1, prim p2]

instance  {α β : Type} [PrimInterface α] [PrimInterface β] [ToString α] [ToString β] : HAdd  α β (Array Prim) where
  hAdd p1 p2 := #[prim p1, prim p2]
instance  {α : Type} [PrimInterface α] [ToString α] : HAdd  α (Array Prim) (Array Prim) where
  hAdd p a := #[prim p] ++ a
instance  {α : Type} [PrimInterface α] [ToString α] : HAdd  (Array Prim) α (Array Prim) where
  hAdd a p := a ++ #[prim p]
instance  : HAdd  Prim Prim (Array Prim) where
  hAdd p1 p2 := #[p1, p2]
instance  : HAdd  Prim (Array Prim) (Array Prim) where
  hAdd p1 p2 := #[p1] ++ p2
instance  : HAdd  (Array Prim) Prim (Array Prim) where
  hAdd p1 p2 := p1 ++ #[p2]
instance  : HAdd  (Array Prim) (Array Prim) (Array Prim) where
  hAdd p1 p2 := p1 ++ p2

-- infixr:80 " ++ " => Prim.comp

#eval Circle.o + Circle.o + Line.o
#html drawsvg (Circle.o + Circle.o + Line.o) frame
