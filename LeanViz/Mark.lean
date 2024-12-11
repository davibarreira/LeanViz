import LeanViz.Primitives
import LeanViz.FreeMonad

open Primitives
open FreeMonad
class MarkInterface (a : Type) where
  θ : a -> Array Prim

structure Mark where
  {T : Type}
  [inst : MarkInterface T]
  [strg : ToString T]
  val : T

def Mark.θ : Mark → Array Prim := fun m => m.inst.θ m.val

instance : ToString Mark where
  toString p := @ToString.toString p.T p.strg p.val

instance : MarkInterface Circle where
  θ p := #[prim p]

instance : MarkInterface Line where
  θ p := #[prim p]

structure Head where
  size : Float
  smile : Float
deriving Repr
instance : ToString Head where
  toString h := "Head (size: " ++ toString h.size ++ ", smile: " ++ toString h.smile ++ ")"


def Head.o : Head := Head.mk 1.0 0.0

instance : MarkInterface Head where
  θ h :=
  let eyes := (Circle.mk 0.3 ⊞[-0.8,1]) + (Circle.mk 0.3 ⊞[0.8,1])
  (Circle.mk 2.0 ⊞[0,0] + eyes + Line.mk (⊞[-1,-0.5], ⊞[1,-0.5]))
    -- let eyes := (Circle.mk 0.3 ⊞[-0.8,1]) + (Circle.mk 0.3 ⊞[0.8,1])
    -- let smile := Line.mk (⊞[-1,-0.5], ⊞[1,-0.5])
    -- let head := Circle.mk h.size ⊞[0,0]
    -- eyes + smile + head


def algθ : 𝕋 (Array Prim) → Array Prim
  | 𝕋.pure x => x
  | 𝕋.comp x y => (algθ x) + (algθ y)
  | 𝕋.act h x => algθ x

def Mark.flat (t : 𝕋 Mark) : Array Prim := algθ (Mark.θ <$> t)

open ProofWidgets Svg in
private def frame : Frame where
  xmin   := -2
  ymin   := -2
  xSize  := 4
  width  := 400
  height := 400

open ProofWidgets Svg in
def Mark.draw (t : 𝕋 Mark) (fr : Frame := frame) : ProofWidgets.Html := drawsvg (Mark.flat t) fr

def x : 𝕋 Mark := pure ⟨Head.o⟩
#eval algθ (Mark.θ <$> x)

def y :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Head.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> y)

def z :𝕋 Mark  := 𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.comp (𝕋.pure ⟨Circle.o⟩) (𝕋.pure ⟨Line.o⟩))
#eval algθ (Mark.θ <$> z)

-- #eval Mark.mk Circle.o
-- #eval prim Circle.o
-- #eval prim Circle.o

open ProofWidgets Svg in
private def frame2 : Frame where
  xmin   := -5
  ymin   := -5
  xSize  := 10
  width  := 400
  height := 400

#html drawsvg (algθ (Mark.θ <$> x))
#html Mark.draw y
#html Mark.draw z
