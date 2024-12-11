import SciLean

open SciLean Scalar RealScalar

set_option autoImplicit true
set_default_scalar Float

namespace G

-- Auxiliar function for Vector Norm
def vecnorm (v : Float^[n]) : Float :=
  (v.foldl (λ acc x => acc + x*x) 0.0).sqrt

instance : HMul Float (Float^[2]) (Float^[2]) where
  hMul x y := ⊞[x * y[1], x * y[2]]
instance : HMul (Float^[2]) Float (Float^[2]) where
  hMul y x := ⊞[x * y[1], x * y[2]]

structure G (n : ℕ) where
  A : Float^[n,n]
  b : Float^[n]

def G.eval (f : G n) (x : Float^[n]) := f.A * x + f.b
def G.comp (f g : G n) : G n :=
  { A := f.A * g.A, b := f.A * g.b + f.b }
instance : HMul (G n) (Float^[n]) (Float^[n]) where
  hMul := G.eval

instance : HMul (G n) (G n) (G n) where
  hMul := G.comp

infixr:80 " ∘ " => G.comp

def G.translate (t : Float^[2]) : G 2 :=
{
  A := 𝐈 2
  b := t
}

def G.rotate (θ : Float) : G 2 :=
{
  A := ⊞[cos θ, -sin θ;sin θ, cos θ]
  b := 0
}

def G.scale (s : Float) : G 2 :=
{
  A := s • 𝐈 2
  b := 0
}

private def examplePoint : Float^[2] := ⊞[1.0, 1.0]  -- Point [1, 1]
#eval examplePoint

def T : G 2 :=
  G.translate ⊞[2.0, 3.0]

def R : G 2 :=
  G.rotate π

def S : G 2 :=
  G.scale 2.0

#eval T.eval examplePoint
#eval (T.comp T).eval examplePoint
#eval R.eval examplePoint
#eval T * examplePoint
#eval (T ∘ T) * examplePoint
#eval (T ∘ T ∘ T) * examplePoint
#eval T ∘ T ∘ T * examplePoint


#eval norm examplePoint
#eval examplePoint[2]



end G
