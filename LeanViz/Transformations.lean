import SciLean

open SciLean Scalar RealScalar

set_option autoImplicit true
set_default_scalar Float

structure G (n : ℕ) where
  A : Float^[n,n]
  b : Float^[n]

def G.eval (f : G n) (x : Float^[n]) := f.A * x + f.b
def G.comp (f g : G n) : G n :=
  { A := f.A * g.A, b := f.A * g.b + f.b }

def G.translate (t : Float^[n]) : G n :=
{
  A := 𝐈 n
  b := t
}

def G.rotate (θ : Float) : G 2 :=
{
  A := ⊞[cos θ, -sin θ;sin θ, cos θ]
  b := 0
}

def G.scale (n : ℕ) (s : Float) : G n :=
{
  A := s • 𝐈 n
  b := 0
}

def examplePoint : Float^[2] := ⊞[1.0, 1.0]  -- Point [1, 1]
#eval examplePoint

def T : G 2 :=
  G.translate ⊞[2.0, 3.0]  -- Translation vector [2, 3]

def R : G 2 :=
  G.rotate π  -- Translation vector [2, 3]

#eval T.eval examplePoint
#eval (T.comp T).eval examplePoint
#eval R.eval examplePoint

instance : HMul (G n) (Float^[n]) (Float^[n]) where
  hMul := G.eval

instance : HMul (G n) (G n) (G n) where
  hMul := G.comp

infixr:80 " ∘ " => G.comp

#eval T * examplePoint
#eval (T ∘ T) * examplePoint
#eval (T ∘ T ∘ T) * examplePoint
#eval T ∘ T ∘ T * examplePoint
