data ExpArit = Lit Double
 | Add ExpArit ExpArit
 | Sub ExpArit ExpArit
 | Mul ExpArit ExpArit
 | Div ExpArit ExpArit
 deriving (Eq,Show)

avalia :: ExpArit -> Double
avalia (Lit x)   = x
avalia (Add x y) = avalia x + avalia y
avalia (Sub x y) = avalia x - avalia y
avalia (Mul x y) = avalia x * avalia y
avalia (Div x y) = avalia x / avalia y

instance Num ExpArit where
 x + y         = Add x y
 x - y         = Sub x y
 x * y         = Mul x y
 abs x         = error "Nao implementado"
 signum x      = error "Nao implementado"
 fromInteger x = Lit (fromIntegral x)

instance Fractional ExpArit where
 x / y        = Div x y
 fromRational x = Lit (fromRational x)
