-- Continuing module Named:
data Defn = Defn -- Type exported, constructor hidden.

type Defining f = (Coercible f Defn, Coercible Defn f)

defn :: Defining f => a -> (a ~~ f)
defn = coerce
