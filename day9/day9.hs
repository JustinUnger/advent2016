import Control.Monad.State

type Replicate = (Int, Int)
type Mstate = (Maybe Replicate, String)

pr :: State Mstate String
pr = undefined

