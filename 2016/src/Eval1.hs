module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip           = return ()
evalComm (Let v ie)     = evalIntExp ie >>= update v
evalComm (Seq c1 c2)    = evalComm c1 >>= \_ -> evalComm c2
evalComm (Cond b c1 c2) = evalBoolExp b >>= \b1 -> if b1 then evalComm c1 else evalComm c2
evalComm (While b c)    = evalBoolExp b >>= \b1 -> if b1 then evalComm c >>= \_ -> evalComm (While b c)
                                                         else evalComm Skip

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const n)  = return n
evalIntExp (Var v)    = lookfor v
evalIntExp (UMinus i) = evalIntExp i >>= \i1 -> return (-i1)
evalIntExp (Plus a b) = evalIntExp a >>= \a1 -> (evalIntExp b >>= \b1 -> return (a1 + b1))
evalIntExp (Minus a b)= evalIntExp a >>= \a1 -> (evalIntExp b >>= \b1 -> return (a1 - b1))
evalIntExp (Times a b)= evalIntExp a >>= \a1 -> (evalIntExp b >>= \b1 -> return (a1 * b1))
evalIntExp (Div a b)  = evalIntExp a >>= \a1 -> (evalIntExp b >>= \b1 -> return (div a1 b1))

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (Eq ie ie2)  = do i <- evalIntExp ie
                              i2<- evalIntExp ie2
                              return (i == i2)
evalBoolExp (Lt ie ie2)  = do i <- evalIntExp ie
                              i2<- evalIntExp ie2
                              return (i < i2)
evalBoolExp (Gt ie ie2)  = do i <- evalIntExp ie
                              i2<- evalIntExp ie2
                              return (i > i2)
evalBoolExp (And ie ie2) = do i <- evalBoolExp ie
                              i2<- evalBoolExp ie2
                              return (i && i2)
evalBoolExp (Or ie ie2)  = do i <- evalBoolExp ie
                              i2<- evalBoolExp ie2
                              return (i || i2)
evalBoolExp (Not ie)     = do i <- evalBoolExp ie
                              return ( not i)
