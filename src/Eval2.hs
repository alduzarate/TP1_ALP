module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

--Tipo error

-- Estado nulo
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of
                Just n -> Right n
                _      -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v n s = M.insert v n s

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s         = Right (Skip :!: s)
stepComm (Let v e) s    = case evalExp e s of 
                            Right (evalExpression :!: s') -> 
                              Right (Skip :!: update v evalExpression s')
                            Left x -> Left x                           

stepComm (Seq Skip c1) s  = Right (c1 :!: s)
stepComm (Seq c1 c2) s  = case stepComm c1 s of  
                            Right (c1' :!: s') -> Right (Seq c1' c2 :!: s')
                            Left x -> Left x
                            
stepComm (IfThenElse cond c1 c2) s =  case evalExp cond s of
                                        Right (cond' :!: s')-> 
                                          if cond'  then Right (c1 :!: s') 
                                                    else Right (c2 :!: s')
                                        Left x -> Left x

stepComm w@(While cond c1) s =  case evalExp cond s of
                                  Right (cond' :!: s') -> 
                                    if  cond' then Right (Seq c1 w :!: s')
                                              else Right (Skip :!: s')

                                  Left x -> Left x
                                  
-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const n) s = Right (n :!: s)
evalExp (Var n) s = case lookfor n s  of
                      Right x -> Right (x :!: s)
                      Left  _ -> Left UndefVar
                      
evalExp (UMinus e) s =  case evalExp e s of
                          Right (e' :!: s') -> Right (-e' :!: s')
                          Left n -> Left n

evalExp (Plus a b) s = case evalExp a s of
                          Right (a' :!: s') -> case evalExp b s' of
                                                  Right (b' :!: s'') -> Right (a' + b' :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x 

evalExp (Minus a b) s = case evalExp a s of
                          Right (a' :!: s') -> case evalExp b s' of
                                                  Right (b' :!: s'') -> Right (a' - b' :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x 

evalExp (Times a b) s = case evalExp a s of
                          Right (a' :!: s') -> case evalExp b s' of
                                                  Right (b' :!: s'') -> Right (a' * b' :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x 

evalExp (Div a b) s = case evalExp a s of
                          Right (a' :!: s') -> case evalExp b s' of
                                                  Right (b' :!: s'') -> if b' /= 0 then Right (div a' b' :!: s'')
                                                                                   else Left DivByZero
                                                  Left x -> Left x
                          Left x -> Left x 

evalExp (EAssign v e) s = case evalExp e s of
                          Right (e' :!: s') -> Right (e' :!: update v e' s')
                          Left n -> Left n

evalExp (ESeq e1 e2) s = case evalExp e1 s of
                          Right (e1' :!: s') -> case evalExp e2 s' of
                                                  Right (e2' :!: s'') -> Right (e2' :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x
                            
evalExp BTrue s = Right (True :!: s)
evalExp BFalse s = Right (False :!: s)

evalExp (Lt e1 e2) s = case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 < n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x
                                                    
evalExp (Gt e1 e2) s = case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 > n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x

evalExp (And e1 e2) s = case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 && n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x

evalExp (Or e1 e2) s = case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 || n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x

evalExp (Not e) s = case evalExp e s of
                          Right (e' :!: s') -> Right (not e' :!: s')
                          Left n -> Left n

evalExp (Eq e1 e2) s = case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 == n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x

evalExp (NEq e1 e2) s =  case evalExp e1 s of
                          Right (n1 :!: s') -> case evalExp e2 s' of
                                                  Right (n2 :!: s'') -> Right ((n1 /= n2) :!: s'')
                                                  Left x -> Left x
                          Left x -> Left x
