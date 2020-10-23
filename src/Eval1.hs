module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- {key1 => value1, key2 => value2}
-- keys -> variables
-- values -> valor de la variable en ese estado

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty 

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v n s = M.insert v n s

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- (x :!: y)
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s         = (Skip :!: s)
stepComm (Let v e) s    = let 
                            (evalExpression :!: s') = evalExp e s
                          in
                            (Skip :!: update v evalExpression s')

stepComm (Seq Skip c1) s  = (c1 :!: s)
stepComm (Seq c1 c2) s  = let  
                            (n1 :!: s') = stepComm c1 s
                          in
                            (Seq n1 c2 :!: s')

stepComm (IfThenElse cond c1 c2) s =  let 
                                        (cond' :!: s') = evalExp cond s
                                      in
                                        if cond'  then (c1 :!: s') 
                                                  else (c2 :!: s')

stepComm w@(While cond c1) s =  let
                                  (cond' :!: s') = evalExp cond s
                                in
                                  if  cond' then (Seq c1 w :!: s')
                                            else (Skip :!: s')

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = (n :!: s)
evalExp (Var n) s = let x = lookfor n s 
                    in (x :!: s)

evalExp (UMinus e) s =  let
                          (n :!: s') = evalExp e s
                        in
                          (-n :!: s')

evalExp (Plus e1 e2) s = let
                          (n1 :!: s') = evalExp e1 s
                          (n2 :!: s'') = evalExp e2 s'
                        in ( n1 + n2 :!: s'')

evalExp (Minus e1 e2) s = let
                          (n1 :!: s') = evalExp e1 s
                          (n2 :!: s'') = evalExp e2 s'
                        in (n1 - n2 :!: s'')
                        
evalExp (Times e1 e2) s = let
                          (n1 :!: s') = evalExp e1 s
                          (n2 :!: s'') = evalExp e2 s'
                        in (n1 * n2 :!: s'')

evalExp (Div e1 e2) s = let
                          (n1 :!: s') = evalExp e1 s
                          (n2 :!: s'') = evalExp e2 s'
                        in (div n1 n2 :!: s'')

evalExp (EAssign v e) s = let
                            (n :!: s') = evalExp e s
                          in
                            (n :!: update v n s')

evalExp (ESeq e1 e2) s = let
                            (n1 :!: s')  = evalExp e1 s
                            (n2 :!: s'') = evalExp e2 s'
                          in (n2 :!: s'')
                            
evalExp BTrue s = (True :!: s)

evalExp BFalse s = (False :!: s)

evalExp (Lt e1 e2) s = let
                        (n1 :!: s') = evalExp e1 s
                        (n2 :!: s'') = evalExp e2 s'
                        in
                          ((n1 < n2) :!: s'')
                          
evalExp (Gt e1 e2) s = let
                        (n1 :!: s') = evalExp e1 s
                        (n2 :!: s'') = evalExp e2 s'
                        in 
                          ((n1 > n2) :!: s'')

evalExp (And e1 e2) s = let
                        (n1 :!: s') = evalExp e1 s
                        (n2 :!: s'') = evalExp e2 s'
                        in 
                          ((n1 && n2) :!: s'')

evalExp (Or e1 e2) s = let
                        (n1 :!: s') = evalExp e1 s
                        (n2 :!: s'') = evalExp e2 s'
                      in 
                        ((n1 || n2) :!: s'')

evalExp (Not e1) s = let
                      (n1 :!: s') = evalExp e1 s
                      in
                        (not n1 :!: s')

evalExp (Eq e1 e2) s = let
                        (n1 :!: s') = evalExp e1 s
                        (n2 :!: s'') = evalExp e2 s'
                        in 
                          (n1 == n2 :!: s'')

evalExp (NEq e1 e2) s =  let
                          (n1 :!: s') = evalExp e1 s
                          (n2 :!: s'') = evalExp e2 s'
                          in 
                            (n1 /= n2 :!: s'')