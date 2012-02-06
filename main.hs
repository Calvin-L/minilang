
module Main where

import Lex
import Parse
import Data.IORef
import Control.Monad
import Debug.Trace

type Env = [(String, Integer)]
type Funcs = [(String, [String], String, [Statement])] -- id, args, out, stms
type State = (Env, Funcs)

newState = ([], [])

getEnv :: State -> Env
getEnv = fst

getFuncs :: State -> Funcs
getFuncs = snd

getVar [] v = 0
getVar ((name,value):rest) v =
    if v == name
       then value
       else getVar rest v

setVar [] id val = [(id, val)]
setVar (a@(name,_):rest) id val =
    if id == name
       then (id, val) : rest
       else a : (setVar rest id val)

getFunc [] f = error $ "Function " ++ f ++ " is undefined"
getFunc funcs@(a@(name, _, _, _):rest) f =
    if f == name
       then a
       else getFunc rest f

numFactors 0 _ = 0
numFactors x y = if x `mod` y == 0 then 1 + numFactors (x `div` y) y else 0

-- optimized calls for things we know we can implement
opt "numFactors" [x, y] = Just $ numFactors x y
opt "sub"   [x, y] = Just $ max (x - y) 0
opt "div"   [x, y] = Just $ x `div` y
opt "add"   [x, y] = Just $ x + y
opt "times" [x, y] = Just $ x * y
opt "pow"   [x, y] = Just $ x ^ y
opt _       _      = Nothing

get :: IORef State -> Expression -> IO Integer
get st (Var id) = do
  state <- readIORef st
  return $ getVar (getEnv state) id
get st (Call id values) = do
  realValues <- sequence $ map (\v -> get st v) values
  let result = opt id realValues
  case result of
    Just x -> return x
    _ -> do
      state@(env, funcs) <- readIORef st
      let (f, args, out, stms) = getFunc funcs id
      newSt <- newIORef ([], funcs)
      sequence_ $ map 
        (\(arg,value) -> set newSt arg value) 
        (zip args realValues)
      run newSt stms
      (env, _) <- readIORef newSt
      return $ getVar env out
get st (Int x) = return x

set st id val = do
  (env, funcs) <- readIORef st
  writeIORef st (setVar env id val, funcs)

addFunc st id args out stms = do
  (env, funcs) <- readIORef st
  writeIORef st (env, (id, args, out, stms) : funcs)

nothing = return ()

runstm st (Set id exp) = do x <- get st exp; set st id x
runstm st (Inc id times) = do x <- get st (Var id); set st id (x + times)

runstm st (Func id args out stms) = addFunc st id args out stms

runstm st (Loop id stm)  = do
  i <- newIORef 0
  go st i
     where
       go st i = do
         val1 <- readIORef i
         val2 <- get st id
         if val1 >= val2 
            then nothing
            else do
              writeIORef i (val1 + 1)
              run st stm
              go st i

runstm st (Block stms)   = run st stms

runstm st (Write [])     = nothing
runstm st (Write exps)    = do 
  x <- get st (head exps)
  putStrLn (show x);
  runstm st (Write (tail exps))

run :: IORef State -> [Statement] -> IO ()
run st [] = return ()
run st (x:l) = do runstm st x; run st l

main = do
  s <- getContents
  let ast = miniParse $ miniLex s
  st <- newIORef newState
  run st ast

