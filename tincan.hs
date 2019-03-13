{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.State
import Control.Monad.Extra
import Control.Lens
import Text.Read (readMaybe)
import Data.Char
import Data.Maybe
import Data.Map (Map)
import Data.List.Extra
import System.Environment
import qualified Data.Map as Map

data Token = Dollar | At | Ampersand | Variable Char | Value Int deriving (Eq, Show)

data ProgramState = ProgramState { _program :: [(Token, Token, Token)]
                                 , _len :: Int
                                 , _currentLine :: Int
                                 , _pc :: Int
                                 , _stack :: [Int]
                                 , _executionFinished :: Bool
                                 , _variables :: Map Char Int
                                 } deriving (Show)
                                 
makeLenses ''ProgramState

stripLines :: [String] -> [String]
stripLines ls = [filter (/='#') l | l <- ls, length l == 40, head l == '#', last l == '#']

toToken :: String -> Token
toToken (readMaybe -> Just v) = Value v
toToken [c] | c `elem` ['A'..'Z'] = Variable c
toToken "$" = Dollar
toToken "@" = At
toToken "&" = Ampersand
toToken t   = error ("Could not parse '" ++ t ++ "'")

tokenizeLine :: String -> (Token, Token, Token)
tokenizeLine (map trim . splitOn "," -> [x,y,z]) = (toToken x, toToken y, toToken z)
tokenizeLine line = error $ "Could not tokenize line: " ++ line

tokenToInt :: Token -> State ProgramState Int
tokenToInt Dollar = use currentLine
tokenToInt At = use pc
tokenToInt Ampersand = uses currentLine (+1)
tokenToInt (Variable c) = getVariable c
tokenToInt (Value i) = return i

getVariable :: Char -> State ProgramState Int
getVariable var = uses variables (^?! ix var)

updateVariable :: Char -> Int -> State ProgramState ()
updateVariable var val = variables . ix var .= val

jump :: Int -> State ProgramState ()
jump addr = do
  currentLine .= addr
  addressOutside <- uses len (\l -> addr >= l || addr < 0)
  when addressOutside $ executionFinished .= True

jumpToNextLine :: State ProgramState ()
jumpToNextLine = uses currentLine (+1) >>= jump

push :: Int -> State ProgramState ()
push val = stack %= (val:)

executeProgram :: State ProgramState ()
executeProgram = (!!) <$> use program <*> use currentLine >>= \case
  (a, Variable b, c) -> do
    differential <- tokenToInt a
    value <- getVariable b
    let diff = value - differential
    pc += 1
    updateVariable b diff
    tokenToInt c >>= \case
      -1      -> push diff >> jumpToNextLine
      address -> if diff <= 0 then jump address else jumpToNextLine  
    unlessM (use executionFinished) executeProgram
  _ -> error "Could not execute line"

generateProgram :: [String] -> ProgramState
generateProgram xxs = ProgramState (map tokenizeLine xxs) (length xxs) 0 0 [] False (Map.fromList $ zip ['A'..'Z'] $ repeat 0)

parseProgram :: String -> ProgramState
parseProgram = generateProgram . stripLines . lines

printStack :: ProgramState -> IO ()
printStack = putStrLn . reverse . map chr . _stack

main :: IO ()
main = getArgs >>= \case
  [file] -> readFile file >>= printStack . execState executeProgram . parseProgram
  _      -> getProgName >>= \name -> putStrLn $ "Usage: " ++ name ++ " [filename]"