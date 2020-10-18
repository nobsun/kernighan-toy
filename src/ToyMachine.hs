module ToyMachine where

import Data.Char
import Prompt

type Memory = ([Cell], [Cell])

outOfRange :: Memory -> Memory
outOfRange (orig,_) = (orig, [])

next :: Memory -> Memory
next (orig, _:cs) = (orig, cs)

top :: Memory -> Cell
top (_, c:_) = c

jump :: Label -> Memory -> Memory
jump lab (orig,cs) = (orig, dropWhile ((lab /=) . fst) cs)

type Cell  = (Label, Content)
type Label = String

data Content
    = Instruction Instruction
    | Data Int

type Instruction = (Operator, Operand)

data Operator
    = GET
    | PRINT
    | STOP
    | LOAD
    | STORE
    | ADD
    | SUB
    | GOTO
    | IFZERO
    | IFPOS
    deriving (Eq, Ord, Enum, Show, Read)

data Operand
    = None
    | Imm Int
    | Ind Label
    deriving (Eq, Show)

update :: Label -> Content -> Memory -> Memory
update lab c (orig,cells) = case break ((lab ==) . fst) cells of
    (ms,_:ns) -> (orig, cycle (zipWith (flip const) orig (ms ++ (lab, c) : ns)))

getVal :: Memory -> Operand -> Int
getVal (_,cells) arg = case arg of
    Imm n   -> n
    Ind lab -> case dropWhile ((lab /=) . fst) cells of
        (_, Data n):_ -> n

--
type ToyState = (Memory, Ac, Ins, Outs)
type Ac       = Int
type Ins      = [String]
type Outs     = String
type Transition = ToyState -> ToyState

fetch :: ToyState -> Content
decode :: Content -> Transition
execute :: Transition -> Transition

fetch (mem,_,_,_) = snd (top mem)
decode c = case c of
    Instruction instr -> case instr of
        (GET, _)    -> iGet
        (PRINT, _)  -> iPrint
        (STOP,_)    -> iStop
        (LOAD,val)  -> iLoad val
        (STORE,m)   -> iStore m
        (ADD,val)   -> iAdd val
        (SUB,val)   -> iSub val
        (GOTO,l)    -> iGoto l
        (IFZERO,l)  -> iIfZero l
        (IFPOS,l)   -> iIfPos l
execute = id

iGet, iPrint, iStop :: Transition
iLoad, iStore, iAdd, iSub, iGoto, iIfZero, iIfPos :: Operand -> Transition

iGet (mem,ac,ins,_) = prompt "Input Number: " (next mem, readInt (head ins), tail ins, "")
iPrint (mem,ac,ins,_) = (next mem, ac, ins, show ac)
iStop (mem,ac,ins,_)  = (outOfRange mem, ac, ins, "")
iLoad arg (mem,ac,ins,_) = (next mem, getVal mem arg, ins, "")
iStore arg (mem,ac,ins,_) = case arg of
    Ind lab -> (next (update lab (Data ac) mem), ac, ins, "")
iAdd arg (mem,ac,ins,_)
    = (next mem, ac + getVal mem arg, ins, "")
iSub arg (mem,ac,ins,_)
    = (next mem, ac - getVal mem arg, ins, "")
iGoto arg (mem,ac,ins,_) = case arg of
    Ind lab -> (jump lab mem, ac, ins, "")
iIfZero arg (mem,ac,ins,_) = if ac == 0
    then case arg of
        Ind lab -> (jump lab mem, ac, ins, "")
    else (next mem, ac, ins, "")
iIfPos arg (mem,ac,ins,_) = if ac >= 0
    then case arg of
        Ind lab -> (jump lab mem, ac, ins, "")
    else (next mem, ac, ins, "")

execTrace :: ToyState -> [ToyState]
execTrace st = st : rests
    where
        rests = if isFinal st
            then []
            else execTrace (step st)

isFinal :: ToyState -> Bool
isFinal ((_,cs),_,_,_) = null cs

step :: ToyState -> ToyState
step st = execute (decode (fetch st)) st

runToy :: SourceCode -> String -> String
runToy code ins = unlines (foldr phi [] (execTrace (loadProg code ins)))
    where
        phi (_,_,_,o) os = if null o then os else o:os

type SourceCode = String

loadProg :: SourceCode -> String -> ToyState
loadProg prog ins = case map readLine (lines (map toUpper prog)) of
    lls -> ((lls,cycle lls), 0, lines ins, "")

readLine :: String -> (Label, Content)
readLine ccs@(c:cs) = if isSpace c
    then case words cs of
        [o]   -> ("", Instruction (readOperator o, None))
        [o,a] -> ("", Instruction (readOperator o, readOperand a))
    else case words ccs of
        [l,o]
            | all isDigit o -> (l, Data (readInt o))
            | otherwise     -> (l, Instruction (readOperator o, None))
        [l,o,a]             -> (l, Instruction (readOperator o, readOperand a))

readOperator :: String -> Operator
readOperator = read

readOperand :: String -> Operand
readOperand s = if all isDigit s
  then Imm (readInt s)
  else Ind s

readInt :: String -> Int
readInt = read

sampleSource :: SourceCode
sampleSource = unlines
    [ "Top GET"
    , "    IFZero bot"
    , "    Add sum"
    , "    store sum"
    , "    goto top"
    , "Bot load sum"
    , "    print"
    , "    stop"
    , "sum 0"
    ]

sampleInputs :: String
sampleInputs = unlines (map show [3,1,4,1,5,9,0])

