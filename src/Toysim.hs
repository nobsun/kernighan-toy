module Toysim where

import qualified Data.Array as A
import qualified Data.Binary as B
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.List
import Data.Maybe

-- :::
-- 
-- # トイ・コンピュータ
-- 
-- 『ディジタル作法』には、トイ・コンピュータの構成と動作について以下のように記述されています。
-- 
-- > トイ・コンピュータには命令とデータを格納するメモリがあり、それに加えて**アキュムレータ**という名前の、数値を1つ保持できる記憶領域が備わっています。
-- > ... 中略 ...
-- > メモリのそれぞれの場所には数値が1個、または命令が1個保持されているものとします。ですから、メモリ中には命令がいくつかとデータ項目がいくつか入っていて、これらを合わせたものがプログラムだということになります。
-- > ... 中略 ...
-- > 動作中には、CPUは最初のメモリ番地から始めて、次のような単純なサイクルを繰り返します。
-- > - **フェッチ（取り出し）**：メモリから次の命令を取り出す
-- > - **デコード（解読）**：その命令が何をするかを判断する
-- > - **実行**：命令の動作を実行して、フェッチに戻る
-- 
-- トイ・コンピュータの動作は、命令的疑似言語で表現すると
-- 
-- ```
-- toy.init(program);
-- while (!toy.is_stopped()) {
--     toy.fetch();
--     toy.decode();
--     toy.execute();
-- }
-- ```
-- 
-- のようになるでしょう。では関数的にはどのような表現になるでしょうか。
-- 
-- ## トイ・コンピュータの動作の表現
-- 
-- 1つの命令の「実行」を、実行前の「状態」から実行後の「状態」への状態遷移を表す関数の適用として表現できます。
-- 
execute :: Instruction -> ToyState -> ToyState
execute instr state = instr state

-- 
-- すなわち、命令は、状態遷移関数というわけです。
-- 
type Instruction = ToyState -> ToyState

-- 
-- そうすると、フェッチとデコードは以下のような型を持つ関数になるでしょう。
-- 
fetch :: ToyState -> Code
decode :: Code -> Instruction

-- 
-- さらに、フェッチ-デコード-実行の1サイクル分を1段の状態遷移関数だとすると、
-- 
step :: ToyState -> ToyState
step state = execute (decode (fetch state)) state

-- 
-- そして、1段の状態遷移関数`step`があれば、トイ・コンピュータ上でのプログラムの実行は、初期状態から停止状態までの状態遷移列として表現できます。
-- 
executeProgram :: ToyState -> [ToyState]
executeProgram state = state : rests
    where
        rests | isFinal state = []
              | otherwise     = executeProgram (step state)

-- 
-- ## トイ・コンピュータの状態
-- 
-- トイ・コンピューターの「状態」はどのような構成になっていればよいでしょうか。
-- 
-- 第1に、フェッチ`fetch`によって現在の状態から、実行すべき命令コードを取り出せなければなりません。実行`execute`によって構成される次の状態には次に実行すべき命令コードが含まれていなければなりません。したがって、「状態」にはプログラムを含む「メモリ」が必要です。
-- 第2に、1つの命令を実行して得られた値を次に伝えるために、「アキュムレータ」が必要です。
-- 第3に、外部からの「入力チャンネル」が必要です
-- 第4に、外部への「出力」が必要です
-- 
-- というわけで、トイ・コンピューターの状態`ToyState`は、メモリ`Memory`、アキュムレータ`Accumulator`、入力チャンネル`Channel`、出力`Output`の4つ組で表現します。
-- 
type ToyState = (Memory, Accumulator, Channel, Output)

-- 
-- ### メモリの表現
-- 
-- メモリの内容`Content`は、命令コード`Code`またはデータ`Data`です。個々の命令やデータはラベルを使って参照することがあるので、メモリはラベル付き内容`(Label, Content)`の並びとして表現しましょう。並びの表現としては、Haskellではリストを用いるのが一般的です^[リストによる表現では、任意の要素へのアクセスが、通常のRAMに期待される$O(1)$にはならないことに注意してください。]。リストであれば、フェッチのときに現在のメモリの先頭を取り出し、次の状態を構成するときは、先頭より後ろの部分を次のメモリとすれば、プログラムの順次実行が表現できます。前方へのジャンプ命令に対応しなければならないので、循環リスト`CyclicList a`を使って表現することにしましょう。
-- 
type Memory = CyclicList (Label, Content)

type CyclicList a = (Size, [a])
type Size = Int

listToCyclicList :: [a] -> CyclicList a
listToCyclicList [] = error "empty"
listToCyclicList xs = (length xs, cycle xs)

type Label = String
data Content
    = Code Code
    | Data Data

type Code = (Operator, Operand)
type Data = Int

-- 
-- トイ・コンピューターのコード`Code`はオペレータ`Operator`とオペランド`Operand`とのペアで表現します。オペレータは[トイ・コンピューターのシミュレーターサイト](https://kernighan.com/toysim.html)によれば、以下の10種類あります。オペランドは数値かラベルかそれとも無しの3種類です。
--  
data Operator
    = GET
    | PRINT
    | STOP
    | LOAD
    | STORE
    | ADD
    | SUB
    | GOTO
    | IFPOS
    | IFZERO
    deriving (Eq, Ord, Enum, Show, Read)

data Operand
    = None
    | Number Int
    | Label Label
    deriving (Eq, Show)

-- 
-- ### アキュムレータの表現
-- 
-- こちらは単純に数値で表現できます。
-- 
type Accumulator = Int

-- 
-- ### 入力チャンネルの表現
-- 
-- ユーザー入力チャンネル`Channel`は0個以上の入力`Input`列の並び`[Intput]`で表現しましょう。これは入力チャンネルとし機能し、入力命令の実行により、入力チャンネルの先頭の要素（文字列）が数値として解釈されてアキュムレータになります。次の状態を構成する入力チャンネルは現在の入力チャンネルの先頭より後ろの部分とすればよいでしょう。
-- 
type Channel = [Input]
type Input = String

-- 
-- ### 出力の表現
-- 
-- 出力命令が実行された結果として構成される次の状態に出力文字列が入ります。
-- 
type Output = String

-- 
-- ### `fetch`
-- 
-- `ToyState`の定義ができたので、`fetch`を定義しましょう。
-- 
fetch ((_,mem),_,_,_) = case mem of
    (_,Code code):_ -> code

-- 
-- ### `decode`
-- 
-- `decode`はコードから対応したインストラクションへの関数なので、以下のように定義できます。
-- 
decode (ope, opd)
    = (case ope of
    GET    -> iGet
    PRINT  -> iPrint
    LOAD   -> iLoad
    STORE  -> iStore
    ADD    -> iAdd
    SUB    -> iSub
    GOTO   -> iGoto
    IFPOS  -> iIfpos
    IFZERO -> iIfzero
    STOP   -> iStop
    ) opd

-- 
-- ## トイ・コンピューターのインストラクション
-- 
-- [カーニハン先生のサイトにトイ・コンピュータのシミュレータのページ](https://kernighan.com/toysim.html)があり、そこに提供されているインストラクションのコードと意味がまとめられています。
-- 
-- |ラベル|オペレータ^[トイ・シミュレーターのページではオペレータは小文字で表記し説明されているが、実際はオペレータにおいてもラベルにおいても大文字小文字は区別されていない。]|オペランド|意味|
-- |------|------|----|----|
-- ||`GET`||キーボードからアキュムレータに数値を読み込む|
-- ||`PRINT`||アキュムレータの内容を出力欄に表示|
-- ||`LOAD` |*Val*|アキュムレータに値（数値またはメモリ番地の内容）を設定（命令中の「値」は変化しない）|
-- ||`STORE` |*M*|アキュムレータの内容の写しをメモリ番地*M*に格納（アキュムレータの内容は変化しない）|
-- ||`ADD`|*Val*|アキュムレータの内容に値（数値またはメモリ番地の内容）を足し込む（命令中の「値」は変化しない）|
-- ||`SUB`|*Val*|アキュムレータの内容から値（数値またはメモリ番地の内容）を引く（命令中の「値」は変化しない）|
-- ||`GOTO`|*L*|ラベル*L*が付けられた命令に飛ぶ|
-- ||`IFPOS`|*L*|もしアキュムレータの内容が$0$または正であればラベル*L*が付けられた命令に飛ぶ|
-- ||`IFZERO`|*L*|もしアキュムレータの内容がちょうど$0$であればラベル*L*が付けられた命令に飛ぶ|
-- ||`STOP`||実行を停止する|
-- |*M*|*Num*||プログラムを起動する前に、このメモリ位置（番地*M*）を数値*Num*に設定する|
-- 
iGet, iPrint, iStop
 , iLoad, iStore, iAdd, iSub
 , iGoto, iIfpos, iIfzero :: Operand -> Instruction

iGet   _ ((sz,mem),acc,ins,_)
    = ((sz, tail mem), read (head ins), tail ins, "")

iPrint _ ((sz,mem),acc,ins,_) 
    = ((sz, tail mem), acc, ins, show acc)

iLoad  opd ((sz,mem),_,ins,_)
    = ((sz, tail mem), value (sz,mem) opd, ins, "")

iStore opd (m,acc,ins,_)
    = next (update opd acc m, acc, ins, "")

iAdd opd ((sz,mem),acc,ins,_)
    = ((sz, tail mem), acc + value (sz, mem) opd, ins, "")
    
iSub opd ((sz,mem),acc,ins,_)
    = ((sz, tail mem), acc - value (sz, mem) opd, ins, "")

iGoto (Label l) ((sz,mem),acc,ins,_) 
    = ((sz, mem'), acc, ins, "")
    where
        mem' = dropWhile ((l /=) . fst) mem

iIfpos lab@(Label _) st@(_,acc,_,_) = st'
    where
        st' = if acc >= 0 then iGoto lab st else next st

iIfzero lab@(Label l) st@(_,acc,_,_) = st'
    where
        st' = if acc == 0 then iGoto lab st else next st

iStop  _ ((_,mem),acc,ins,_)
    = ((0, mem), acc, ins, "\nstopped")

-- 
-- `STOP`の実行によって、メモリサイズを0に設定したので、停止状態の判定`isFinal`は以下のように定義できます。
-- 
isFinal st = case st of
    ((0,_),_,_,_) -> True
    _             -> False

-- 
-- いくつかの補助関数を定義すれば、インストラクションの定義は完成します。
-- 
next :: ToyState -> ToyState
next ((sz,mem),acc,ins,_) = ((sz, tail mem), acc, ins, "")

update :: Operand -> Data -> Memory -> Memory
update (Label m) val (sz,mem)
    = case break ((m ==) . fst) (take sz mem) of
          (xs, _:ys) -> (sz, cycle (xs ++ (m,Data val) : ys))
          _          -> error (m ++ ": not exist")

value :: Memory -> Operand -> Data
value (sz,mem) opd
    = case opd of
          Number num -> num
          Label lab  -> case dropWhile ((lab /=) . fst) (take sz mem) of
              (_,Data d):_ -> d
              _            -> error (lab ++ ": not exist")

-- 
-- ## `ToyState` の表示
-- 
-- トイ・コンピューターが動作する様子を見るために、`ToyState`を表示できるようにしましょう。表示はメモリの表示とアキュムレータの表示ができればよいでしょう。
-- 
showToyState :: ToyState -> String
showToyState ((sz, mem), acc, _, _)
    = unlines [showMemory (sz,mem), showAccumulator acc]

showMemory :: Memory -> String
showMemory (sz,mem)
    = unlines (map showCell (take sz mem))
    where
        showCell (lab, content)
            = leftJustify 8 lab ++ showContent content

leftJustify :: Int -> String -> String
leftJustify w s = take w (s ++ repeat ' ')

showContent :: Content -> String
showContent c = case c of
    Data d             -> show d
    Code (ope, opd) -> showOperator ope ++ ' ' : showOperand opd

showOperator :: Operator -> String
showOperator = show

showOperand :: Operand -> String
showOperand operand = case operand of
    None      -> ""
    Label lab -> lab
    Number n  -> show n

showAccumulator :: Accumulator -> String
showAccumulator acc = "Accumulator: " ++ show acc

-- 
-- ## プログラムローダー
-- 
-- ソースプログラムから、トイ・コンピューターの初期状態を構成できるようにしましょう。
-- 
loadProg :: String -> Memory
loadProg src = (,) . length <*> cycle . map conv $ lines $ map toUpper src

conv :: String -> (Label, Content)
conv line = case line of
    c:cs -> if isSpace c 
        then ("", toContent (words line))
        else case words line of
            label:content -> (label, toContent content)

toContent :: [String] -> Content
toContent ss = case ss of
    [cs] | all isDigit cs -> Data (read cs)
         | otherwise      -> Code (read cs, None)
    [ope,cs]              -> Code (read ope, opd)
        where
            opd = if all isDigit cs 
                then Number (read cs)
                else Label cs

-- 
-- ## ラベル参照チェッカー
-- 
-- トイ・プログラムで、存在しないラベルを参照していないかどうか、実行前にチェックしましょう。
-- 
labelCheck :: Memory -> Either Message Memory
labelCheck (sz, mem)
    = if null diffs then Right (sz, mem)
      else Left ("Not exist: " ++ unwords diffs)
    where
        (ls, cs)  = unzip (take sz mem)
        labs      = filter (not . null) ls
        rlabs     = mapMaybe exlab cs
        diffs     = map head (group (sort rlabs)) \\ labs
        exlab c    = case c of
            Code (_, Label l) -> Just l
            _                    -> Nothing

type Message = String

-- 
-- ## トイ・コンピュータ上のトイ・プログラムの起動
-- 
-- トイ・コンピュータ上でのトイ・プログラムの起動をやってみましょう。
-- 
type SourceProgram = String

run :: SourceProgram -> Channel -> [Output]
run prog input
    = case labelCheck (loadProg prog) of
          Left msg  -> [msg]
          Right mem -> filter (not . null) (map output (executeProgram (initState mem input)))
    where
        output (_,_,_,out) = out

initState :: Memory -> Channel -> ToyState
initState mem input = (mem,0,input,"")

-- 
-- ## トイ・コンピュータ上でトイ・プログラムを起動するプログラムの起動
-- 
-- トイ・プログラムをファイルから読むようにし、外界との対話できるようにしましょう。
-- 
runToy :: FilePath -> IO ()
runToy prog = readFile prog >>= interact . drive . run

drive :: ([String] -> [String]) -> (String -> String)
drive w = unlines . w . lines

-- 
-- # トイ・コンピューターの機械語
-- 
-- これまで扱ってきたコードは、アセンブリ言語だったわけですが、これを整数に置き換えると、機械語になると考えてよいでしょう。また、メモリはリストではなく、整数アドレスでインデックスされている配列であらわしましょう。
-- 
type Mem = A.Array Addr Int
type Addr = Int

-- 
-- ## オペコード、オペランド、データ
-- 
-- オペコードは、オペレータ番号で表現します。
-- 
fromOperator :: Operator -> MOpecode
fromOperator = fromEnum

toOperator :: MOpecode -> Operator
toOperator = toEnum

-- 
-- オペランドは、即値または間接参照値です。偶数で即値、奇数で間接参照値を表現しましょう。
-- 
type MOperand = Int

fromImm :: Int  -> MOperand
fromImm imm = 2 * imm

toImm :: MOperand -> Int
toImm opd = opd `div` 2

fromInd :: Addr -> MOperand
fromInd ind = 2 * ind + 1

toInd :: MOperand -> Addr
toInd opd = opd `div` 2

-- 
-- データについては、即値のみで、いわゆるポインタを使わない（使えない）仕様と解釈して、そのまま`Int`で表現します。
-- 
-- ## アセンブル
-- 
-- トイ・コンピューターのアセンブリ言語で表現したプログラムを機械語で表現したプログラムに変換するアセンブラを書きましょう。
-- 
assemble :: SourceProgram -> Mem
assemble = asm . loadProg

-- 
-- `asm :: Memory -> Mem` はアセンブリ言語プログラムの内部表現から機械語プログラムの内部表現への変換関数です。
-- 
asm :: Memory -> Mem
asm (sz, mem) = A.listArray (0, n - 1) (concat mmem)
    where
        ((n, tab), mmem) = mapAccumL psi (0, []) mem'
        mem' = take sz mem
        psi (i,syms) (lab, content) = case content of
            Code (ope, opd) -> case opd of
                None            -> ((i', syms'), [fromEnum ope])
                Number v        -> ((i'', syms'), [fromEnum ope, fromImm v])
                Label l         -> ((i'', syms'), [fromEnum ope, fromInd a])
                    where
                        a = snd (head (dropWhile ((l /=) . fst) tab))
            Data d          -> ((i', syms'), [d])
            where
                i'  = succ i
                i'' = succ i'
                syms' = case lab of
                    "" -> syms
                    _  -> (lab, i) : syms

--
-- ## メモリイメージのシリアライズとデシリアライズ
-- 
-- binary パッケージにある `Data.Binary` モジュールをつかいましょう。
--
serialize :: Mem -> ByteString
serialize = B.encode . A.elems

deserialize :: ByteString -> Mem
deserialize = fromList . B.decode

serializeFile :: FilePath -> Mem -> IO ()
serializeFile f = B.encodeFile f . toList

deserializeFile :: FilePath -> IO Mem
deserializeFile f = fromList <$> B.decodeFile f

toList :: Mem -> [Int]
toList = A.elems

fromList :: [Int] -> Mem
fromList is = A.listArray (0, pred (length is)) is
-- 
-- ## トイ・マシンの状態
-- 
-- あらためて、このトイ・マシンの状態を構成しましょう。メモリ`Mem`が含まれているはずですね。それから、アキュムレータ`AC`と、プログラムカウンター`PC`という2つのレジスタがあるでしょう。また、入力チャンネル`Chan`、出力`Out`があるはずです。
-- 
type AC = Register
type PC = Register
type Register = Int
type Chan = [Int]
type Out  = Int
none :: Out
none = minBound
type MState = (Mem, AC, PC, Chan, Out)

mIsFinal :: MState -> Bool
mIsFinal (_,_,pc,_,_) = pc < 0

mOut :: MState -> Out
mOut (_,_,_,_,out) = out

-- 
-- 状態が定義できたら、機械語のプログラムを動作させることを考えます。といっても、アセンブリ言語のプログラムでやったのと同じことをするだけです。
-- 
-- ### フェッチ-デコード-実行サイクル
-- 
-- フェッチ—デコード-実行のサイクルを考えましょう。
-- 
-- まず`mFetch`はプログラムカウンターが指す位置のオペコードを取得します。
-- 
type MOpecode = Int
type MInstruction = MState -> MState

mFetch   :: MState -> MOpecode
mFetch (mem,_,pc,_,_) = mem A.! pc

-- 
-- 次にデコード`mDecode`はディスパッチですね。
-- 
mDecode :: MOpecode -> MInstruction
mDecode ope (mem,ac,pc,ins,_) = case toEnum ope of
    GET    -> (mem, head ins, pc + 1, tail ins, none)
    PRINT  -> (mem, ac, pc + 1, ins, ac)
    STOP   -> (mem, ac, -1, ins, none)
    LOAD   -> (mem, getval mem (pc + 1), pc + 2, ins, none)
    STORE  -> (mem A.// [(addr (mem A.! (pc + 1)), ac)], ac, pc + 2, ins, none)
    ADD    -> (mem, ac + getval mem (pc + 1), pc + 2, ins, none)
    SUB    -> (mem, ac - getval mem (pc + 1), pc + 2, ins, none)
    GOTO   -> (mem, ac, pc', ins, none)
                where
                    pc' = addr (mem A.! (pc + 1))
    IFPOS  -> (mem, ac, pc', ins, none)
                where
                    pc' = if ac >= 0 then addr (mem A.! (pc + 1)) else pc + 2
    IFZERO -> (mem, ac, pc', ins, none)
                where
                    pc' = if ac == 0 then addr (mem A.! (pc + 1)) else pc + 2

-- 
-- 補助関数`getval :: Mem -> Addr -> Int`はメモリの指定したアドレスにあるデータ（オペランドが示す値）です。
-- 
getval :: Mem -> Addr -> Int
getval mem a = case mem A.! a of
    opd -> if even opd then val opd
           else mem A.! addr opd

addr :: MOperand -> Addr
addr = toInd

val :: MOperand -> Int
val = toImm

-- 
-- 最後に実行は、デコードで得られたインストラクションである状態遷移関数の適用ですので、アセンブリ言語のときと同じで`id`でいいでしょう。
-- 
mExecute :: MInstruction -> MState -> MState
mExecute = id

-- 
-- 1つのインストラクションを実行する過程（つまり、フェッチ-デコード-実行の1サイクル分）を`mStep`で表現しましょう。
-- 
mStep    :: MState -> MState
mStep st = mExecute (mDecode (mFetch st)) st

-- 
-- ## 機械語プログラムの実行
-- 
-- 実行に関してもアセンブリ言語のときと同じですね。
-- 
mExecuteProgram :: MState -> [MState]
mExecuteProgram state = state : rests
    where
        rests | mIsFinal state = []
              | otherwise      = mExecuteProgram (mStep state)

-- 
-- ## 機械語プログラムの起動
-- 
-- 機械語プログラムの起動
-- 
mRun :: Mem -> Chan -> [Out]
mRun mem chan = foldr phi [] (mExecuteProgram (mem, 0, 0, chan, none))
    where
        phi (_,_,_,_,o) ys = if minBound == o then ys else o : ys
