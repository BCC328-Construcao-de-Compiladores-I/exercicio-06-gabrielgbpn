{-# OPTIONS_GHC -w #-}
module L.L2.Frontend.LALR where

import L.L2.Frontend.Lexer (Token(..), Lexeme(..))
import L.L2.Frontend.Syntax
import Utils.Var (Var(..))
import Utils.Value (Value(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,92) ([18432,3072,2304,384,0,0,36,6,0,0,0,0,0,0,0,512,0,1,0,256,0,32,0,0,32880,0,4110,0,16,14336,64,0,0,61440,4,0,0,0,0,0,3584,16,448,2,15360,2,6016,57344,256,7168,32,896,4,32880,0,16384,16384,0,32768,60,32768,23,0,0,0,32768,49156,0,256,0,0,0,0,0,0,1536,0,192,0,128,4096,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseL2","L2","Statements","Statement","Def","LAssign","LRead","LPrint","Expr","IDENT","NUMBER","STRING","DEF","IN","END","ASSIGN","PLUS","MINUS","TIMES","DIV","LPAREN","RPAREN","SEMI","COMMA","READ","PRINT","%eof"]
        bit_start = st Prelude.* 29
        bit_end = (st Prelude.+ 1) Prelude.* 29
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..28]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (12) = happyShift action_8
action_0 (15) = happyShift action_9
action_0 (27) = happyShift action_10
action_0 (28) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 _ = happyReduce_3

action_1 (12) = happyShift action_8
action_1 (15) = happyShift action_9
action_1 (27) = happyShift action_10
action_1 (28) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (12) = happyShift action_8
action_3 (15) = happyShift action_9
action_3 (27) = happyShift action_10
action_3 (28) = happyShift action_11
action_3 (5) = happyGoto action_17
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 (9) = happyGoto action_6
action_3 (10) = happyGoto action_7
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (18) = happyShift action_16
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (12) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_14
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (23) = happyShift action_13
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (29) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (12) = happyShift action_19
action_13 (13) = happyShift action_20
action_13 (14) = happyShift action_21
action_13 (23) = happyShift action_22
action_13 (11) = happyGoto action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (12) = happyShift action_19
action_14 (13) = happyShift action_20
action_14 (14) = happyShift action_21
action_14 (23) = happyShift action_22
action_14 (11) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_23
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (12) = happyShift action_19
action_16 (13) = happyShift action_20
action_16 (14) = happyShift action_21
action_16 (23) = happyShift action_22
action_16 (11) = happyGoto action_18
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_2

action_18 (19) = happyShift action_26
action_18 (20) = happyShift action_27
action_18 (21) = happyShift action_28
action_18 (22) = happyShift action_29
action_18 (25) = happyShift action_34
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_19

action_20 _ = happyReduce_17

action_21 _ = happyReduce_18

action_22 (12) = happyShift action_19
action_22 (13) = happyShift action_20
action_22 (14) = happyShift action_21
action_22 (23) = happyShift action_22
action_22 (11) = happyGoto action_33
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (12) = happyShift action_19
action_23 (13) = happyShift action_20
action_23 (14) = happyShift action_21
action_23 (23) = happyShift action_22
action_23 (11) = happyGoto action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (19) = happyShift action_26
action_24 (20) = happyShift action_27
action_24 (21) = happyShift action_28
action_24 (22) = happyShift action_29
action_24 (26) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (19) = happyShift action_26
action_25 (20) = happyShift action_27
action_25 (21) = happyShift action_28
action_25 (22) = happyShift action_29
action_25 (24) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (12) = happyShift action_19
action_26 (13) = happyShift action_20
action_26 (14) = happyShift action_21
action_26 (23) = happyShift action_22
action_26 (11) = happyGoto action_42
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (12) = happyShift action_19
action_27 (13) = happyShift action_20
action_27 (14) = happyShift action_21
action_27 (23) = happyShift action_22
action_27 (11) = happyGoto action_41
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (12) = happyShift action_19
action_28 (13) = happyShift action_20
action_28 (14) = happyShift action_21
action_28 (23) = happyShift action_22
action_28 (11) = happyGoto action_40
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_19
action_29 (13) = happyShift action_20
action_29 (14) = happyShift action_21
action_29 (23) = happyShift action_22
action_29 (11) = happyGoto action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (25) = happyShift action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (12) = happyShift action_37
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (16) = happyShift action_36
action_32 (19) = happyShift action_26
action_32 (20) = happyShift action_27
action_32 (21) = happyShift action_28
action_32 (22) = happyShift action_29
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (19) = happyShift action_26
action_33 (20) = happyShift action_27
action_33 (21) = happyShift action_28
action_33 (22) = happyShift action_29
action_33 (24) = happyShift action_35
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_9

action_35 _ = happyReduce_16

action_36 (12) = happyShift action_8
action_36 (15) = happyShift action_9
action_36 (27) = happyShift action_10
action_36 (28) = happyShift action_11
action_36 (5) = happyGoto action_44
action_36 (6) = happyGoto action_3
action_36 (7) = happyGoto action_4
action_36 (8) = happyGoto action_5
action_36 (9) = happyGoto action_6
action_36 (10) = happyGoto action_7
action_36 _ = happyReduce_3

action_37 (24) = happyShift action_43
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_11

action_39 _ = happyReduce_15

action_40 _ = happyReduce_14

action_41 (21) = happyShift action_28
action_41 (22) = happyShift action_29
action_41 _ = happyReduce_13

action_42 (21) = happyShift action_28
action_42 (22) = happyShift action_29
action_42 _ = happyReduce_12

action_43 (25) = happyShift action_46
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (17) = happyShift action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_8

action_46 _ = happyReduce_10

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (L2 happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 7 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def (Var happy_var_2) happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LAssign (Var happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 7 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TIdent happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (LRead happy_var_3 (Var happy_var_5)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 5 10 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (LPrint happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (LAdd happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (LMinus happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (LMul happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (LDiv happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn11
		 (LVal (VInt happy_var_1)
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn11
		 (LVal (VStr happy_var_1)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn11
		 (LVar (Var happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Token _ (TIdent happy_dollar_dollar) -> cont 12;
	Token _ (TNumber happy_dollar_dollar) -> cont 13;
	Token _ (TString happy_dollar_dollar) -> cont 14;
	Token _ TDef -> cont 15;
	Token _ TIn -> cont 16;
	Token _ TEnd -> cont 17;
	Token _ TAssign -> cont 18;
	Token _ TPlus -> cont 19;
	Token _ TMinus -> cont 20;
	Token _ TTimes -> cont 21;
	Token _ TDiv -> cont 22;
	Token _ TLParen -> cont 23;
	Token _ TRParen -> cont 24;
	Token _ TSemicolon -> cont 25;
	Token _ TComma -> cont 26;
	Token _ TRead -> cont 27;
	Token _ TPrint -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parseL2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError [] = Left "Parse error: unexpected end of input"
parseError ((Token (line, col) lexeme) : _) = 
  Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": unexpected " ++ show lexeme
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
