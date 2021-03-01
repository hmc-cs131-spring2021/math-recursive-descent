{-|
Module       : MathParser
Description  : Parser for the Math language

Contributes a parser for the following grammar:

   expr ::= term + expr
         |  term - expr
         |  term

   term ::= factor * term
         |  factor / term
         |  factor
         
   factor ::= n
           | (expr)
-}

module MathParser where

import MathTokenizer
import MathAST

parse :: String -> Exp
parse s = 
  case expr (tokenize s) of
    -- a successful parse that consumes all the tokens succeeds
    (e, []) -> e

    -- a successful parse with leftover tokens fails
    (_, extraTokens) -> error ("Unexpected input: " ++ show extraTokens) False


-- | A parser function takes a list of tokens and returns a pair:
--   the result of parsing and the unconsumed tokens
type ParserFunction = [Token] -> (Exp, [Token])


expr :: ParserFunction
expr tokens = 
  case term tokens of

    -- term + expr
    (left, CrossToken : tokens') -> 
      let (right, tokens'') = expr tokens' in 
        (BinOp left PlusOp right, tokens'')

    -- term - expr
    (left, DashToken : tokens') -> 
      let (right, tokens'') = expr tokens' in 
        (BinOp left MinusOp right, tokens'')

    -- term
    result -> result


term :: ParserFunction
term tokens = 
  case factor tokens of

    -- factor * term
    (left, StarToken : tokens') -> 
      let (right, tokens'') = term tokens' in 
        (BinOp left TimesOp right, tokens'')

    -- factor / term
    (left, SlashToken : tokens') -> 
      let (right, tokens'') = term tokens' in 
        (BinOp left DivOp right, tokens'')

    -- factor
    result -> result


factor :: ParserFunction
-- n 
factor ( (NumberToken n) : tokens') = (Num n, tokens')

-- (expr)
factor (LParenToken : tokens') = 
  case expr tokens' of
    (e, RParenToken : tokens'') -> (e, tokens'')
    _ -> error "Expected ')'"

-- Everything else fails
factor _ = error "Expected number or '('"
