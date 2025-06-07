{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Nixfmt.Rewrite.List (
  rewriteListConcatenation,
)
where

import Data.Bifunctor (bimap)
import Data.Function (on)
import Nixfmt.Pretty ()
import Nixfmt.Types (
  Ann (..),
  Binder (..),
  Expression (..),
  Item (..),
  Items (..),
  ParamAttr (..),
  Parameter (..),
  Selector (..),
  SimpleSelector (..),
  StringPart (..),
  Term (..),
  Token (..),
  Whole (..),
  ann,
 )
import Text.Megaparsec (pos1)

builtinsConcatLists :: Expression
builtinsConcatLists =
  Term
    ( Selection
        (Token $ ann pos1 (Identifier "builtins"))
        [Selector (Just $ ann pos1 TDot) (IDSelector $ ann pos1 (Identifier "concatLists"))]
        Nothing
    )

mkParenthesized :: Expression -> Term
mkParenthesized expr =
  Parenthesized
    (ann pos1 TParenOpen)
    expr
    (ann pos1 TParenClose)

mkList :: [Expression] -> Term
mkList exprs =
  List
    (ann pos1 TBrackOpen)
    (Items $ (Item . mkParenthesized) <$> exprs)
    (ann pos1 TBrackClose)

mkConcatListsApplication :: [Expression] -> Expression
mkConcatListsApplication exprs =
  Application
    builtinsConcatLists
    (Term $ mkList exprs)

gatherConcatenationExpressions :: Expression -> [Expression]
gatherConcatenationExpressions = \case
  Operation left Ann{value = TConcat} right ->
    ((<>) `on` gatherConcatenationExpressions) left right
  expr -> [expr]

rewriteListConcatenation :: Expression -> Expression
rewriteListConcatenation = rewriteExpression

rewriteStringPart :: StringPart -> StringPart
rewriteStringPart = \case
  Interpolation (Whole expr trivia) -> Interpolation (Whole (rewriteExpression expr) trivia)
  sp -> sp -- No rewrite for other expressions

rewritePath :: Ann [StringPart] -> Ann [StringPart]
rewritePath a = a{value = rewriteStringPart <$> (value a)}

rewriteString :: Ann [[StringPart]] -> Ann [[StringPart]]
rewriteString a = a{value = fmap rewriteStringPart <$> (value a)}

rewriteSimpleSelector :: SimpleSelector -> SimpleSelector
rewriteSimpleSelector = \case
  InterpolSelector stringPart -> InterpolSelector stringPart{value = rewriteStringPart (value stringPart)}
  StringSelector string -> StringSelector (rewriteString string)
  ss -> ss -- No rewrite for other selectors

rewriteSelector :: Selector -> Selector
rewriteSelector (Selector maybeDot ident) = Selector maybeDot (rewriteSimpleSelector ident)

rewriteParamAttr :: ParamAttr -> ParamAttr
rewriteParamAttr = \case
  ParamAttr name maybeDefault maybeComma ->
    ParamAttr name (bimap id rewriteExpression <$> maybeDefault) maybeComma
  paramAttr -> paramAttr -- No rewrite for other attributes

rewriteParameter :: Parameter -> Parameter
rewriteParameter = \case
  SetParameter open paramAttrs close -> SetParameter open (rewriteParamAttr <$> paramAttrs) close
  ContextParameter first at second -> ContextParameter (rewriteParameter first) at (rewriteParameter second)
  param -> param -- No rewrite for other parameters

rewriteBinder :: Binder -> Binder
rewriteBinder = \case
  Inherit inherit term sels semicolon ->
    Inherit inherit (rewriteTerm <$> term) (rewriteSimpleSelector <$> sels) semicolon
  Assignment sels eq rhs semicolon ->
    Assignment (rewriteSelector <$> sels) eq (rewriteExpression rhs) semicolon

rewriteTerm :: Term -> Term
rewriteTerm = \case
  SimpleString string -> SimpleString (rewriteString string)
  IndentedString string -> IndentedString (rewriteString string)
  Path path -> Path (rewritePath path)
  List open items close -> List open (rewriteTerm <$> items) close
  Set rec open items close -> Set rec open (rewriteBinder <$> items) close
  Selection term selector def -> Selection (rewriteTerm term) (rewriteSelector <$> selector) (bimap id rewriteTerm <$> def)
  Parenthesized open expr close -> Parenthesized open (rewriteExpression expr) close
  other -> other

rewriteConcat :: Expression -> Expression -> Expression
rewriteConcat left right = case (left, right) of
  -- Identity
  (Term (List _ (Items []) _), _) -> rewriteExpression right
  (_, Term (List _ (Items []) _)) -> rewriteExpression left
  -- Simplification of known lists
  (Term (List openLeft (Items itemsLeft) closeLeft), Term (List _openRight (Items itemsRight) _closeRight)) ->
    Term (List openLeft (rewriteTerm <$> Items (itemsLeft <> itemsRight)) closeLeft)
  -- Concatenation of lists
  _ -> mkConcatListsApplication $ ((<>) `on` gatherConcatenationExpressions) left right

-- TODO: Need to rewrite StringPart as well.
rewriteExpression :: Expression -> Expression
rewriteExpression = \case
  Term term -> Term (rewriteTerm term)
  With with expr0 semicolon expr1 ->
    With with (rewriteExpression expr0) semicolon (rewriteExpression expr1)
  Let let_ items in_ body -> Let let_ (rewriteBinder <$> items) in_ (rewriteExpression body)
  assertion@(Assert{}) -> assertion -- Don't rewrite asserts.
  If if_ expr0 then_ expr1 else_ expr2 ->
    If if_ (rewriteExpression expr0) then_ (rewriteExpression expr1) else_ (rewriteExpression expr2)
  Abstraction param colon body -> Abstraction (rewriteParameter param) colon (rewriteExpression body)
  Application g a -> Application (rewriteExpression g) (rewriteExpression a)
  Operation left op right -> case op of
    Ann{value = TConcat} -> rewriteConcat left right
    _ -> Operation (rewriteExpression left) op (rewriteExpression right)
  MemberCheck name dot sels -> MemberCheck (rewriteExpression name) dot (rewriteSelector <$> sels)
  Negation not_ expr -> Negation not_ (rewriteExpression expr)
  Inversion tilde expr -> Inversion tilde (rewriteExpression expr)
