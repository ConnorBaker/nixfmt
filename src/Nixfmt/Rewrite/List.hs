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

import Data.Function (on)
import Nixfmt.Pretty ()
import Nixfmt.Types (
  Ann (..),
  Expression (..),
  Item (..),
  Items (..),
  Selector (..),
  SimpleSelector (..),
  Term (..),
  Token (..),
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

rewriteListConcatenation :: Expression -> Expression
rewriteListConcatenation = rewriteExpression

rewriteTerm :: Term -> Term
rewriteTerm = \case
  List open items close -> List open (rewriteTerm <$> items) close
  -- TODO: Set
  -- TODO: Selection
  Parenthesized open expr close -> Parenthesized open (rewriteExpression expr) close
  other -> other

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

rewriteExpression :: Expression -> Expression
rewriteExpression = \case
  Term term -> Term (rewriteTerm term)
  With with expr0 semicolon expr1 ->
    With with (rewriteExpression expr0) semicolon (rewriteExpression expr1)
  Let let_ items in_ body ->
    -- TODO: Handle items
    Let let_ items in_ (rewriteExpression body)
  assertion@(Assert{}) -> assertion -- Don't rewrite asserts.
  If if_ expr0 then_ expr1 else_ expr2 ->
    If if_ (rewriteExpression expr0) then_ (rewriteExpression expr1) else_ (rewriteExpression expr2)
  Abstraction param colon body ->
    -- TODO: Ignoring param, though there could be rewrites applied to defaults
    Abstraction param colon (rewriteExpression body)
  Application g a -> Application (rewriteExpression g) (rewriteExpression a)
  Operation left op right -> case op of
    Ann{value = TConcat} ->
      -- Concatenation is right-associative.
      -- Need to fold the left and right sides of the concatenation into a single expression using concatLists.
      case (left, right) of
        -- Identity
        (Term (List _ (Items []) _), _) -> rewriteExpression right
        (_, Term (List _ (Items []) _)) -> rewriteExpression left
        -- Simplification of known lists
        (Term (List openLeft (Items itemsLeft) closeLeft), Term (List _openRight (Items itemsRight) _closeRight)) ->
          Term (List openLeft (rewriteTerm <$> Items (itemsLeft <> itemsRight)) closeLeft)
        _ -> mkConcatListsApplication $ ((<>) `on` gatherConcatenationExpressions) left right
    _ -> Operation (rewriteExpression left) op (rewriteExpression right)
  MemberCheck name dot sels ->
    -- TODO: Does not handle potential rewrites in selectors
    MemberCheck (rewriteExpression name) dot sels
  Negation not_ expr -> Negation not_ (rewriteExpression expr)
  Inversion tilde expr -> Inversion tilde (rewriteExpression expr)
