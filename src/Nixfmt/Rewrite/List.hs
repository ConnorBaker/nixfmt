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

import Control.Monad.Fix (fix)
import Nixfmt.Pretty ()
import Nixfmt.Types (
  Ann (..),
  Expression (..),
  Item (..),
  Items (..),
  Term (..),
  Token (..),
 )

rewriteListConcatenation :: Expression -> Expression
rewriteListConcatenation = rewriteExpression

rewriteTerm :: Term -> Term
rewriteTerm = \case
  (List open items close) ->
    List
      open
      -- TODO: Why was I unable to write this using just fmap?
      ( Items
          ( map
              ( \case
                  (Item term) -> Item (rewriteTerm term)
                  other -> other
              )
              (unItems items)
          )
      )
      close
  -- TODO: Set
  -- TODO: Selection
  (Parenthesized open expr close) -> Parenthesized open (rewriteExpression expr) close
  other -> other

rewriteExpression :: Expression -> Expression
rewriteExpression = \case
  (Term term) -> Term (rewriteTerm term)
  (With with expr0 semicolon expr1) ->
    With with (rewriteExpression expr0) semicolon (rewriteExpression expr1)
  (Let let_ items in_ body) ->
    -- TODO: Handle items
    Let let_ items in_ (rewriteExpression body)
  assertion@(Assert{}) -> assertion -- Don't rewrite asserts.
  (If if_ expr0 then_ expr1 else_ expr2) ->
    If if_ (rewriteExpression expr0) then_ (rewriteExpression expr1) else_ (rewriteExpression expr2)
  (Abstraction param colon body) ->
    -- TODO: Ignoring param, though there could be rewrites applied to defaults
    Abstraction param colon (rewriteExpression body)
  (Application g a) -> Application (rewriteExpression g) (rewriteExpression a)
  (Operation left op right) -> case op of
    Ann{value = TConcat} ->
      case (left, right) of
        -- Rewrite concatenation of lists with the empty list
        (Term (List _ (Items []) _), _) -> right
        (_, Term (List _ (Items []) _)) -> left
        -- Rewrite concatenation of lists of values as a single list
        (Term (List openLeft (Items itemsLeft) closeLeft), Term (List _openRight (Items itemsRight) _closeRight)) ->
          Term (List openLeft (Items (itemsLeft <> itemsRight)) closeLeft)
        -- TODO: Rewrite concatenation of lists of values and values with builtins.concatLists
        -- Otherwise, rewrite both sides
        _ -> Operation (rewriteExpression left) op (rewriteExpression right)
    _ -> Operation (rewriteExpression left) op (rewriteExpression right)
  (MemberCheck name dot sels) ->
    -- TODO: Does not handle potential rewrites in selectors
    MemberCheck (rewriteExpression name) dot sels
  (Negation not_ expr) -> Negation not_ (rewriteExpression expr)
  (Inversion tilde expr) -> Inversion tilde (rewriteExpression expr)
