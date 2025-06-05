{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Nixfmt.Rewrite (
  doRewrites,
)
where

import Nixfmt.Pretty ()
import Nixfmt.Rewrite.List (rewriteListConcatenation)
import Nixfmt.Types (Expression)

-- General problems:
-- 1. Not sure how best to merge or preseve annotations -- source line under rewrites is lost.
-- 2. Have not read current (or even old) state of the art on how to best architect rewriting systems.
-- 3. Rewrites in this fashion are fragile; for example, using parentheses will prevent rewrites as I've implemented them.

-- | Apply rewrites to an expression.
-- Should be done in a fixed-point so that rewrites can be applied multiple times.
doRewrites :: Expression -> Expression
doRewrites = flip (foldl' (flip id)) (take 100 $ repeat rewriteListConcatenation)