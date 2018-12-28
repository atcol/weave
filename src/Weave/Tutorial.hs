{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| An API & DSL for event generation.

-}
module Weave.Tutorial
  (
    -- * Introduction
    -- $introduction

    -- * Copyright
    -- $copyright
  )

where

{- $introduction
  Chaos is an API & DSL for generating events using time & spatial parameters.
  This allows one to integrate actions seamlessly while declaring their source of
  invocation in a human-readable format.

  For example, let's say I wanted to @apt-update@ once:

  @in 30 seconds { apt-get update }@

  or just do so repetedly

  @every 6 hours { apt-get update }@
-}


{- $copyright
  Copyright 2017, 2018 - Alex Collins.

  This tutorial is licensed under a
  <http://creativecommons.org/licenses/by/4.0/ Creative Commons Attribution 4.0 International License>
-}
