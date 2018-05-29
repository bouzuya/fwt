module Test.Main (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Math as Math
import Prelude (Unit, discard, ($), (==))
import Size (getColumns, getSize)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main
  :: forall e
  . Eff
      ( avar :: AVAR
      , testOutput :: TESTOUTPUT
      , console :: CONSOLE
      | e
      )
      Unit
main = do
  log "You should add some tests."
  runTest do
    suite "1" do
      test "1" do
        Assert.assert "1 == 1" $ 1 == 1
    test "10x10" do
      let rect = { w: 10, h: 10 }
      Assert.equal 0 (getColumns 0 rect)
      Assert.equal 1 (getColumns 1 rect)
      Assert.equal 2 (getColumns 2 rect)
      Assert.equal 2 (getColumns 3 rect)
      Assert.equal 2 (getColumns 4 rect)
      Assert.equal 3 (getColumns 5 rect)
      Assert.equal 3 (getColumns 6 rect)
      Assert.equal 3 (getColumns 9 rect)
      Assert.equal 4 (getColumns 10 rect)
      Assert.equal 10 (getColumns 100 rect)
      Assert.equal 0.0 (getSize (getColumns 0 rect) rect)
      Assert.equal 10.0 (getSize (getColumns 1 rect) rect)
      Assert.equal 5.0 (getSize (getColumns 2 rect) rect)
      Assert.equal 5.0 (getSize (getColumns 3 rect) rect)
      Assert.equal 5.0 (getSize (getColumns 4 rect) rect)
      Assert.equal 3.0 (Math.floor (getSize (getColumns 5 rect) rect)) -- 3.3...
      Assert.equal 3.0 (Math.floor (getSize (getColumns 6 rect) rect)) -- 3.3...
      Assert.equal 3.0 (Math.floor (getSize (getColumns 9 rect) rect)) -- 3.3...
      Assert.equal 2.5 (getSize (getColumns 10 rect) rect)
      Assert.equal 1.0 (getSize (getColumns 100 rect) rect)
    suite "5x10" do
      let
        rect = { w: 5, h: 10 }
      test "1" do
        Assert.equal 1 (getColumns 1 rect)
        Assert.equal 1 (getColumns 2 rect)
        Assert.equal 5.0 (getSize (getColumns 1 rect) rect)
        Assert.equal 5.0 (getSize (getColumns 2 rect) rect)
      test "3-8" do
        Assert.equal 2 (getColumns 3 rect)
        Assert.equal 2 (getColumns 8 rect)
        Assert.equal 2.5 (getSize (getColumns 3 rect) rect)
        Assert.equal 2.5 (getSize (getColumns 8 rect) rect)
      test "9-18" do
        Assert.equal 3 (getColumns 9 rect)
        Assert.equal 3 (getColumns 18 rect)
        Assert.equal 1.0 (Math.floor (getSize (getColumns 9 rect) rect)) -- 1.6...
        Assert.equal 1.0 (Math.floor (getSize (getColumns 18 rect) rect)) -- 1.6...
