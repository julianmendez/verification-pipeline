package soda.tiles.verifier.tile.primitive

/*
 * This package contains tests for the primitive tiles.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.verifier.entity.Action
import   soda.tiles.verifier.entity.FluentValue
import   soda.tiles.verifier.entity.IdentifierSet
import   soda.tiles.verifier.entity.IfRule
import   soda.tiles.verifier.entity.InhibitsRule
import   soda.tiles.verifier.entity.NoConcurrencyRule
import   soda.tiles.verifier.entity.Rule
import   soda.tiles.verifier.entity.TileMessage
import   soda.tiles.verifier.entity.TileMessageBuilder
import   soda.tiles.verifier.entity.TilePair





trait SquareApplyTile
  extends
    ApplyTile [Int, Int]
{



  lazy val phi : Int => Int =
     elem => elem * elem

}

case class SquareApplyTile_ () extends SquareApplyTile

object SquareApplyTile {
  def mk : SquareApplyTile =
    SquareApplyTile_ ()
}

case class ApplyTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val scenario = ScenarioExample .mk

  def mk_tile_message (value : Int) : TileMessage [Int] =
    TileMessageBuilder
      .mk
      .build (scenario .context_0) (scenario .instance_0) (value)

  test ("apply on single value") (
    check(
      obtained = SquareApplyTile .mk
        .apply (mk_tile_message (5) )
        .contents
    ) (
      expected = 25
    )
  )

  test ("apply on zero") (
    check(
      obtained = SquareApplyTile .mk
        .apply (mk_tile_message (0) )
        .contents
    ) (
      expected = 0
    )
  )

  test ("apply on negative value") (
    check(
      obtained = SquareApplyTile .mk
        .apply (mk_tile_message (-3) )
        .contents
    ) (
      expected = 9
    )
  )

}


case class CrossTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained: A) (expected: A): org.scalatest.compatible.Assertion =
    assert(obtained == expected)

  lazy val scenario = ScenarioExample .mk

  def mk_tile_message [A ] (seq: Seq [A] ) : TileMessage [Seq [A] ] =
    TileMessageBuilder
      .mk
      .build (scenario.context_0) (scenario.instance_0) (seq)

  test ("cross on two empty sequences returns empty sequence") (
    check (
      obtained = CrossTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] () ) ) (mk_tile_message (Seq [String] () ) )
        .contents
    ) (
      expected = Seq [TilePair [Int, String] ] ()
    )
  )

  test ("cross on empty first sequence returns empty sequence") (
    check (
      obtained = CrossTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] () ) ) (mk_tile_message (Seq [String] ("a" , "b") ) )
        .contents
    ) (
      expected = Seq [TilePair [Int, String] ] ()
    )
  )

  test ("cross on empty second sequence returns empty sequence") (
    check (
      obtained = CrossTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1 , 2) ) ) (mk_tile_message (Seq [String] () ) )
        .contents
    ) (
      expected = Seq [TilePair [Int, String] ] ()
    )
  )

  test ("cross on single-element sequences") (
    check (
      obtained = CrossTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1) ) ) (mk_tile_message (Seq [String] ("a") ) )
        .contents
    ) (
      expected = Seq (TilePair .mk (1) ("a") )
    )
  )

  test ("cross on multi-element sequences") (
    check (
      obtained = CrossTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1 , 2) ) ) (mk_tile_message (Seq [String] ("a" , "b") ) )
        .contents
    ) (
      expected = Seq(
        TilePair .mk (1) ("a") ,
        TilePair .mk (1) ("b") ,
        TilePair .mk (2) ("a") ,
        TilePair .mk (2) ("b")
      )
    )
  )

}


trait SumFoldTile
  extends
    FoldTile [Int, Int]
{



  lazy val z : Int = 0

  lazy val phi : Int => Int => Int =
     acc =>
       elem =>
        acc + elem

}

case class SumFoldTile_ () extends SumFoldTile

object SumFoldTile {
  def mk : SumFoldTile =
    SumFoldTile_ ()
}

case class FoldTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val scenario = ScenarioExample .mk

  def mk_tile_message (seq : Seq [Int] ) : TileMessage [Seq [Int] ] =
    TileMessageBuilder
      .mk
      .build (scenario .context_0) (scenario .instance_0) (seq)

  test ("fold on empty sequence returns initial value") (
    check(
      obtained = SumFoldTile .mk
        .apply (mk_tile_message (Seq [Int] () ) )
        .contents
    ) (
      expected = 0
    )
  )

  test ("fold on single element sequence") (
    check(
      obtained = SumFoldTile .mk
        .apply (mk_tile_message (Seq [Int] (5) ) )
        .contents
    ) (
      expected = 5
    )
  )

  test ("fold on multiple elements sequence") (
    check(
      obtained = SumFoldTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3 , 4) ) )
        .contents
    ) (
      expected = 10
    )
  )

}


trait ScenarioExample
{



  lazy val fluent_value_0 = "fluent0"

  lazy val fluent_value_1 = "fluent1"

  lazy val fluent_value_2 = "fluent2"

  lazy val fluent_value_3 = "fluent3"

  lazy val fluent_0 = fluent_value_0

  lazy val fluent_1 = fluent_value_1

  lazy val fluent_2 = fluent_value_2

  lazy val fluent_3 = fluent_value_3

  lazy val action_0 = "action0"

  lazy val action_1 = "action1"

  lazy val action_2 = "action2"

  lazy val action_3 = "action3"

  lazy val action_set_0_1 = (Seq [Action] (action_0 , action_1) ) .toSet

  lazy val action_set_1_2 = (Seq [Action] (action_1 , action_2) ) .toSet

  lazy val fluent_set_0 = (Seq [FluentValue] (fluent_0) ) .toSet

  lazy val fluent_set_1 = (Seq [FluentValue] (fluent_1) ) .toSet

  lazy val fluent_set_2 = (Seq [FluentValue] (fluent_2) ) .toSet

  lazy val fluent_set_0_1 = (Seq [FluentValue] (fluent_0 , fluent_1) ) .toSet

  lazy val if_rule_0 = IfRule (fluent_set_0 , fluent_set_0_1)

  lazy val inh_rule_0 = InhibitsRule (fluent_set_2 , action_2)

  lazy val nc_rule_0 = NoConcurrencyRule (action_set_0_1)

  lazy val nc_rule_1 = NoConcurrencyRule (action_set_1_2)

  lazy val context_0 = Seq [Rule] (if_rule_0 , inh_rule_0 , nc_rule_0 , nc_rule_1)

  lazy val instance_0 = Seq [IdentifierSet] (fluent_set_0_1 , action_set_0_1 , fluent_set_0_1 , action_set_1_2 , fluent_set_0_1)

}

case class ScenarioExample_ () extends ScenarioExample

object ScenarioExample {
  def mk : ScenarioExample =
    ScenarioExample_ ()
}


case class ZipTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val scenario = ScenarioExample .mk

  def mk_tile_message [T ] (seq : Seq [T] ) : TileMessage [Seq [T] ] =
    TileMessageBuilder
      .mk
      .build (scenario .context_0) (scenario .instance_0) (seq)

  test ("zip on two empty sequences returns empty sequence") (
    check (
      obtained = ZipTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] () ) ) (mk_tile_message (Seq [String] () ) )
        .contents
    ) (
      expected = Seq [TilePair [Int, String] ] ()
    )
  )

  test ("zip on sequences of equal length") (
    check (
      obtained = ZipTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3) ) ) (mk_tile_message (Seq [String] ("a" , "b" , "c") ) )
        .contents
    ) (
      expected = Seq (
        TilePair .mk [Int, String] (1) ("a") ,
        TilePair .mk [Int, String] (2) ("b") ,
        TilePair .mk [Int, String] (3) ("c")
      )
    )
  )

  test ("zip truncates to the shorter sequence (first shorter)") (
    check (
      obtained = ZipTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1 , 2) ) ) (mk_tile_message (Seq [String] ("a" , "b" , "c") ) )
        .contents
    ) (
      expected = Seq (
        TilePair .mk [Int, String] (1) ("a") ,
        TilePair .mk [Int, String] (2) ("b")
      )
    )
  )

  test ("zip truncates to the shorter sequence (second shorter)") (
    check (
      obtained = ZipTile .mk [Int, String]
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3) ) ) (mk_tile_message (Seq [String] ("a") ) )
        .contents
    ) (
      expected = Seq (
        TilePair .mk [Int, String] (1) ("a")
      )
    )
  )

}

