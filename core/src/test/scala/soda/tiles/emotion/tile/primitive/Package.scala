package soda.tiles.emotion.tile.primitive

/*
 * This package contains tests for the primitive tiles.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.ActionSetType
import   soda.tiles.emotion.entity.AllowsRule
import   soda.tiles.emotion.entity.CausesIfRule
import   soda.tiles.emotion.entity.Context
import   soda.tiles.emotion.entity.ContravenesRule
import   soda.tiles.emotion.entity.DefaultRule
import   soda.tiles.emotion.entity.FacilitatesRule
import   soda.tiles.emotion.entity.Fluent
import   soda.tiles.emotion.entity.FluentOrActionSet
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.FluentSetType
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.Instance
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileMessageBuilder
import   soda.tiles.emotion.entity.TilePair
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.TriggersRule

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


case class DistinctTileSpec ()
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

  test ("distinct on empty sequence returns empty sequence") (
    check(
      obtained = DistinctTile .mk
        .apply (mk_tile_message (Seq [Int] () ) )
        .contents
    ) (
      expected = Seq [Int] ()
    )
  )

  test ("distinct on sequence with no duplicates") (
    check(
      obtained = DistinctTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3) ) )
        .contents
    ) (
      expected = Seq [Int] (1 , 2 , 3)
    )
  )

  test ("distinct on sequence with some duplicates") (
    check(
      obtained = DistinctTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 2 , 3 , 1 , 4) ) )
        .contents
    ) (
      expected = Seq [Int] (1 , 2 , 3 , 4)
    )
  )

  test ("distinct on sequence with all duplicates") (
    check(
      obtained = DistinctTile .mk
        .apply (mk_tile_message (Seq [Int] (5 , 5 , 5 , 5) ) )
        .contents
    ) (
      expected = Seq [Int] (5)
    )
  )

}


trait EvenFilterTile
  extends
    FilterTile [Int]
{



  lazy val phi : Int => Boolean =
     elem => (elem % 2 == 0)

}

case class EvenFilterTile_ () extends EvenFilterTile

object EvenFilterTile {
  def mk : EvenFilterTile =
    EvenFilterTile_ ()
}

case class FilterTileSpec ()
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

  test ("filter on empty sequence returns empty sequence") (
    check(
      obtained = EvenFilterTile .mk
        .apply (mk_tile_message (Seq [Int] () ) )
        .contents
    ) (
      expected = Seq [Int] ()
    )
  )

  test ("filter on sequence with no matching elements") (
    check(
      obtained = EvenFilterTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 3 , 5) ) )
        .contents
    ) (
      expected = Seq [Int] ()
    )
  )

  test ("filter on sequence with some matching elements") (
    check(
      obtained = EvenFilterTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3 , 4 , 5) ) )
        .contents
    ) (
      expected = Seq [Int] (2 , 4)
    )
  )

  test ("filter on sequence with all matching elements") (
    check(
      obtained = EvenFilterTile .mk
        .apply (mk_tile_message (Seq [Int] (2 , 4 , 6) ) )
        .contents
    ) (
      expected = Seq [Int] (2 , 4 , 6)
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


trait DoubleMapTile
  extends
    MapTile [Int, Int]
{



  lazy val phi : Int => Int =
     elem => elem * 2

}

case class DoubleMapTile_ () extends DoubleMapTile

object DoubleMapTile {
  def mk : DoubleMapTile =
    DoubleMapTile_ ()
}

case class MapTileSpec ()
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

  test ("map on empty sequence returns empty sequence") (
    check(
      obtained = DoubleMapTile .mk
        .apply (mk_tile_message (Seq [Int] () ) )
        .contents
    ) (
      expected = Seq [Int] ()
    )
  )

  test ("map on single element sequence") (
    check(
      obtained = DoubleMapTile .mk
        .apply (mk_tile_message (Seq [Int] (5) ) )
        .contents
    ) (
      expected = Seq [Int] (10)
    )
  )

  test ("map on multiple elements sequence") (
    check(
      obtained = DoubleMapTile .mk
        .apply (mk_tile_message (Seq [Int] (1 , 2 , 3 , 4) ) )
        .contents
    ) (
      expected = Seq [Int] (2 , 4 , 6 , 8)
    )
  )

}


trait ScenarioExample
{



  lazy val fluent_name_0 = "fluent0"

  lazy val fluent_name_1 = "fluent1"

  lazy val fluent_name_2 = "fluent2"

  lazy val fluent_name_3 = "fluent3"

  lazy val fluent_0 = Fluent .mk (fluent_name_0) (true)

  lazy val fluent_1 = Fluent .mk (fluent_name_1) (false)

  lazy val fluent_2 = Fluent .mk (fluent_name_2) (true)

  lazy val fluent_3 = Fluent .mk (fluent_name_3) (false)

  lazy val action_0 = "action0"

  lazy val action_1 = "action1"

  lazy val action_2 = "action2"

  lazy val action_3 = "action3"

  lazy val action_set_0_1 = (Seq [Action] (action_0 , action_1) ) .toSet

  lazy val action_set_1_2 = (Seq [Action] (action_1 , action_2) ) .toSet

  lazy val fluent_set_0 = (Seq [Fluent] (fluent_0) ) .toSet

  lazy val fluent_set_1 = (Seq [Fluent] (fluent_1) ) .toSet

  lazy val fluent_set_2 = (Seq [Fluent] (fluent_2) ) .toSet

  lazy val fluent_set_0_1 = (Seq [Fluent] (fluent_0 , fluent_1) ) .toSet

  lazy val if_rule_0 = IfRule (fluent_set_0 , fluent_set_0_1)

  lazy val inh_rule_0 = InhibitsRule (fluent_set_2 , action_2)

  lazy val nc_rule_0 = NoConcurrencyRule (action_set_0_1)

  lazy val nc_rule_1 = NoConcurrencyRule (action_set_1_2)

  lazy val context_0 = Seq [Rule] (if_rule_0 , inh_rule_0 , nc_rule_0 , nc_rule_1)

  lazy val flac_0 = FluentSetType (fluent_set_0_1)

  lazy val flac_1 = ActionSetType (action_set_0_1)

  lazy val flac_2 = FluentSetType (fluent_set_0_1)

  lazy val flac_3 = ActionSetType (action_set_1_2)

  lazy val flac_4 = FluentSetType (fluent_set_0_1)

  lazy val instance_0 = Seq [FluentOrActionSet] (flac_0 , flac_1 , flac_2 , flac_3 , flac_4)

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

