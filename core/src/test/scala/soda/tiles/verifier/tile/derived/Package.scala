package soda.tiles.verifier.tile.derived

/*
 * This package contains test classes to model the blocks.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.verifier.entity.Identifier
import   soda.tiles.verifier.entity.IdentifierSet
import   soda.tiles.verifier.entity.Transition
import   soda.tiles.verifier.entity.TileMessage
import   soda.tiles.verifier.entity.TileMessageBuilder
import   soda.tiles.verifier.tile.primitive.ScenarioExample





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


case class TransitionsTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained: A) (expected: A): org.scalatest.compatible.Assertion =
    assert(obtained == expected)

  lazy val scenario = ScenarioExample .mk

  lazy val s0_elem = Set [Identifier] ("s0_elem")

  lazy val a_elem = Set [Identifier] ("a_elem")

  lazy val s1_elem = Set [Identifier] ("s1_elem")

  lazy val elem0 = Set [Identifier] ("elem0")

  lazy val extra = Set [Identifier] ("extra")

  def mk_tile_message [A ] (seq: Seq[A]) : TileMessage[Seq[A]] =
    TileMessageBuilder
      .mk
      .build (scenario.context_0) (scenario.instance_0) (seq)

  lazy val transitions_tile = TransitionsTile .mk

  test ("empty trajectory produces empty transition sequence") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (Seq [IdentifierSet] () ) )
        .contents
    ) (
      expected = Seq [Transition] ()
    )
  )

  test ("single-element trajectory produces empty transition sequence") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (Seq [IdentifierSet] (elem0) ) )
        .contents
    ) (
      expected = Seq [Transition] ()
    )
  )

  lazy val seq_test_3 =
    Seq [IdentifierSet] (s0_elem , a_elem , s1_elem)

  lazy val obtained_test_3 =
    transitions_tile
      .apply (mk_tile_message(seq_test_3) )
      .contents

  lazy val expected_test_3 =
    Seq(Transition.mk(s0_elem) (a_elem) (s1_elem))

  test ("three-element trajectory produces one transition") (
    check (obtained_test_3) (expected_test_3)
  )

  lazy val seq_test_4 =
    Seq [IdentifierSet] (s0_elem , a_elem , s1_elem , extra)

  test ("four-element trajectory produces empty transition sequence") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (seq_test_4) )
        .contents
    ) (
      expected = Seq [Transition] ()
    )
  )

  lazy val seq_test_5 =
    Seq [IdentifierSet] (s0_elem , a_elem)

  test ("two-element trajectory produces empty transition sequence") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (seq_test_5) )
        .contents
    ) (
      expected = Seq [Transition] ()
    )
  )

  lazy val seq_test_7 =
    Seq [IdentifierSet] (s0_elem , a_elem , s1_elem , elem0 , extra , a_elem , s0_elem)

  lazy val expected_test_7 =
    Seq [Transition] (
      Transition .mk (s0_elem) (a_elem) (s1_elem) ,
      Transition .mk (s1_elem) (elem0) (extra) ,
      Transition .mk (extra) (a_elem) (s0_elem)
    )

  test ("seven-element trajectory produces correctly ordered transitions") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (seq_test_7) )
        .contents
    ) (
      expected = expected_test_7
    )
  )

}

