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

