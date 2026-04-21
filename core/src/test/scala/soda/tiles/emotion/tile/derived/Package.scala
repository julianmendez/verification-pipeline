package soda.tiles.emotion.tile.derived

/*
 * This package contains test classes to model the blocks.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileMessageBuilder
import   soda.tiles.emotion.tile.primitive.ScenarioExample





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
    Seq [IdentifierSet] (s0_elem, a_elem, s1_elem)

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
    Seq [IdentifierSet] (s0_elem, a_elem, s1_elem, extra)

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
    Seq [IdentifierSet] (s0_elem, a_elem)

  test ("two-element trajectory produces empty transition sequence") (
    check (
      obtained = transitions_tile
        .apply (mk_tile_message (seq_test_5) )
        .contents
    ) (
      expected = Seq [Transition] ()
    )
  )

/*
  test ("context and instance are preserved by TransitionsTile") (
    val msg =
      mk_tile_message (Seq [IdentifierSet] () )

    private lazy val __soda__val result =
      transitions_tile.apply(msg)

    assert(result.context == msg.context)
    assert(result.instance == msg.instance)
  )
*/

}

