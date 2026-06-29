package soda.tiles.verifier.tile.composite

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

