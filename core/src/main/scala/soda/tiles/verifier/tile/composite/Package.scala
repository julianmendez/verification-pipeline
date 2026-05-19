package soda.tiles.verifier.tile.composite

/*
 * This package contains classes to model composite tiles.
 */

import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.Rule
import   soda.tiles.verifier.entity.RuleSeq
import   soda.tiles.verifier.entity.TileMessage
import   soda.tiles.verifier.entity.TilePair
import   soda.tiles.verifier.entity.TileQuad
import   soda.tiles.verifier.entity.Transition
import   soda.tiles.verifier.entity.TransitionSeq
import   soda.tiles.verifier.tile.constant.TrajectoryTile
import   soda.tiles.verifier.tile.derived.InhibitTile
import   soda.tiles.verifier.tile.derived.TransitionsTile
import   soda.tiles.verifier.tile.derived.VerifierTile
import   soda.tiles.verifier.tile.primitive.CrossTile

trait InhibitCrossVerifierTile
{



  lazy val cross_tile = CrossTile .mk [TilePair [Transition, ActionSet] , Rule]

  lazy val inhibit_tile = InhibitTile .mk

  lazy val verifier_tile = VerifierTile .mk

  def apply (message0 : TileMessage [TransitionSeq] ) (message1 : TileMessage [RuleSeq] )
      : TileMessage [Seq [TileQuad [Transition, ActionSet, Rule, Boolean] ] ] =
    verifier_tile .apply (
      cross_tile .apply (
        inhibit_tile .apply (
          message0
        )
      ) (
        message1
      )
    )

}

case class InhibitCrossVerifierTile_ () extends InhibitCrossVerifierTile

object InhibitCrossVerifierTile {
  def mk : InhibitCrossVerifierTile =
    InhibitCrossVerifierTile_ ()
}


trait TrajectoryTransitionsTile
{



  lazy val trajectory_tile = TrajectoryTile .mk

  lazy val transitions_tile = TransitionsTile .mk

  def apply (message : TileMessage [Boolean] ) : TileMessage [TransitionSeq] =
    transitions_tile (
      trajectory_tile (
        message
      )
    )

}

case class TrajectoryTransitionsTile_ () extends TrajectoryTransitionsTile

object TrajectoryTransitionsTile {
  def mk : TrajectoryTransitionsTile =
    TrajectoryTransitionsTile_ ()
}



