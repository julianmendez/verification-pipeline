package soda.tiles.emotion.tile.composite

/*
 * This package contains classes to model composite tiles.
 */

import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.RuleSeq
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileQuad
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.TransitionSeq
import   soda.tiles.emotion.tile.constant.TrajectoryTile
import   soda.tiles.emotion.tile.derived.PreprocessorTile
import   soda.tiles.emotion.tile.derived.TransitionsTile
import   soda.tiles.emotion.tile.derived.VerifierTile
import   soda.tiles.emotion.tile.primitive.CrossTile

trait CrossPreprocessorVerifierTile
{



  lazy val cross_tile = CrossTile .mk [Transition, Rule]

  lazy val preprocessor_tile = PreprocessorTile .mk

  lazy val verifier_tile = VerifierTile .mk

  def apply (message0 : TileMessage [TransitionSeq] ) (message1 : TileMessage [RuleSeq] )
      : TileMessage [Seq [TileQuad [Transition, Rule, ActionSet, Boolean] ] ] =
    verifier_tile .apply (
      preprocessor_tile .apply (
        cross_tile .apply (
          message0
        ) (
          message1
        )
      )
    )

}

case class CrossPreprocessorVerifierTile_ () extends CrossPreprocessorVerifierTile

object CrossPreprocessorVerifierTile {
  def mk : CrossPreprocessorVerifierTile =
    CrossPreprocessorVerifierTile_ ()
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



