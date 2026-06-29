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
import   soda.tiles.verifier.tile.derived.ReverseTile
import   soda.tiles.verifier.tile.derived.TransitionsTile
import   soda.tiles.verifier.tile.derived.VerifierTile
import   soda.tiles.verifier.tile.primitive.CrossTile
import   soda.tiles.verifier.tile.primitive.FoldTile





/*
directive lean
import Soda.tiles.fairness.tool.TileMessage
*/

/**
 * This tile returns a collection containing only the unique elements from the original, removing any duplicates while
 * keeping the first occurrence of each.
 */

trait DistinctTile [A ]
{



  lazy val zero : Seq [A] = Seq [A] ()

  def add_if_new (acc : Seq [A] ) (elem : A) : Seq [A] =
    if ( (acc .contains (elem) )
    ) acc
    else acc .+: (elem)

  lazy val main_fold_tile = FoldTile .mk [A, Seq [A] ] (zero) (add_if_new)

  lazy val reverse_tile = ReverseTile .mk [A]

  def apply (message : TileMessage [Seq [A] ] ) : TileMessage [Seq [A] ] =
    reverse_tile .apply (
      main_fold_tile .apply (
        message
      )
    )

}

case class DistinctTile_ [A] () extends DistinctTile [A]

object DistinctTile {
  def mk [A] : DistinctTile [A] =
    DistinctTile_ [A] ()
}


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



