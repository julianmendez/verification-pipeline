package soda.tiles.verifier.pipeline

/*
 * This package contains classes to model pipelines.
 */

import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.Rule
import   soda.tiles.verifier.entity.TileMessage
import   soda.tiles.verifier.entity.TileQuad
import   soda.tiles.verifier.entity.Transition
import   soda.tiles.verifier.tile.composite.InhibitCrossVerifierTile
import   soda.tiles.verifier.tile.composite.TrajectoryTransitionsTile
import   soda.tiles.verifier.tile.constant.RulesTile

/**
 * This pipeline returns a sequence of Boolean values with the transitions,
 * where 'true' means that the transition was correct, and 'false' means that the transition was wrong.
 */

trait VerificationPipeline
{



  lazy val rules_tile = RulesTile .mk

  lazy val trajectory_transitions_tile = TrajectoryTransitionsTile .mk

  lazy val inhibit_cross_verifier_tile = InhibitCrossVerifierTile .mk

  def apply (message : TileMessage [Boolean] )
      : TileMessage [Seq [TileQuad [Transition, ActionSet, Rule, Boolean] ] ] =
    inhibit_cross_verifier_tile .apply (
      trajectory_transitions_tile .apply (
        message
      )
    ) (
      rules_tile .apply (
        message
      )
    )

  def run (message : TileMessage [Boolean] )
    : TileMessage [Seq [TileQuad [Transition, ActionSet, Rule, Boolean] ] ] =
    apply (message)

}

case class VerificationPipeline_ () extends VerificationPipeline

object VerificationPipeline {
  def mk : VerificationPipeline =
    VerificationPipeline_ ()
}

