package soda.tiles.emotion.pipeline

/*
 * This package contains classes to model pipelines.
 */

import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileQuad
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.tile.composite.PreprocessorCrossVerifierTile
import   soda.tiles.emotion.tile.composite.TrajectoryTransitionsTile
import   soda.tiles.emotion.tile.constant.RulesTile

/**
 * This pipeline returns a sequence of Boolean values with the transitions,
 * where 'true' means that the transition was correct, and 'false' means that the transition was wrong.
 */

trait EmotionalReasoningPipeline
{



  lazy val rules_tile = RulesTile .mk

  lazy val trajectory_transitions_tile = TrajectoryTransitionsTile .mk

  lazy val preprocessor_cross_verifier_tile = PreprocessorCrossVerifierTile .mk

  def apply (message : TileMessage [Boolean] )
      : TileMessage [Seq [TileQuad [Transition, ActionSet, Rule, Boolean] ] ] =
    preprocessor_cross_verifier_tile .apply (
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

case class EmotionalReasoningPipeline_ () extends EmotionalReasoningPipeline

object EmotionalReasoningPipeline {
  def mk : EmotionalReasoningPipeline =
    EmotionalReasoningPipeline_ ()
}

