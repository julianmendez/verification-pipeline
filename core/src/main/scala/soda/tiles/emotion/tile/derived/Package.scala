package soda.tiles.emotion.tile.derived

/*
 * This package contains classes to model the blocks.
 */

import   soda.lib.Fold
import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.AllowsRule
import   soda.tiles.emotion.entity.CausesIfRule
import   soda.tiles.emotion.entity.ContravenesRule
import   soda.tiles.emotion.entity.DefaultRule
import   soda.tiles.emotion.entity.FacilitatesRule
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.FluentValue
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.TransitionSeq
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.TileMessageBuilder
import   soda.tiles.emotion.entity.TilePair
import   soda.tiles.emotion.entity.TileTriple
import   soda.tiles.emotion.entity.TileQuad
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.TriggersRule
import   soda.tiles.emotion.tile.primitive.FoldTile
import   soda.tiles.emotion.tile.primitive.MapTile

trait PreprocessorTile
{



  lazy val empty_set : ActionSet = Set [Action] ()

  private def _get_actions (transition : Transition) (input : FluentSet) (action : Action) : ActionSet =
    if ( (input .forall ( fluent => transition .input .contains (fluent) ) )
    ) empty_set .+ (action)
    else empty_set

  def find (transition : Transition) (rule : Rule) : ActionSet =
    rule match  {
      case CausesIfRule (input_set , action , output_set) => empty_set
      case IfRule (input_set , output_set) => empty_set
      case TriggersRule (input_set , action) => empty_set
      case AllowsRule (input_set , action) => empty_set
      case InhibitsRule (input_set , action) => _get_actions (transition) (input_set) (action)
      case NoConcurrencyRule (action_set) => empty_set
      case DefaultRule (input_fluent) => empty_set
      case InfluencesIfRule (input_set , action , output_set) => empty_set
      case InfluencesRule (input_set , output_set) => empty_set
      case FacilitatesRule (input_set , action) => empty_set
      case ContravenesRule (input_set , action) => _get_actions (transition) (input_set) (action)
      case ForbidsToCauseRule (input_set , output_set) => empty_set
    }

  def process_elem (elem : TilePair [Transition, Rule] ) : TileTriple [Transition, Rule, ActionSet]  =
    TileTriple .mk (elem .fst) (elem .snd) (find (elem .fst) (elem .snd) )

  lazy val map_tile = MapTile .mk [TilePair [Transition, Rule] , TileTriple [Transition, Rule, ActionSet] ] (process_elem)

  def apply (message : TileMessage [Seq [TilePair [Transition, Rule] ] ] )
      : TileMessage [Seq [TileTriple [Transition, Rule, ActionSet] ] ] =
    map_tile .apply (
      message
    )

}

case class PreprocessorTile_ () extends PreprocessorTile

object PreprocessorTile {
  def mk : PreprocessorTile =
    PreprocessorTile_ ()
}






trait SlidingWindow
{

  def   defined : Boolean
  def   s0 : Option [FluentSet]
  def   a : Option [ActionSet]
  def   accum : Seq [Transition]

}

case class SlidingWindow_ (defined : Boolean, s0 : Option [FluentSet], a : Option [ActionSet], accum : Seq [Transition]) extends SlidingWindow

object SlidingWindow {
  def mk (defined : Boolean) (s0 : Option [FluentSet]) (a : Option [ActionSet]) (accum : Seq [Transition]) : SlidingWindow =
    SlidingWindow_ (defined, s0, a, accum)
}

trait TransitionsTile
{



  lazy val fold = Fold .mk

  private def _process_action (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (sw .a .isEmpty)
    )
      SlidingWindow .mk (sw .defined) (sw .s0) (Some (elem) ) (
        sw .accum
      )
    else
      SlidingWindow .mk (sw .defined) (Some (elem) ) (None) (
        (sw .accum) .:+ (Transition .mk (sw .s0 .get) (sw .a .get) (elem) )
      )

  private def _process_input_state (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (sw .s0 .isEmpty)
    ) SlidingWindow .mk (sw .defined) (Some (elem) ) (sw .a) (sw .accum)
    else _process_action (sw) (elem)

  private def _process_window (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (! (sw .defined) )
    ) sw
    else _process_input_state (sw) (elem)

  private lazy val _empty_sliding_window : SlidingWindow =
    SlidingWindow .mk (true) (None) (None) (Seq [Transition] () )

  private def _postprocess (sw : SlidingWindow) : Option [Seq [Transition] ] =
    if ( (sw .defined) && (sw .a .isEmpty)
    ) Some (sw .accum)
    else None

  def make_instants (seq : Seq [IdentifierSet] ) : Option [TransitionSeq] =
    _postprocess (
      fold (seq) (_empty_sliding_window) (_process_window)
    )

  def make_transitions (seq : Trajectory) : TransitionSeq =
    make_instants (seq) .getOrElse (Seq [Transition] () )

  def apply (message : TileMessage [Trajectory] ) : TileMessage [TransitionSeq] =
    TileMessageBuilder .mk .build (message .context) (message .instance) (
      make_transitions (message .contents)
    )

}

case class TransitionsTile_ () extends TransitionsTile

object TransitionsTile {
  def mk : TransitionsTile =
    TransitionsTile_ ()
}


trait VerifierTile
{



  def verify_CausesIfRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) (output : FluentSet) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) )
        && (transition .actions .contains (action) )
        && (! (inhibited .contains (action) ) ) )
    ) (output .forall ( fluent => transition .output .contains (fluent) ) )
    else true

  def verify_IfRule (transition : Transition) (input : FluentSet) (output : FluentSet) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) ) )
    ) (output .forall ( fluent => transition .input .contains (fluent) ) )
    else true

  def verify_TriggersRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) )
      && (! (inhibited .contains (action) ) ) )
    ) (transition .actions .contains (action) )
    else true

  def verify_AllowsRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) )
      && (transition .actions .contains (action) )
      && (! (inhibited .contains (action) ) ) )
    ) true
    else true

  def verify_InhibitsRule (transition : Transition) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) ) )
    ) ! (transition .actions .contains (action) )
    else true

  def verify_NoConcurrencyRule (transition : Transition) (actions : ActionSet) : Boolean =
    (actions .intersect (transition .actions) .toList .length) <= 1

  def verify_DefaultRule (transition : Transition) (fluent : FluentValue) : Boolean =
    transition .input .contains (fluent)

  def verify_ForbidsToCauseRule (transition : Transition) (input : FluentSet) (output : FluentSet) : Boolean =
    if ( (input .forall ( fluent => transition .input .contains (fluent) ) )
    ) (output .forall ( fluent => ! transition .output .contains (fluent) ) )
    else true

  def verify_transition (transition : Transition) (rule : Rule) (inhibited : ActionSet) : Boolean =
    rule match  {
      case CausesIfRule (input_set , action , output_set) => verify_CausesIfRule (transition) (inhibited) (input_set) (action) (output_set)
      case IfRule (input_set , output_set) => verify_IfRule (transition) (input_set) (output_set)
      case TriggersRule (input_set , action) => verify_TriggersRule (transition) (inhibited) (input_set) (action)
      case AllowsRule (input_set , action) => verify_AllowsRule (transition) (inhibited) (input_set) (action)
      case InhibitsRule (input_set , action) => verify_InhibitsRule (transition) (input_set) (action)
      case NoConcurrencyRule (action_set) => verify_NoConcurrencyRule (transition) (action_set)
      case DefaultRule (input_fluent) => verify_DefaultRule (transition) (input_fluent)
      case InfluencesIfRule (input_set , action , output_set) => verify_CausesIfRule (transition) (inhibited) (input_set) (action) (output_set)
      case InfluencesRule (input_set , output_set) => verify_IfRule (transition) (input_set) (output_set)
      case FacilitatesRule (input_set , action) => verify_AllowsRule (transition) (inhibited) (input_set) (action)
      case ContravenesRule (input_set , action) => verify_InhibitsRule (transition) (input_set) (action)
      case ForbidsToCauseRule (input_set , output_set) => verify_ForbidsToCauseRule (transition) (input_set) (output_set)
    }

  def verify_elem (elem : TileTriple [Transition, Rule, ActionSet] )
      : TileQuad [Transition, Rule, ActionSet, Boolean] =
    TileQuad .mk (elem .fst) (elem .snd) (elem .trd) (
      verify_transition (elem .fst) (elem .snd) (elem .trd)
    )

  lazy val map_tile = MapTile .mk [TileTriple [Transition, Rule, ActionSet] , TileQuad [Transition, Rule, ActionSet, Boolean] ] (verify_elem)

  def apply (message : TileMessage [Seq [TileTriple [Transition, Rule, ActionSet] ] ] )
      : TileMessage [Seq [TileQuad [Transition, Rule, ActionSet, Boolean] ] ] =
    map_tile .apply (
      message
    )

}

case class VerifierTile_ () extends VerifierTile

object VerifierTile {
  def mk : VerifierTile =
    VerifierTile_ ()
}

