package soda.tiles.emotion.block

/*
 * This package contains classes to model the blocks.
 */

import   soda.lib.Fold
import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.ActionSetType
import   soda.tiles.emotion.entity.AllowsRule
import   soda.tiles.emotion.entity.CausesIfRule
import   soda.tiles.emotion.entity.ContravenesRule
import   soda.tiles.emotion.entity.DefaultRule
import   soda.tiles.emotion.entity.FacilitatesRule
import   soda.tiles.emotion.entity.Fluent
import   soda.tiles.emotion.entity.FluentOrActionSet
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.FluentSetType
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.TriggersRule

trait Preprocessor
{



  lazy val empty_set : ActionSet = Set [Action] ()

  def find (transition : Transition) : ActionSet =
    transition match  {
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

  private def _get_actions (transition : Transition) (input : FluentSet) (action : Action) : ActionSet =
    if ( (input .forall (fluent => transition .input .contains (fluent) ) )
    ) empty_set .+ (action)
    else empty_set

}

case class Preprocessor_ () extends Preprocessor

object Preprocessor {
  def mk : Preprocessor =
    Preprocessor_ ()
}




trait SlidingWindow
{

  def   defined : Boolean
  def   s0 : Option [FluentSet]
  def   a : Option [ActionSet]
  def   s1 : Option [FluentSet]
  def   accum : Seq [Transition]

}

case class SlidingWindow_ (defined : Boolean, s0 : Option [FluentSet], a : Option [ActionSet], s1 : Option [FluentSet], accum : Seq [Transition]) extends SlidingWindow

object SlidingWindow {
  def mk (defined : Boolean) (s0 : Option [FluentSet]) (a : Option [ActionSet]) (s1 : Option [FluentSet]) (accum : Seq [Transition]) : SlidingWindow =
    SlidingWindow_ (defined, s0, a, s1, accum)
}

trait TransitionBuilder
{



  lazy val fold = Fold .mk

  private def _process_input_state (sw : SlidingWindow) (elem : FluentOrActionSet) : SlidingWindow =
    elem match  {
      case FluentSetType (s0) => SlidingWindow .mk (sw .defined) (Some (s0) ) (sw .a) (sw .s1) (sw .accum)
      case ActionSetType (a) => SlidingWindow .mk (false) (sw .s0) (sw .a) (sw .s1) (sw .accum)
    }

  private def _process_action (sw : SlidingWindow) (elem : FluentOrActionSet) : SlidingWindow =
    elem match  {
      case ActionSetType (a) => SlidingWindow .mk (sw .defined) (sw .s0) (Some (a) ) (sw .s1) (sw .accum)
      case FluentSetType (s) => SlidingWindow .mk (false) (sw .s0) (sw .a) (sw .s1) (sw .accum)
    }

  private def _process_output_state (sw : SlidingWindow) (elem : FluentOrActionSet) : SlidingWindow =
    elem match  {
      case FluentSetType (s1) => SlidingWindow .mk (sw .defined) (sw .s0) (sw .a) (Some (s1) ) (sw .accum)
      case ActionSetType (a) => SlidingWindow .mk (false) (sw .s0) (sw .a) (sw .s1) (sw .accum)
    }

  private def _process_window (sw : SlidingWindow) (elem : FluentOrActionSet) : SlidingWindow =
    if ( (! (sw .defined) )
    ) sw
    else
      if ( (sw .s0 .isEmpty)
      ) _process_input_state (sw) (elem)
      else
        if ( (sw .a .isEmpty)
        ) _process_action (sw) (elem)
        else
          if ( (sw .s1 .isEmpty)
          ) _process_output_state (sw) (elem)
          else SlidingWindow .mk (false) (sw .s0) (sw .a) (sw .s1) (sw .accum)

  private lazy val _empty_sliding_window : SlidingWindow =
    SlidingWindow .mk (true) (None) (None) (None) (Seq [Transition] () )

  private def _postprocess (sw : SlidingWindow) : Option [Seq [Transition] ] =
    if ( (sw .defined) && (sw .s0 .isEmpty) && (sw .a .isEmpty) && (sw .s1 .isEmpty)
    ) Some (sw .accum)
    else None

  def make_instants (seq : Seq [FluentOrActionSet] ) : Option [Seq [Transition] ] =
    _postprocess (
      fold (seq) (_empty_sliding_window) (_process_window)
    )

}

case class TransitionBuilder_ () extends TransitionBuilder

object TransitionBuilder {
  def mk : TransitionBuilder =
    TransitionBuilder_ ()
}


trait Verifier
{



  def verify (transition : Transition) (inhibited : ActionSet) : Boolean =
    transition match  {
      case CausesIfRule (input_set , action , output_set) => verifyCausesIfRule (transition) (inhibited) (input_set) (action) (output_set)
      case IfRule (input_set , output_set) => verifyIfRule (transition) (input_set) (output_set)
      case TriggersRule (input_set , action) => verifyTriggersRule (transition) (inhibited) (input_set) (action)
      case AllowsRule (input_set , action) => verifyAllowsRule (transition) (inhibited) (input_set) (action)
      case InhibitsRule (input_set , action) => verifyInhibitsRule (transition) (input_set) (action)
      case NoConcurrencyRule (action_set) => verifyNoConcurrencyRule (transition) (action_set)
      case DefaultRule (input_fluent) => verifyDefaultRule (transition) (input_fluent)
      case InfluencesIfRule (input_set , action , output_set) => verifyCausesIfRule (transition) (inhibited) (input_set) (action) (output_set)
      case InfluencesRule (input_set , output_set) => verifyIfRule (transition) (input_set) (output_set)
      case FacilitatesRule (input_set , action) => verifyAllowsRule (transition) (inhibited) (input_set) (action)
      case ContravenesRule (input_set , action) => verifyInhibitsRule (transition) (input_set) (action)
      case ForbidsToCauseRule (input_set , output_set) => verifyForbidsToCauseRule (transition) (input_set) (output_set)
    }

  def verifyCausesIfRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) (output : FluentSet) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) )
        && (transition .actions .contains (action) )
        && (! (inhibited .contains (action) ) ) )
    ) (output .forall (fluent => transition .output .contains (fluent) ) )
    else true

  def verifyIfRule (transition : Transition) (input : FluentSet) (output : FluentSet) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) ) )
    ) (output .forall (fluent => transition .input .contains (fluent) ) )
    else true

  def verifyTriggersRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) )
      && (! (inhibited .contains (action) ) ) )
    ) (transition .actions .contains (action) )
    else true

  def verifyAllowsRule (transition : Transition) (inhibited : ActionSet) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) )
      && (transition .actions .contains (action) )
      && (! (inhibited .contains (action) ) ) )
    ) true
    else true

  def verifyInhibitsRule (transition : Transition) (input : FluentSet) (action : Action) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) ) )
    ) ! (transition .actions .contains (action) )
    else true

  def verifyNoConcurrencyRule (transition : Transition) (actions : ActionSet) : Boolean =
    (actions .intersect (transition .actions) .toList .length) <= 1

  def verifyDefaultRule (transition : Transition) (fluent : Fluent) : Boolean =
    transition .input .contains (fluent)

  def verifyForbidsToCauseRule (transition : Transition) (input : FluentSet) (output : FluentSet) : Boolean =
    if ( (input .forall (fluent => transition .input .contains (fluent) ) )
    ) (output .forall (fluent => ! transition .output .contains (fluent) ) )
    else true

}

case class Verifier_ () extends Verifier

object Verifier {
  def mk : Verifier =
    Verifier_ ()
}

