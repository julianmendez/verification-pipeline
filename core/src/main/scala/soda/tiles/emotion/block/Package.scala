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
import   soda.tiles.emotion.entity.Instant
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.TriggersRule



trait SlidingWindow
{

  def   defined : Boolean
  def   s0 : Option [FluentSet]
  def   a : Option [ActionSet]
  def   s1 : Option [FluentSet]
  def   accum : Seq [Instant]

}

case class SlidingWindow_ (defined : Boolean, s0 : Option [FluentSet], a : Option [ActionSet], s1 : Option [FluentSet], accum : Seq [Instant]) extends SlidingWindow

object SlidingWindow {
  def mk (defined : Boolean) (s0 : Option [FluentSet]) (a : Option [ActionSet]) (s1 : Option [FluentSet]) (accum : Seq [Instant]) : SlidingWindow =
    SlidingWindow_ (defined, s0, a, s1, accum)
}

trait InstantBuilder
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
    SlidingWindow .mk (true) (None) (None) (None) (Seq [Instant] () )

  private def _postprocess (sw : SlidingWindow) : Option [Seq [Instant] ] =
    if ( (sw .defined) && (sw .s0 .isEmpty) && (sw .a .isEmpty) && (sw .s1 .isEmpty)
    ) Some (sw .accum)
    else None

  def make_instants (seq : Seq [FluentOrActionSet] ) : Option [Seq [Instant] ] =
    _postprocess (
      fold (seq) (_empty_sliding_window) (_process_window)
    )

}

case class InstantBuilder_ () extends InstantBuilder

object InstantBuilder {
  def mk : InstantBuilder =
    InstantBuilder_ ()
}


trait Verifier
{



  def verify (instant : Instant) : Boolean =
    instant match  {
      case CausesIfRule (input_set , action , output_set) => verifyCausesIfRule (instant) (input_set) (action) (output_set)
      case IfRule (input_set , output_set) => verifyIfRule (instant) (input_set) (output_set)
      case TriggersRule (input_set , action) => verifyTriggersRule (instant) (input_set) (action)
      case AllowsRule (input_set , action) => verifyAllowsRule (instant) (input_set) (action)
      case InhibitsRule (input_set , action) => verifyInhibitsRule (instant) (input_set) (action)
      case NoConcurrencyRule (action_set) => verifyNoConcurrencyRule (instant) (action_set)
      case DefaultRule (input_fluent) => verifyDefaultRule (instant) (input_fluent)
      case InfluencesIfRule (input_set , action , output_set) => verifyInfluencesIfRule (instant) (input_set) (action) (output_set)
      case InfluencesRule (input_set , output_set) => verifyInfluencesRule (instant) (input_set) (output_set)
      case FacilitatesRule (input_set , action) => verifyFacilitatesRule (instant) (input_set) (action)
      case ContravenesRule (input_set , action) => verifyContravenesRule (instant) (input_set) (action)
      case ForbidsToCauseRule (input_set , output_set) => verifyForbidsToCauseRule (instant) (input_set) (output_set)
    }

  def verifyCausesIfRule (t : Instant) (input : FluentSet) (action : Action) (output : FluentSet) : Boolean =
    true

  def verifyIfRule (t : Instant) (input : FluentSet) (output : FluentSet) : Boolean =
    true

  def verifyTriggersRule (t : Instant) (input : FluentSet) (action : Action) : Boolean =
    true

  def verifyAllowsRule (t : Instant) (input : FluentSet) (action : Action) : Boolean =
    true

  def verifyInhibitsRule (t : Instant) (input : FluentSet) (action : Action) : Boolean =
    true

  def verifyNoConcurrencyRule (t : Instant) (action : ActionSet) : Boolean =
    true

  def verifyDefaultRule (t : Instant) (input : Fluent) : Boolean =
    true

  def verifyInfluencesIfRule (t : Instant) (input : FluentSet) (action : Action) (output : FluentSet) : Boolean =
    true

  def verifyInfluencesRule (t : Instant) (input : FluentSet) (output : FluentSet) : Boolean =
    true

  def verifyFacilitatesRule (t : Instant) (input : FluentSet) (action : Action) : Boolean =
    true

  def verifyContravenesRule (t : Instant) (input : FluentSet) (action : Action) : Boolean =
    true

  def verifyForbidsToCauseRule (t : Instant) (input : FluentSet) (output : FluentSet) : Boolean =
    true

}

case class Verifier_ () extends Verifier

object Verifier {
  def mk : Verifier =
    Verifier_ ()
}

