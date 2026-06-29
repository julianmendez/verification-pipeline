package soda.tiles.verifier.tile.derived

/*
 * This package contains classes to model the blocks.
 */

import   soda.tiles.verifier.entity.Action
import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.AllowsRule
import   soda.tiles.verifier.entity.CausesIfRule
import   soda.tiles.verifier.entity.ContravenesRule
import   soda.tiles.verifier.entity.DefaultRule
import   soda.tiles.verifier.entity.FacilitatesRule
import   soda.tiles.verifier.entity.FluentSet
import   soda.tiles.verifier.entity.FluentValue
import   soda.tiles.verifier.entity.ForbidsToCauseRule
import   soda.tiles.verifier.entity.IdentifierSet
import   soda.tiles.verifier.entity.IfRule
import   soda.tiles.verifier.entity.InfluencesIfRule
import   soda.tiles.verifier.entity.InfluencesRule
import   soda.tiles.verifier.entity.InhibitsRule
import   soda.tiles.verifier.entity.NoConcurrencyRule
import   soda.tiles.verifier.entity.Rule
import   soda.tiles.verifier.entity.RuleSeq
import   soda.tiles.verifier.entity.Transition
import   soda.tiles.verifier.entity.TransitionSeq
import   soda.tiles.verifier.entity.TileMessage
import   soda.tiles.verifier.entity.TileMessageBuilder
import   soda.tiles.verifier.entity.TilePair
import   soda.tiles.verifier.entity.TileTriple
import   soda.tiles.verifier.entity.TileQuad
import   soda.tiles.verifier.entity.Trajectory
import   soda.tiles.verifier.entity.TriggersRule
import   soda.tiles.verifier.tile.primitive.ApplyTile
import   soda.tiles.verifier.tile.primitive.BindTile
import   soda.tiles.verifier.tile.primitive.FoldTile

/*
directive lean
import soda.tiles.verifier.entity.TileMessage
*/

/**
 * This takes a condition (predicate) and passes through only those elements that satisfy it, discarding all others
 * while preserving the original order.
 */

trait FilterTile [A ]
{

  def   phi : A => Boolean

  def filter (elem : A) : Seq [A] =
    if ( (phi (elem) )
    ) Seq [A] (elem)
    else Seq [A] ()

  lazy val bind_tile = BindTile .mk (filter)

  def apply (message : TileMessage [Seq [A] ] ) : TileMessage [Seq [A] ] =
    bind_tile .apply (
      message
    )

}

case class FilterTile_ [A] (phi : A => Boolean) extends FilterTile [A]

object FilterTile {
  def mk [A] (phi : A => Boolean) : FilterTile [A] =
    FilterTile_ [A] (phi)
}


trait InhibitTile
{



  lazy val empty_set : ActionSet = Set [Action] ()

  private def _get_actions (transition : Transition) (input : FluentSet) (action : Action) : ActionSet =
    if ( (input .forall ( fluent => transition .input .contains (fluent) ) )
    ) empty_set .+ (action)
    else empty_set

  def get_inhibit (transition : Transition) (rule : Rule) : ActionSet =
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

  def get_inhibit_set (transition : Transition) (rules : RuleSeq) : ActionSet =
    rules
      .flatMap ( rule => get_inhibit (transition) (rule) )
      .toSet

  def process_elem (elem : Transition) (rules : RuleSeq) : TilePair [Transition, ActionSet]  =
    TilePair .mk (elem) (get_inhibit_set (elem) (rules) )

  def apply (message : TileMessage [Seq [Transition] ] )
      : TileMessage [Seq [TilePair [Transition, ActionSet] ] ] =
    MapTile
      .mk [Transition, TilePair [Transition, ActionSet] ] (
         transition => process_elem (transition) (message .context)
      ) .apply (
      message
    )

}

case class InhibitTile_ () extends InhibitTile

object InhibitTile {
  def mk : InhibitTile =
    InhibitTile_ ()
}


/*
directive lean
import soda.tiles.verifier.entity.TileMessage
*/

/**
 * This tile applies a transformation function to each element of the sequence,
 * producing a new sequence with the transformed elements, preserving the original order.
 */

trait MapTile [A , B ]
{

  def   phi : A => B

  def bind_phi (x : A) : Seq [B] =
    Seq [B] (phi (x) )

  lazy val bind_tile = BindTile .mk (bind_phi)

  def apply (message : TileMessage [Seq [A] ] ) : TileMessage [Seq [B] ] =
    bind_tile .apply (
      message
    )

}

case class MapTile_ [A, B] (phi : A => B) extends MapTile [A, B]

object MapTile {
  def mk [A, B] (phi : A => B) : MapTile [A, B] =
    MapTile_ [A, B] (phi)
}


/*
directive lean
import soda.tiles.verifier.entity.TileMessage
*/

/**
 * This tile reverses a collection.
 */

trait ReverseTile [A ]
{



  lazy val zero : Seq [A] = Seq [A] ()

  def prepend (acc : Seq [A] ) (elem : A) : Seq [A] =
    acc .+: (elem)

  lazy val fold_tile = FoldTile .mk [A, Seq [A] ] (zero) (prepend)

  def apply (message : TileMessage [Seq [A] ] ) : TileMessage [Seq [A] ] =
    fold_tile .apply (
      message
    )

}

case class ReverseTile_ [A] () extends ReverseTile [A]

object ReverseTile {
  def mk [A] : ReverseTile [A] =
    ReverseTile_ [A] ()
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



  def process_action (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (sw .a .isEmpty)
    )
      SlidingWindow .mk (sw .defined) (sw .s0) (Some (elem) ) (
        sw .accum
      )
    else
      SlidingWindow .mk (sw .defined) (Some (elem) ) (None) (
        (sw .accum) .:+ (Transition .mk (sw .s0 .get) (sw .a .get) (elem) )
      )

  def process_input_state (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (sw .s0 .isEmpty)
    ) SlidingWindow .mk (sw .defined) (Some (elem) ) (sw .a) (sw .accum)
    else process_action (sw) (elem)

  def process_window (sw : SlidingWindow) (elem : IdentifierSet) : SlidingWindow =
    if ( (! (sw .defined) )
    ) sw
    else process_input_state (sw) (elem)

  lazy val empty_sliding_window : SlidingWindow =
    SlidingWindow .mk (true) (None) (None) (Seq [Transition] () )

  def postprocess (sw : SlidingWindow) : Seq [Transition] =
    if ( (sw .defined) && (sw .a .isEmpty)
    ) sw .accum
    else Seq [Transition] ()

  lazy val apply_tile = ApplyTile .mk [SlidingWindow, Seq [Transition] ] (postprocess)

  lazy val fold_tile = FoldTile .mk (empty_sliding_window) (process_window)

  def apply (message : TileMessage [Trajectory] ) : TileMessage [TransitionSeq] =
    apply_tile .apply (
      fold_tile .apply (
        message
      )
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

  def verify_transition (transition : Transition) (inhibited : ActionSet) (rule : Rule) : Boolean =
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

  def verify_elem (elem : TilePair [TilePair [Transition, ActionSet] , Rule] )
      : TileQuad [Transition, ActionSet, Rule, Boolean] =
    TileQuad .mk (elem .fst .fst) (elem .fst .snd) (elem .snd) (
      verify_transition (elem .fst .fst) (elem .fst .snd) (elem .snd)
    )

  lazy val map_tile = MapTile .mk [TilePair [TilePair [Transition, ActionSet] , Rule] , TileQuad [Transition, ActionSet, Rule, Boolean] ] (verify_elem)

  def apply (message : TileMessage [Seq [TilePair [TilePair [Transition, ActionSet] , Rule] ] ] )
      : TileMessage [Seq [TileQuad [Transition, ActionSet, Rule, Boolean] ] ] =
    map_tile .apply (
      message
    )

}

case class VerifierTile_ () extends VerifierTile

object VerifierTile {
  def mk : VerifierTile =
    VerifierTile_ ()
}

