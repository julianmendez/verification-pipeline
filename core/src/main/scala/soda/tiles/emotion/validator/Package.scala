package soda.tiles.emotion.validator

/*
 * This package contains classes to validate entities.
 */

import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.AllowsRule
import   soda.tiles.emotion.entity.CausesIfRule
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.ContravenesRule
import   soda.tiles.emotion.entity.DefaultRule
import   soda.tiles.emotion.entity.FacilitatesRule
import   soda.tiles.emotion.entity.FluentMap
import   soda.tiles.emotion.entity.FluentName
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.TriggersRule





trait ActionValidator
{



  lazy val vu = ValidationUtil .mk

  lazy val error_unknown_action = "unknown action"

  def is_valid (actions : ActionSet) (all_actions : ActionSet) : Boolean =
    actions
      .forall ( x => all_actions .contains (x) )

  def validate_action (action : Action) (action_set : ActionSet) : Option [String] =
    if ( action_set .contains(action)
    ) None
    else Some (error_unknown_action + action.toString)

  def validate (actions : ActionSet) (all_actions : ActionSet) : Option[String] =
    vu .concatenate (
      actions
        .toSeq
        .filter ( x => ! all_actions .contains(x) )
        .map ( x => Some (error_unknown_action + " " + x .toString) )
    )

}

case class ActionValidator_ () extends ActionValidator

object ActionValidator {
  def mk : ActionValidator =
    ActionValidator_ ()
}


trait ConfigurationValidator
{



  lazy val rv = RuleValidator .mk

  lazy val tv = TrajectoryValidator .mk

  lazy val vu = ValidationUtil .mk

  def is_valid (conf : Configuration) : Boolean =
    conf .rules .forall ( x =>
      rv .is_valid (x) (conf .fluents) (conf .actions)
    ) && tv .is_valid (conf .trajectory) (conf .fluents) (conf .actions)

  def validate (conf : Configuration) : Option [String] =
    vu .concatenate (
      conf .rules .map ( x =>
        rv .validate (x) (conf .fluents) (conf .actions)
      ) .++ (
        Seq [Option [String] ] (
          tv .validate (conf .trajectory) (conf .fluents) (conf .actions)
        )
      )
    )

}

case class ConfigurationValidator_ () extends ConfigurationValidator

object ConfigurationValidator {
  def mk : ConfigurationValidator =
    ConfigurationValidator_ ()
}


trait FluentValidator
{



  lazy val vu = ValidationUtil .mk

  lazy val error_unknown_fluent = "unknown fluent"

  lazy val error_duplicate_fluent_mapping = "duplicate fluent mapping"

  def get_fluents (fluent_set : FluentSet) (fluent_map : FluentMap) : Set [FluentName] =
    fluent_set
      .filter ( x => fluent_map .contains (x) )
      .map ( x => fluent_map .get (x) .get)

  def is_valid (fluent_set : FluentSet) (fluent_map : FluentMap) : Boolean =
    (get_fluents (fluent_set) (fluent_map) .size) == (fluent_set .size)

  def validate_fluent (fluent : FluentName) (fluent_map : FluentMap) : Option [String] =
    if ( fluent_map .contains (fluent)
    ) None
    else Some (error_unknown_fluent + fluent.toString)

  def validate_keys (fluent_set : FluentSet) (fluent_map : FluentMap) : Option [String] =
    fluent_set
      .find ( x => ! fluent_map .contains (x) )
      .map ( x => error_unknown_fluent + " " + x .toString)

  def mapped (fluent_set : FluentSet) (fluent_map : FluentMap) : Seq [Identifier] =
    fluent_set
      .toSeq
      .filter ( x => fluent_map .contains (x) )
      .map ( x => fluent_map .get (x) .get)

  private def _validate_injective_with_set (fluent_set : FluentSet) (fluent_map : FluentMap) (
      seq : Seq [Identifier] ) (uniq : Set [Identifier] ) : Option [String] =
    seq .filter ( x => ! uniq .contains (x) )
      .headOption
      .map ( x => error_duplicate_fluent_mapping + " " + x .toString)

  private def _validate_injective_with (fluent_set : FluentSet) (fluent_map : FluentMap) (seq : Seq [Identifier] )
      : Option [String] =
    _validate_injective_with_set (fluent_set) (fluent_map) (seq) (seq .toSet)

  def validate_injective (fluent_set : FluentSet) (fluent_map : FluentMap) : Option [String] =
    _validate_injective_with (fluent_set) (fluent_map) (mapped (fluent_set) (fluent_map) )

  def validate (fluent_set : FluentSet) (fluent_map : FluentMap) : Option [String] =
    vu .concatenate_pair (
      validate_keys (fluent_set) (fluent_map) ) (
      validate_injective (fluent_set) (fluent_map) )

}

case class FluentValidator_ () extends FluentValidator

object FluentValidator {
  def mk : FluentValidator =
    FluentValidator_ ()
}


trait RuleValidator
{



  lazy val fv = FluentValidator .mk

  lazy val av = ActionValidator .mk

  lazy val vu = ValidationUtil .mk

  def is_valid (rule : Rule) (fluent_map : FluentMap) (action_set : ActionSet) : Boolean =
    rule match  {
      case CausesIfRule (input , action , output) =>
        fv .is_valid (input) (fluent_map) && fv .is_valid (output) (fluent_map) &&
        action_set .contains (action)
      case IfRule (input , output) =>
        fv .is_valid (input) (fluent_map) && fv .is_valid (output) (fluent_map)
      case TriggersRule (input , action) =>
        fv .is_valid (input) (fluent_map) && action_set .contains (action)
      case AllowsRule (input , action) =>
        fv .is_valid (input) (fluent_map) && action_set .contains (action)
      case InhibitsRule (input , action) =>
        fv .is_valid (input) (fluent_map) && action_set .contains (action)
      case NoConcurrencyRule (actions) =>
        av .is_valid (actions) (action_set)
      case DefaultRule (fluent) =>
        fluent_map .contains (fluent)
      case InfluencesIfRule (input , action , output) =>
        fv .is_valid (input) (fluent_map) && fv .is_valid (output) (fluent_map) &&
        action_set .contains (action)
      case InfluencesRule (input , output) =>
        fv .is_valid (input) (fluent_map) && fv .is_valid (output) (fluent_map)
      case FacilitatesRule (input , action) =>
        fv .is_valid (input) (fluent_map) && action_set .contains (action)
      case ContravenesRule (input , action) =>
        fv .is_valid (input) (fluent_map) && action_set .contains (action)
      case ForbidsToCauseRule (input , output) =>
        fv .is_valid (input) (fluent_map) && fv .is_valid (output) (fluent_map)
    }

  def validate (rule : Rule) (fluent_map : FluentMap) (action_set : ActionSet) : Option [String] =
    rule match  {
      case CausesIfRule (input , action , output) =>
        vu .concatenate_triplet (
          fv .validate (input) (fluent_map) ) (
          fv .validate (output) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case IfRule (input , output) =>
        vu .concatenate_pair (
         fv .validate (input) (fluent_map) ) (
         fv .validate (output) (fluent_map) )
      case TriggersRule (input , action) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case AllowsRule (input , action) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case InhibitsRule (input , action) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case NoConcurrencyRule (actions) =>
        av .validate(actions)(action_set)
      case DefaultRule (fluent) =>
        fv .validate_fluent (fluent) (fluent_map)
      case InfluencesIfRule (input , action , output) =>
        vu .concatenate_triplet (
          fv .validate (input) (fluent_map) ) (
          fv .validate (output) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case InfluencesRule (input , output) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          fv .validate (output) (fluent_map) )
      case FacilitatesRule (input , action) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case ContravenesRule (input , action) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          av .validate_action (action) (action_set) )
      case ForbidsToCauseRule (input , output) =>
        vu .concatenate_pair (
          fv .validate (input) (fluent_map) ) (
          fv .validate (output) (fluent_map) )
    }

}

case class RuleValidator_ () extends RuleValidator

object RuleValidator {
  def mk : RuleValidator =
    RuleValidator_ ()
}


trait ValidationWindow
{

  def   valid : Boolean
  def   even : Boolean
  def   fluents : FluentMap
  def   actions : ActionSet
  def   error : Option [String]

}

case class ValidationWindow_ (valid : Boolean, even : Boolean, fluents : FluentMap, actions : ActionSet, error : Option [String]) extends ValidationWindow

object ValidationWindow {
  def mk (valid : Boolean) (even : Boolean) (fluents : FluentMap) (actions : ActionSet) (error : Option [String]) : ValidationWindow =
    ValidationWindow_ (valid, even, fluents, actions, error)
}

trait TrajectoryValidator
{



  lazy val fv = FluentValidator .mk

  lazy val av = ActionValidator .mk

  lazy val vu = ValidationUtil .mk

  lazy val error_trajectory_is_too_short = "the trajectory is too short"

  lazy val error_trajectory_length_should_be_odd = "the trajectory length should be an odd number"

  private def _tailrec_foldl [A , B ] (sequence : Seq [A] ) (current : B)
      (next : B => A => B) : B =
    sequence match  {
      case Nil => current
      case (head) +: (tail) =>
        _tailrec_foldl [A, B] (tail) (next (current) (head) ) (next)
    }

  def foldl [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B) : B =
    _tailrec_foldl [A, B] (sequence) (initial) (next)

  def initial_window (fluents : FluentMap) (actions : ActionSet) : ValidationWindow =
    ValidationWindow .mk (true) (true) (fluents) (actions) (None)

  def process_window (vw : ValidationWindow) (elem : IdentifierSet) : ValidationWindow =
    if ( (! (vw .valid) )
    ) ValidationWindow .mk (false) (! vw .even) (vw .fluents) (vw .actions) (vw .error)
    else
      if ( (vw .even)
      ) ValidationWindow .mk (fv .is_valid (elem) (vw .fluents) ) (! vw .even) (vw .fluents) (vw .actions) (fv .validate (elem) (vw .fluents) )
      else ValidationWindow .mk (av .is_valid (elem) (vw .actions) ) (! vw .even) (vw .fluents) (vw .actions) (av .validate (elem) (vw .actions) )

  def has_valid_length (trajectory : Trajectory) : Boolean =
    (trajectory .size >= 3) && (! (trajectory .size % 2 == 0) )

  def validate_length (trajectory : Trajectory) : Option [String] =
    if ( (trajectory .size < 3)
    ) Some (error_trajectory_is_too_short)
    else
      if ( (trajectory .size % 2 == 0)
      ) Some (error_trajectory_length_should_be_odd)
      else None

  def is_valid (trajectory : Trajectory) (fluents : FluentMap) (actions : ActionSet) : Boolean =
    has_valid_length (trajectory) &&
    foldl [IdentifierSet, ValidationWindow] (trajectory) (initial_window (fluents) (actions) ) (process_window) .valid

  def validate (trajectory : Trajectory) (fluents : FluentMap) (actions : ActionSet) : Option [String] =
    vu .concatenate_pair (
      validate_length (trajectory) ) (
      foldl [IdentifierSet, ValidationWindow] (trajectory) (initial_window (fluents) (actions) ) (process_window) .error
    )

}

case class TrajectoryValidator_ () extends TrajectoryValidator

object TrajectoryValidator {
  def mk : TrajectoryValidator =
    TrajectoryValidator_ ()
}


trait ValidationUtil
{



  lazy val separator = "\n"

  lazy val empty = ""

  def concatenate (seq : Seq [Option [String] ] ) : Option [String] =
    if ( (seq .exists ( x => x .isDefined) )
    ) Some (
      seq
        .filter ( x => x .isDefined)
        .map ( x => x .getOrElse (empty) )
        .mkString (separator)
    )
    else None

  def concatenate_pair (fst : Option [String] ) (snd : Option [String] ) : Option [String] =
    concatenate (
      Seq [Option [String] ] (fst , snd)
    )

  def concatenate_triplet (fst : Option [String] ) (snd : Option [String] ) (trd : Option [String] ) : Option [String] =
    concatenate (
      Seq [Option [String] ] (fst , snd , trd)
    )

}

case class ValidationUtil_ () extends ValidationUtil

object ValidationUtil {
  def mk : ValidationUtil =
    ValidationUtil_ ()
}

