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



  lazy val error_unknown_action = "unknown action ----> "

  def validate (actions : ActionSet) (all_actions : ActionSet) : Seq [String] =
    actions
      .toSeq
      .filter ( x => ! all_actions .contains(x) )
      .map ( x => error_unknown_action + x .toString)

  def validate_action (action : Action) (action_set : ActionSet) : Seq [String] =
    validate (Seq [Action] (action) .toSet) (action_set)

  def is_valid (actions : ActionSet) (all_actions : ActionSet) : Boolean =
    validate (actions) (all_actions) .isEmpty

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

  def validate (conf : Configuration) : Seq [String] =
    conf .rules
      .flatMap ( x =>
        rv .validate (x) (conf .fluents) (conf .actions)
      ) .++ (
        tv .validate (conf .trajectory) (conf .fluents) (conf .actions)
      )

  def is_valid (conf : Configuration) : Boolean =
    validate (conf) .isEmpty

}

case class ConfigurationValidator_ () extends ConfigurationValidator

object ConfigurationValidator {
  def mk : ConfigurationValidator =
    ConfigurationValidator_ ()
}


trait FluentValidator
{



  lazy val error_unknown_fluent = "unknown fluent ----> "

  lazy val error_duplicate_fluent_mapping = "contradictory fluent values for ----> "

  def validate_keys (fluent_set : FluentSet) (fluent_map : FluentMap) : Seq [String] =
    fluent_set
      .toSeq
      .filter ( x => ! fluent_map .contains (x) )
      .map ( x => error_unknown_fluent + x .toString)

  def validate_fluent (fluent : FluentName) (fluent_map : FluentMap) : Seq [String] =
    validate_keys (Seq [FluentName] (fluent) .toSet) (fluent_map)

  def mapped (fluent_set : FluentSet) (fluent_map : FluentMap) : Seq [Identifier] =
    fluent_set
      .toSeq
      .filter ( x => fluent_map .contains (x) )
      .map ( x => fluent_map .get (x) .get)

  def validate_injective (fluent_set : FluentSet) (fluent_map : FluentMap) (mapped : Seq [Identifier] )
      : Seq [String] =
    mapped
      .toSet
      .filter ( x =>
        mapped .count ( y => x == y) > 1 )
      .toSeq
      .map ( x => error_duplicate_fluent_mapping + x .toString)

  def validate (fluent_set : FluentSet) (fluent_map : FluentMap) : Seq [String] =
    validate_keys (fluent_set) (fluent_map) .++ (
      validate_injective (fluent_set) (fluent_map) (mapped (fluent_set) (fluent_map) ) )

  def is_valid (fluent_set : FluentSet) (fluent_map : FluentMap) : Boolean =
    validate (fluent_set) (fluent_map) .isEmpty

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

  def validate (rule : Rule) (fluent_map : FluentMap) (action_set : ActionSet) : Seq [String] =
    rule match  {
      case CausesIfRule (input , action , output) =>
        fv .validate (input) (fluent_map) .++ ( (
        fv .validate (output) (fluent_map) ) .++ (
        av .validate_action (action) (action_set) ) )
      case IfRule (input , output) =>
        fv .validate (input) (fluent_map) .++ (
        fv .validate (output) (fluent_map) )
      case TriggersRule (input , action) =>
        fv .validate (input) (fluent_map) .++ (
        av .validate_action (action) (action_set) )
      case AllowsRule (input , action) =>
        fv .validate (input) (fluent_map) .++ (
        av .validate_action (action) (action_set) )
      case InhibitsRule (input , action) =>
        fv .validate (input) (fluent_map) .++ (
        av .validate_action (action) (action_set) )
      case NoConcurrencyRule (actions) =>
        av .validate(actions)(action_set)
      case DefaultRule (fluent) =>
        fv .validate_fluent (fluent) (fluent_map)
      case InfluencesIfRule (input , action , output) =>
        fv .validate (input) (fluent_map) .++ ( (
        fv .validate (output) (fluent_map) ) .++ (
        av .validate_action (action) (action_set) ) )
      case InfluencesRule (input , output) =>
        fv .validate (input) (fluent_map) .++ (
        fv .validate (output) (fluent_map) )
      case FacilitatesRule (input , action) =>
        fv .validate (input) (fluent_map) .++ (
        av .validate_action (action) (action_set) )
      case ContravenesRule (input , action) =>
        fv .validate (input) (fluent_map) .++ (
        av .validate_action (action) (action_set) )
      case ForbidsToCauseRule (input , output) =>
        fv .validate (input) (fluent_map) .++ (
        fv .validate (output) (fluent_map) )
    }

  def is_valid (rule : Rule) (fluent_map : FluentMap) (action_set : ActionSet) : Boolean =
    validate (rule) (fluent_map) (action_set) .isEmpty

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
  def   errors : Seq [String]

}

case class ValidationWindow_ (valid : Boolean, even : Boolean, fluents : FluentMap, actions : ActionSet, errors : Seq [String]) extends ValidationWindow

object ValidationWindow {
  def mk (valid : Boolean) (even : Boolean) (fluents : FluentMap) (actions : ActionSet) (errors : Seq [String]) : ValidationWindow =
    ValidationWindow_ (valid, even, fluents, actions, errors)
}

trait TrajectoryValidator
{



  lazy val fv = FluentValidator .mk

  lazy val av = ActionValidator .mk

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
    ValidationWindow .mk (true) (true) (fluents) (actions) (Seq [String] () )

  def process_window (vw : ValidationWindow) (elem : IdentifierSet) : ValidationWindow =
    if ( (! (vw .valid) )
    ) ValidationWindow .mk (false) (! vw .even) (vw .fluents) (vw .actions) (vw .errors)
    else
      if ( (vw .even)
      ) ValidationWindow .mk (fv .is_valid (elem) (vw .fluents) ) (! vw .even) (vw .fluents) (vw .actions) (fv .validate (elem) (vw .fluents) )
      else ValidationWindow .mk (av .is_valid (elem) (vw .actions) ) (! vw .even) (vw .fluents) (vw .actions) (av .validate (elem) (vw .actions) )

  def validate_length (trajectory : Trajectory) : Seq [String] =
    if ( (trajectory .size < 3)
    ) Seq [String] (error_trajectory_is_too_short)
    else
      if ( (trajectory .size % 2 == 0)
      ) Seq [String] (error_trajectory_length_should_be_odd)
      else Seq [String] ()

  def validate (trajectory : Trajectory) (fluents : FluentMap) (actions : ActionSet) : Seq [String] =
    validate_length (trajectory) .++ (
      foldl [IdentifierSet, ValidationWindow] (trajectory) (initial_window (fluents) (actions) ) (process_window) .errors
    )

  def is_valid (trajectory : Trajectory) (fluents : FluentMap) (actions : ActionSet) : Boolean =
    validate (trajectory) (fluents) (actions) .isEmpty

}

case class TrajectoryValidator_ () extends TrajectoryValidator

object TrajectoryValidator {
  def mk : TrajectoryValidator =
    TrajectoryValidator_ ()
}

