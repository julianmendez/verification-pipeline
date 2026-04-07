package soda.tiles.emotion.entity

/*
 * This package contains classes to model entities.
 */





trait Configuration
{

  def   fluents : FluentSet
  def   actions : ActionSet
  def   rules : RuleSet
  def   trajectory : Trajectory

}

case class Configuration_ (fluents : FluentSet, actions : ActionSet, rules : RuleSet, trajectory : Trajectory) extends Configuration

object Configuration {
  def mk (fluents : FluentSet) (actions : ActionSet) (rules : RuleSet) (trajectory : Trajectory) : Configuration =
    Configuration_ (fluents, actions, rules, trajectory)
}


type Identifier = String

type Action = Identifier

type FluentName = Identifier

type FluentValue = Boolean

trait Fluent
{

  def   name : FluentName
  def   value : FluentValue

}

case class Fluent_ (name : FluentName, value : FluentValue) extends Fluent

object Fluent {
  def mk (name : FluentName) (value : FluentValue) : Fluent =
    Fluent_ (name, value)
}

type FluentSet = Set [Fluent]

type ActionSet = Set [Action]

sealed trait FluentOrActionSet

case class FluentSetType (fluent_set : FluentSet) extends FluentOrActionSet

case class ActionSetType (action_set : ActionSet) extends FluentOrActionSet


trait Transition
{

  def   input : FluentSet
  def   actions : ActionSet
  def   output : FluentSet

}

case class Transition_ (input : FluentSet, actions : ActionSet, output : FluentSet) extends Transition

object Transition {
  def mk (input : FluentSet) (actions : ActionSet) (output : FluentSet) : Transition =
    Transition_ (input, actions, output)
}

type Trajectory = Seq [FluentOrActionSet]

type Instance = Trajectory




sealed trait Rule

case class CausesIfRule (input : FluentSet , action : Action , output : FluentSet) extends Rule

case class IfRule (input : FluentSet , output : FluentSet) extends Rule

case class TriggersRule (input : FluentSet , action : Action) extends Rule

case class AllowsRule (input : FluentSet , action : Action) extends Rule

case class InhibitsRule (input : FluentSet , action : Action) extends Rule

case class NoConcurrencyRule (action : ActionSet) extends Rule

case class DefaultRule (input : Fluent) extends Rule

case class InfluencesIfRule (input : FluentSet , action : Action , output : FluentSet) extends Rule

case class InfluencesRule (input : FluentSet , output : FluentSet) extends Rule

case class FacilitatesRule (input : FluentSet , action : Action) extends Rule

case class ContravenesRule (input : FluentSet , action : Action) extends Rule

case class ForbidsToCauseRule (input : FluentSet , output : FluentSet) extends Rule


type RuleSet = Seq [Rule]

type Context = RuleSet


/*
directive lean
import Soda.tiles.emotion.entity.Entity
*/

trait TilePair [A , B ]
{

  def   fst : A
  def   snd : B

}

case class TilePair_ [A, B] (fst : A, snd : B) extends TilePair [A, B]

object TilePair {
  def mk [A, B] (fst : A) (snd : B) : TilePair [A, B] =
    TilePair_ [A, B] (fst, snd)
}

trait TileTriple [A , B , C ]
{

  def   fst : A
  def   snd : B
  def   trd : C

}

case class TileTriple_ [A, B, C] (fst : A, snd : B, trd : C) extends TileTriple [A, B, C]

object TileTriple {
  def mk [A, B, C] (fst : A) (snd : B) (trd : C) : TileTriple [A, B, C] =
    TileTriple_ [A, B, C] (fst, snd, trd)
}

trait TileMessage [A ]
{

  def   context : RuleSet
  def   instance : Trajectory
  def   contents : A

}

case class TileMessage_ [A] (context : RuleSet, instance : Trajectory, contents : A) extends TileMessage [A]

object TileMessage {
  def mk [A] (context : RuleSet) (instance : Trajectory) (contents : A) : TileMessage [A] =
    TileMessage_ [A] (context, instance, contents)
}

trait TileMessageBuilder
{



  def build [A ] (context : Context) (instance : Instance) (contents : A) : TileMessage [A] =
    TileMessage .mk (context) (instance) (contents)

}

case class TileMessageBuilder_ () extends TileMessageBuilder

object TileMessageBuilder {
  def mk : TileMessageBuilder =
    TileMessageBuilder_ ()
}

