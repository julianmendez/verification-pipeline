package soda.tiles.emotion.entity

/*
 * This package contains classes to model entities.
 */





trait Configuration
{

  def   fluents : FluentMap
  def   actions : ActionSet
  def   rules : RuleSeq
  def   trajectory : Trajectory

}

case class Configuration_ (fluents : FluentMap, actions : ActionSet, rules : RuleSeq, trajectory : Trajectory) extends Configuration

object Configuration {
  def mk (fluents : FluentMap) (actions : ActionSet) (rules : RuleSeq) (trajectory : Trajectory) : Configuration =
    Configuration_ (fluents, actions, rules, trajectory)
}


type Identifier = String

type IdentifierSet = Set [Identifier]

type FluentName = Identifier

type FluentValue = Identifier

type FluentSet = IdentifierSet

type FluentMap = Map [FluentValue, FluentName]

type Action = Identifier

type ActionSet = IdentifierSet

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

type TransitionSeq = Seq [Transition]

type Trajectory = Seq [IdentifierSet]

type Instance = Trajectory




sealed trait Rule

case class CausesIfRule (input : FluentSet , action : Action , output : FluentSet) extends Rule

case class IfRule (input : FluentSet , output : FluentSet) extends Rule

case class TriggersRule (input : FluentSet , action : Action) extends Rule

case class AllowsRule (input : FluentSet , action : Action) extends Rule

case class InhibitsRule (input : FluentSet , action : Action) extends Rule

case class NoConcurrencyRule (actions : ActionSet) extends Rule

case class DefaultRule (fluent : FluentValue) extends Rule

case class InfluencesIfRule (input : FluentSet , action : Action , output : FluentSet) extends Rule

case class InfluencesRule (input : FluentSet , output : FluentSet) extends Rule

case class FacilitatesRule (input : FluentSet , action : Action) extends Rule

case class ContravenesRule (input : FluentSet , action : Action) extends Rule

case class ForbidsToCauseRule (input : FluentSet , output : FluentSet) extends Rule


type RuleSeq = Seq [Rule]

type Context = RuleSeq


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

trait TileQuad [A , B , C , D ]
{

  def   fst : A
  def   snd : B
  def   trd : C
  def   fth : D

}

case class TileQuad_ [A, B, C, D] (fst : A, snd : B, trd : C, fth : D) extends TileQuad [A, B, C, D]

object TileQuad {
  def mk [A, B, C, D] (fst : A) (snd : B) (trd : C) (fth : D) : TileQuad [A, B, C, D] =
    TileQuad_ [A, B, C, D] (fst, snd, trd, fth)
}

trait TileMessage [A ]
{

  def   context : RuleSeq
  def   instance : Trajectory
  def   contents : A

}

case class TileMessage_ [A] (context : RuleSeq, instance : Trajectory, contents : A) extends TileMessage [A]

object TileMessage {
  def mk [A] (context : RuleSeq) (instance : Trajectory) (contents : A) : TileMessage [A] =
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

trait InstanceBuilder
{



  def build (configuration : Configuration) : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (configuration .rules) (configuration .trajectory) (true)

}

case class InstanceBuilder_ () extends InstanceBuilder

object InstanceBuilder {
  def mk : InstanceBuilder =
    InstanceBuilder_ ()
}

