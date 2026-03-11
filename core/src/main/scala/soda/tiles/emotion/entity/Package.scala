package soda.tiles.emotion.entity

/*
 * This package contains classes to model a fairness scenario.
 */





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


