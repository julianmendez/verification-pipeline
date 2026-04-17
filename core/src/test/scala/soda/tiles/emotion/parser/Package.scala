package soda.tiles.emotion.parser

import   org.scalatest.funsuite.AnyFunSuite
import   org.scalatest.Assertion
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.ActionSetType
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.Instance
import   soda.tiles.emotion.entity.Fluent
import   soda.tiles.emotion.entity.FluentName
import   soda.tiles.emotion.entity.FluentOrActionSet
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.FluentSetType
import   soda.tiles.emotion.entity.FluentValue
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.RuleSet

trait Example1Instance
{



  lazy val fluents : Map [FluentValue, FluentName] =
    Map [FluentValue, FluentName] (
       ("need_high" , "need") ,
       ("need_undecided" , "need") ,
       ("need_low" , "need") ,
       ("goal_high" , "goal") ,
       ("goal_undecided" , "goal") ,
       ("goal_low" , "goal") ,
       ("control_high" , "control") ,
       ("control_undecided" , "control") ,
       ("control_low" , "control") ,
       ("account_self" , "accountability") ,
       ("account_other" , "accountability") ,
       ("account_environment" , "accountability") ,
       ("account_undecided" , "accountability")
     )

  lazy val actions : ActionSet =
    Seq [Action] (
      "perceive_threat",
      "receive_support",
      "achieve_goal",
      "make_error",
      "no_action"
    )

  lazy val rules : RuleSet = Seq [Rule] (
    CausesIfRule (
      Seq [FluentValue] (
        "goal_undecided"
      ) ,
      "perceive_threat" ,
      Seq [FluentValue] (
        "goal_high" ,
        "need_high" ,
        "account_environment" ,
        "control_low"
      )
    ) ,
    CausesIfRule (
      Seq [FluentValue] (
        "goal_undecided"
      ) ,
      "perceive_threat" ,
      Seq [FluentValue] (
        "goal_high" ,
        "need_high" ,
        "account_environment" ,
        "control_low"
      )
    ) ,
    CausesIfRule (
      Seq [FluentValue] (
        "need_high"
      ) ,
      "receive_support" ,
      Seq [FluentValue] (
        "need_low" ,
        "account_other" ,
        "control_high"
      )
    ) ,
    CausesIfRule (
      Seq [FluentValue] (
        "goal_high"
      ) ,
      "achieve_goal" ,
      Seq [FluentValue] (
        "need_low" ,
        "account_self" ,
        "control_high"
      )
    ) ,
    CausesIfRule (
      Seq [FluentValue] (
        "goal_high"
      ) ,
      "make_error" ,
      Seq [FluentValue] (
        "need_high" ,
        "goal_low" ,
        "account_self" ,
        "control_low"
      )
    ) ,
    FacilitatesRule (
      Seq [FluentValue] (
        "control_high"
      ) ,
      "achieve_goal"
    ) ,
    InhibitsRule (
      Seq [FluentValue] (
        "control_low"
      ) ,
      "achieve_goal"
    ) ,
    ContravenesRule (
      Seq [FluentValue] (
        "account_environment"
      ) ,
      "achieve_goal"
    ) ,
    CausesIfRule (
      Seq [FluentValue] () ,
      "no_action" ,
      Seq [FluentValue] ()
    ) ,
    IfRule (
      Seq [FluentValue] () ,
      Seq [FluentValue] ()
    ) ,
    TriggersRule (
      Seq [FluentValue] () ,
      "no_action"
    ) ,
    AllowsRule (
      Seq [FluentValue] () ,
      "no_action"
    ) ,
    InhibitsRule (
      Seq [FluentValue] () ,
      "no_action"
    ) ,
    NoConcurrencyRule (
      Seq [Action] ()
    ) ,
    DefaultRule (
      "goal_undecided"
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] () ,
      "no_action" ,
      Seq [FluentValue] ()
    ) ,
    InfluencesRule (
      Seq [FluentValue] () ,
      Seq [FluentValue] ()
    ) ,
    FacilitatesRule (
      Seq [FluentValue] () ,
      "no_action"
    ) ,
    ContravenesRule (
      Seq [FluentValue] () ,
      "no_action"
    ) ,
    ForbidsToCauseRule (
      Seq [FluentValue] () ,
      Seq [FluentValue] ()
    )
  )

  lazy val trajectory : Seq [IdentifierSet] =
    Seq [IdentifierSet] (
      Seq [FluentValue] (
        "need_undecided" ,
        "goal_undecided" ,
        "account_undecided" ,
        "control_undecided"
      ) ,
      Seq [Action] (
        "perceive_threat"
      ) ,
      Seq [FluentValue] (
        "need_high" ,
        "goal_high" ,
        "account_environment" ,
        "control_low"
      ) ,
      Seq [Action] (
        "receive_support"
      ) ,
      Seq [FluentValue] (
        "need_low" ,
        "goal_high" ,
        "account_other" ,
        "control_high"
      ) ,
      Seq [Action] (
        "achieve_goal"
      ) ,
      Seq [FluentValue] (
        "need_low" ,
        "goal_high" ,
        "account_self" ,
        "control_high"
      ) ,
      Seq [Action] (
        "make_error"
      ) ,
      Seq [FluentValue] (
        "need_high" ,
        "goal_low" ,
        "account_self" ,
        "control_low"
      )
    )

}

case class Example1Instance_ () extends Example1Instance

object Example1Instance {
  def mk : Example1Instance =
    Example1Instance_ ()
}


trait Example2Instance
{



  lazy val fluents : Map [FluentValue, FluentName] =
    Map [FluentValue, FluentName] (
       ("need_high" , "need") ,
       ("need_undecided" , "need") ,
       ("need_low" , "need") ,
       ("goal_high" , "goal") ,
       ("goal_undecided" , "goal") ,
       ("goal_low" , "goal") ,
       ("control_high" , "control") ,
       ("control_undecided" , "control") ,
       ("control_low" , "control") ,
       ("account_self" , "accountability") ,
       ("account_other" , "accountability") ,
       ("account_environment" , "accountability") ,
       ("account_undecided" , "accountability")
     )

  lazy val actions : ActionSet =
    Seq [Action] (
      "endorsement" ,
      "attribution" ,
      "affirmation" ,
      "commitment" ,
      "justification" ,
      "challenge"
    )

  lazy val rules : Seq [Rule] = Seq [Rule] (
    InfluencesIfRule (
      Seq [FluentValue] (
        "goal_low"
      ) ,
      "commitment" ,
      Seq [FluentValue] (
        "goal_high"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_other"
      ) ,
      "commitment" ,
      Seq [FluentValue] (
        "account_self"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "need_high"
      ) ,
      "endorsement" ,
      Seq [FluentValue] (
        "need_undecided"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_high"
      ) ,
      "endorsement" ,
      Seq [FluentValue] (
        "control_undecided"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_self"
      ) ,
      "justification" ,
      Seq [FluentValue] (
        "account_environment"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_undecided"
      ) ,
      "justification" ,
      Seq [FluentValue] (
        "control_low"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_environment"
      ) ,
      "attribution" ,
      Seq [FluentValue] (
        "account_self"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_low"
      ) ,
      "attribution" ,
      Seq [FluentValue] (
        "control_undecided"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_self"
      ) ,
      "challenge" ,
      Seq [FluentValue] (
        "account_environment"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_undecided"
      ) ,
      "challenge" ,
      Seq [FluentValue] (
        "control_low"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "need_undecided"
      ) ,
      "affirmation" ,
      Seq [FluentValue] (
        "need_high"
      )
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_low"
      ) ,
      "affirmation" ,
      Seq [FluentValue] (
        "control_undecided"
      )
    )

lazy val trajectory : Seq [IdentifierSet] =
  Seq [IdentifierSet] (
    Seq [FluentValue] (
      "need_high" ,
      "goal_low" ,
      "account_other" ,
      "control_high"
    ) ,
    Seq [Action] (
      "commitment"
    ) ,
    Seq [FluentValue] (
      "need_high" ,
      "goal_high" ,
      "account_self" ,
      "control_high"
    ) ,
    Seq [Action] (
      "endorsement"
    ) ,
    Seq [FluentValue] (
      "need_undecided" ,
      "goal_high" ,
      "account_self" ,
      "control_undecided"
    ) ,
    Seq [Action] (
      "justification"
    ) ,
    Seq [FluentValue] (
      "need_undecided" ,
      "goal_high" ,
      "account_environment" ,
      "control_low"
    ) ,
    Seq [Action] (
      "attribution"
    ) ,
    Seq [FluentValue] (
      "need_undecided" ,
      "goal_high" ,
      "account_self" ,
      "control_undecided"
    ) ,
    Seq [Action] (
      "challenge"
    ) ,
    Seq [FluentValue] (
      "need_undecided" ,
      "goal_high" ,
      "account_environment" ,
      "control_low"
    ) ,
    Seq [Action] (
      "affirmation"
    ) ,
    Seq [FluentValue] (
      "need_high" ,
      "goal_high" ,
      "account_environment" ,
      "control_undecided"
    )
  )

}

case class Example2Instance_ () extends Example2Instance

object Example2Instance {
  def mk : Example2Instance =
    Example2Instance_ ()
}

