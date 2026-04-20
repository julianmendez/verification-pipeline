package soda.tiles.emotion.parser

import   org.scalatest.funsuite.AnyFunSuite
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.AllowsRule
import   soda.tiles.emotion.entity.CausesIfRule
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.ContravenesRule
import   soda.tiles.emotion.entity.DefaultRule
import   soda.tiles.emotion.entity.FacilitatesRule
import   soda.tiles.emotion.entity.FluentName
import   soda.tiles.emotion.entity.FluentValue
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.RuleSeq
import   soda.tiles.emotion.entity.TriggersRule





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
    Set [Action] (
      "perceive_threat",
      "receive_support",
      "achieve_goal",
      "make_error",
      "no_action"
    )

  lazy val rules : RuleSeq = Seq [Rule] (
    CausesIfRule (
      Set [FluentValue] (
        "goal_undecided"
      ) ,
      "perceive_threat" ,
      Set [FluentValue] (
        "goal_high" ,
        "need_high" ,
        "account_environment" ,
        "control_low"
      )
    ) ,
    CausesIfRule (
      Set [FluentValue] (
        "need_high"
      ) ,
      "receive_support" ,
      Set [FluentValue] (
        "need_low" ,
        "account_other" ,
        "control_high"
      )
    ) ,
    CausesIfRule (
      Set [FluentValue] (
        "goal_high"
      ) ,
      "achieve_goal" ,
      Set [FluentValue] (
        "need_low" ,
        "account_self" ,
        "control_high"
      )
    ) ,
    CausesIfRule (
      Set [FluentValue] (
        "goal_high"
      ) ,
      "make_error" ,
      Set [FluentValue] (
        "need_high" ,
        "goal_low" ,
        "account_self" ,
        "control_low"
      )
    ) ,
    FacilitatesRule (
      Set [FluentValue] (
        "control_high"
      ) ,
      "achieve_goal"
    ) ,
    InhibitsRule (
      Set [FluentValue] (
        "control_low"
      ) ,
      "achieve_goal"
    ) ,
    ContravenesRule (
      Set [FluentValue] (
        "account_environment"
      ) ,
      "achieve_goal"
    ) ,
    CausesIfRule (
      Set [FluentValue] () ,
      "no_action" ,
      Set [FluentValue] ()
    ) ,
    IfRule (
      Set [FluentValue] () ,
      Set [FluentValue] ()
    ) ,
    TriggersRule (
      Set [FluentValue] () ,
      "no_action"
    ) ,
    AllowsRule (
      Set [FluentValue] () ,
      "no_action"
    ) ,
    InhibitsRule (
      Set [FluentValue] () ,
      "no_action"
    ) ,
    NoConcurrencyRule (
      Set [Action] ()
    ) ,
    DefaultRule (
      "goal_undecided"
    ) ,
    InfluencesIfRule (
      Set [FluentValue] () ,
      "no_action" ,
      Set [FluentValue] ()
    ) ,
    InfluencesRule (
      Set [FluentValue] () ,
      Set [FluentValue] ()
    ) ,
    FacilitatesRule (
      Set [FluentValue] () ,
      "no_action"
    ) ,
    ContravenesRule (
      Set [FluentValue] () ,
      "no_action"
    ) ,
    ForbidsToCauseRule (
      Set [FluentValue] () ,
      Set [FluentValue] ()
    )
  )

  lazy val trajectory : Seq [IdentifierSet] =
    Seq [IdentifierSet] (
      Set [FluentValue] (
        "need_undecided" ,
        "goal_undecided" ,
        "account_undecided" ,
        "control_undecided"
      ) ,
      Set [Action] (
        "perceive_threat"
      ) ,
      Set [FluentValue] (
        "need_high" ,
        "goal_high" ,
        "account_environment" ,
        "control_low"
      ) ,
      Set [Action] (
        "receive_support"
      ) ,
      Set [FluentValue] (
        "need_low" ,
        "goal_high" ,
        "account_other" ,
        "control_high"
      ) ,
      Set [Action] (
        "achieve_goal"
      ) ,
      Set [FluentValue] (
        "need_low" ,
        "goal_high" ,
        "account_self" ,
        "control_high"
      ) ,
      Set [Action] (
        "make_error"
      ) ,
      Set [FluentValue] (
        "need_high" ,
        "goal_low" ,
        "account_self" ,
        "control_low"
      )
    )

  lazy val instance : Configuration =
    Configuration .mk (fluents) (actions) (rules) (trajectory)

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
    ) .toSet

  lazy val rules : RuleSeq = Seq [Rule] (
    InfluencesIfRule (
      Seq [FluentValue] (
        "goal_low"
      ) .toSet ,
      "commitment" ,
      Seq [FluentValue] (
        "goal_high"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_other"
      ) .toSet ,
      "commitment" ,
      Seq [FluentValue] (
        "account_self"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "need_high"
      ) .toSet ,
      "endorsement" ,
      Seq [FluentValue] (
        "need_undecided"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_high"
      ) .toSet ,
      "endorsement" ,
      Seq [FluentValue] (
        "control_undecided"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_self"
      ) .toSet ,
      "justification" ,
      Seq [FluentValue] (
        "account_environment"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_undecided"
      ) .toSet ,
      "justification" ,
      Seq [FluentValue] (
        "control_low"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_environment"
      ) .toSet ,
      "attribution" ,
      Seq [FluentValue] (
        "account_self"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_low"
      ) .toSet ,
      "attribution" ,
      Seq [FluentValue] (
        "control_undecided"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "account_self"
      ) .toSet ,
      "challenge" ,
      Seq [FluentValue] (
        "account_environment"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_undecided"
      ) .toSet ,
      "challenge" ,
      Seq [FluentValue] (
        "control_low"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "need_undecided"
      ) .toSet ,
      "affirmation" ,
      Seq [FluentValue] (
        "need_high"
      ) .toSet
    ) ,
    InfluencesIfRule (
      Seq [FluentValue] (
        "control_low"
      ) .toSet ,
      "affirmation" ,
      Seq [FluentValue] (
        "control_undecided"
      ) .toSet
    )
  )

  lazy val trajectory : Seq [IdentifierSet] =
    Seq [IdentifierSet] (
      Seq [FluentValue] (
        "need_high" ,
        "goal_low" ,
        "account_other" ,
        "control_high"
      ) .toSet ,
      Seq [Action] (
        "commitment"
      ) .toSet ,
      Seq [FluentValue] (
        "need_high" ,
        "goal_high" ,
        "account_self" ,
        "control_high"
      ) .toSet ,
      Seq [Action] (
        "endorsement"
      ) .toSet ,
      Seq [FluentValue] (
        "need_undecided" ,
        "goal_high" ,
        "account_self" ,
        "control_undecided"
      ) .toSet ,
      Seq [Action] (
        "justification"
      ) .toSet ,
      Seq [FluentValue] (
        "need_undecided" ,
        "goal_high" ,
        "account_environment" ,
        "control_low"
      ) .toSet ,
      Seq [Action] (
        "attribution"
      ) .toSet ,
      Seq [FluentValue] (
        "need_undecided" ,
        "goal_high" ,
        "account_self" ,
        "control_undecided"
      ) .toSet ,
      Seq [Action] (
        "challenge"
      ) .toSet ,
      Seq [FluentValue] (
        "need_undecided" ,
        "goal_high" ,
        "account_environment" ,
        "control_low"
      ) .toSet ,
      Seq [Action] (
        "affirmation"
      ) .toSet ,
      Seq [FluentValue] (
        "need_high" ,
        "goal_high" ,
        "account_environment" ,
        "control_undecided"
      ) .toSet
    )

  lazy val instance : Configuration =
    Configuration .mk (fluents) (actions) (rules) (trajectory)

}

case class Example2Instance_ () extends Example2Instance

object Example2Instance {
  def mk : Example2Instance =
    Example2Instance_ ()
}


case class YamlParserSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  lazy val parser = YamlParser .mk

  lazy val example1_name = "/example/example-1.yaml"

  lazy val example1_contents = read_file (example1_name)

  lazy val example1_parsed_instance = parser .parse ( new StringReader (example1_contents) )

  lazy val example1_instance = Some (Example1Instance .mk .instance)

  lazy val example2_name = "/example/example-2.yaml"

  lazy val example2_contents = read_file (example2_name)

  lazy val example2_parsed_instance = parser .parse ( new StringReader (example2_contents) )

  lazy val example2_instance = Some (Example2Instance .mk .instance)

  test ("parse example 1") (
    check (
      obtained = example1_parsed_instance
    ) (
      expected = example1_instance
    )
  )

  test ("parse example 2") (
    check (
      obtained = example2_parsed_instance
    ) (
      expected = example2_instance
    )
  )

}

