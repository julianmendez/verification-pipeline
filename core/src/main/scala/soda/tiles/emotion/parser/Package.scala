package soda.tiles.emotion.parser

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
import   soda.tiles.emotion.entity.FluentValue
import   soda.tiles.emotion.entity.ForbidsToCauseRule
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.IfRule
import   soda.tiles.emotion.entity.InfluencesIfRule
import   soda.tiles.emotion.entity.InfluencesRule
import   soda.tiles.emotion.entity.InhibitsRule
import   soda.tiles.emotion.entity.NoConcurrencyRule
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.RuleSeq
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.TriggersRule





trait ActionSetParser
{



  lazy val identifier = "actions"

  def parse_actions_with (part : Any) : Option [ActionSet] =
    part match  {
      case (a , s) =>
        if ( (a == identifier)
        )
          ListParser .mk
            .parse (s)
            .map ( x => x .toSet)
        else None
      case otherwise =>
        None
    }

  def parse_actions (part : Any) : Option [ActionSet] =
    part match  {
      case s : Seq [Any] =>
        if ( s .isEmpty
        ) None
        else parse_actions_with (s (0) )
      case otherwise => None
    }

  def parse (part : Any) : Option [ActionSet] =
    parse_actions (part)

}

case class ActionSetParser_ () extends ActionSetParser

object ActionSetParser {
  def mk : ActionSetParser =
    ActionSetParser_ ()
}




trait PartialConfiguration
{

  def   maybe_fluents : Option [FluentMap]
  def   maybe_actions : Option [ActionSet]
  def   maybe_rules : Option [RuleSeq]
  def   maybe_trajectory : Option [Trajectory]

}

case class PartialConfiguration_ (maybe_fluents : Option [FluentMap], maybe_actions : Option [ActionSet], maybe_rules : Option [RuleSeq], maybe_trajectory : Option [Trajectory]) extends PartialConfiguration

object PartialConfiguration {
  def mk (maybe_fluents : Option [FluentMap]) (maybe_actions : Option [ActionSet]) (maybe_rules : Option [RuleSeq]) (maybe_trajectory : Option [Trajectory]) : PartialConfiguration =
    PartialConfiguration_ (maybe_fluents, maybe_actions, maybe_rules, maybe_trajectory)
}

trait ConfigurationParser
{



  lazy val empty_fluents : FluentMap =
    Map [FluentValue, FluentName] ()

  lazy val empty_actions : ActionSet =
    Set [Action] ()

  lazy val empty_rules : Seq [Rule] =
    Seq [Rule] ()

  lazy val empty_trajectory : Seq [IdentifierSet] =
    Seq [IdentifierSet] ()

  def build_partial_configuration_with (elem : Any) : Option [PartialConfiguration] =
    elem match  {
      case s : Seq [Any] =>
        if ( (s .length == 4)
        )
          Some (
            PartialConfiguration .mk (
              FluentMapParser .mk .parse (s (0) ) ) (
              ActionSetParser .mk .parse (s (1) ) ) (
              RuleSeqParser .mk .parse (s (2) ) ) (
              TrajectoryParser .mk .parse (s (3) )
            )
          )
        else None
      case otherwise => None
    }

  def build_partial_configuration (s : Seq [Any] ) : Option [PartialConfiguration] =
    if ( (s .length == 1)
    ) build_partial_configuration_with (s (0) )
    else None

  def build_configuration (c : PartialConfiguration) : Option [Configuration] =
    Some (
      Configuration .mk (
        c .maybe_fluents .getOrElse (empty_fluents) ) (
        c .maybe_actions .getOrElse (empty_actions) ) (
        c .maybe_rules .getOrElse (empty_rules) ) (
        c .maybe_trajectory .getOrElse (empty_trajectory)
      )
    )

  def parse_configuration (part : Any) : Option [Configuration] =
    part match  {
      case s : Seq [Any] =>
        build_partial_configuration (s)
          .flatMap ( partial_conf => build_configuration (partial_conf) )
      case otherwise => None
    }

  def parse (part : Any) : Option [Configuration] =
    parse_configuration (part)

}

case class ConfigurationParser_ () extends ConfigurationParser

object ConfigurationParser {
  def mk : ConfigurationParser =
    ConfigurationParser_ ()
}


trait FluentMapParser
{



  lazy val identifier = "fluents"

  lazy val keyword_fluent = "fluent"

  lazy val keyword_values = "values"

  def parse_one_fluent_with_values (name : FluentName) (part : Any) : Option [Seq [Tuple2 [FluentValue, FluentName] ] ] =
    part match  {
      case s : Seq [Any] =>
        Some (s .map ( elem => (elem .toString , name) ) )
      case otherwise => None
    }

  def parse_one_fluent_with (name : FluentName) (part : Any) : Option [Seq [Tuple2 [FluentValue, FluentName] ] ] =
    part match  {
      case (key , value) =>
        if ( (key == keyword_values)
        ) parse_one_fluent_with_values (name) (value)
        else None
      case otherwise => None
    }

  def parse_one_fluent (first : Any) (second : Any) : Option [Seq [Tuple2 [FluentValue, FluentName] ] ] =
    first match  {
      case (key , value) =>
        if ( (key == keyword_fluent)
        ) parse_one_fluent_with (value .toString) (second)
        else None
      case otherwise => None
    }

  def parse_sequence_with_elem (s : Seq [Any] ) : Option [Seq [Tuple2 [FluentValue, FluentName] ] ] =
    if ( (s .length == 2)
    ) parse_one_fluent (s (0) )  (s (1) )
    else None

  def parse_sequence_with (part : Any) : Option [Seq [Tuple2 [FluentValue, FluentName] ] ] =
    part match  {
      case s : Seq [Any] => parse_sequence_with_elem (s)
      case otherwise => None
    }

  def convert_to_map (s : Seq [Option [Seq [Tuple2 [FluentValue, FluentName] ] ] ] ) : Option [FluentMap] =
    if ( (s .contains (None) )
    ) None
    else Some(s .flatten .flatten .toMap)

  def parse_sequence (part : Any) : Option [FluentMap] =
    part match  {
      case s : Seq [Any] =>
        convert_to_map (s .map ( pair =>  parse_sequence_with (pair) ) )
      case otherwise => None
    }

  def parse_fluents_with (part : Any) : Option [FluentMap] =
    part match  {
      case (a , s) =>
        if ( (a == identifier)
        ) parse_sequence (s)
        else None
      case otherwise => None
    }

  def parse_fluents (part : Any) : Option [FluentMap] =
    part match  {
      case s : Seq [Any] =>
        if ( s .isEmpty
        ) None
        else parse_fluents_with (s (0) )
      case otherwise => None
    }

  def parse (part : Any) : Option [FluentMap] =
    parse_fluents (part)

}

case class FluentMapParser_ () extends FluentMapParser

object FluentMapParser {
  def mk : FluentMapParser =
    FluentMapParser_ ()
}


/**
 * This is a generic YAML parser.
 * This parser converts all the Integer objects into String objects.
 */

trait GenericYamlParser
{



  import   org.snakeyaml.engine.v2.api.Load
  import   org.snakeyaml.engine.v2.api.LoadSettings
  import   java.io.Reader
  import   scala.jdk.CollectionConverters.CollectionHasAsScala
  import   scala.jdk.CollectionConverters.IteratorHasAsScala
  import   scala.jdk.CollectionConverters.MapHasAsScala

  lazy val code_point_limit : Int = 1024 * 1024 * 1024

  /**
   * Converts Java data structures into immutable Scala data structures .
   * Integer objects are converted into String objects .
   *
   * @param obj object
   * @return the object with immutable Scala data structures
   */

  private def _rec_as_scala (obj : Any) : Option [Any] =
    obj match  {
      case x : String => Some (x)
      case x : Integer => Some (x .toString)
      case x : Tuple2 [Any, Any] =>
        _rec_as_scala (x ._1)
          .flatMap ( a =>
            _rec_as_scala (x ._2)
              .map ( b =>
                Tuple2 (a , b)
              )
          )
      case x : java.util.Map [Any, Any] =>
        Some (x .asScala
          .flatMap ( elem => _rec_as_scala (elem) )
          .toSeq
        )
      case x : java.util.Collection [Any] =>
        Some (x .asScala
          .flatMap ( elem => _rec_as_scala (elem) )
          .toSeq
        )
      case otherwise => None
    }

  /**
   * Parses a YAML document.
   *
   * @param reader reader
   * @return a structure with the YAML document.
   */

  def parse (reader : Reader) : Seq [Any] =
    ( new Load (
        LoadSettings
          .builder ()
          .setCodePointLimit (code_point_limit)
          .build ()
        )
      )
      .loadAllFromReader (reader)
      .iterator ()
      .asScala
      .toSeq
      .flatMap ( x => _rec_as_scala (x) )

}

case class GenericYamlParser_ () extends GenericYamlParser

object GenericYamlParser {
  def mk : GenericYamlParser =
    GenericYamlParser_ ()
}


trait ListParser
{



  def parse_string_part (e : Any) : Option [String] =
    e match  {
      case s : String => Some (s)
      case otherwise => None
    }

  def parse_string_list (part : Any) : Option [List [String] ] =
    part match  {
      case p : Seq [Any] =>
        Some (p .flatMap ( e => parse_string_part (e) ) .toList)
      case otherwise => None
    }

  def parse (part : Any) : Option [List [String] ] =
    parse_string_list (part)

}

case class ListParser_ () extends ListParser

object ListParser {
  def mk : ListParser =
    ListParser_ ()
}


trait RuleParser
{



  lazy val identifier = "rule"

  lazy val keyword_rule = "rule"

  lazy val keyword_input_fluent = "input_fluent"

  lazy val keyword_input = "input"

  lazy val keyword_output = "output"

  lazy val keyword_action = "action"

  lazy val keyword_actions = "actions"

  def parse_set_of_strings (part : Any) : Option [Set [String] ] =
    part match  {
      case s : Seq [Any] =>
        Some (
          s .map ( elem => elem .toString)
            .toSet
        )
      case otherwise => None
    }

  def parse_fluents (part : Any) : Option [FluentSet] =
    parse_set_of_strings (part)

  def parse_actions_with (part : Any) : Option [FluentSet] =
    parse_set_of_strings (part)

  def parse_fluent (part : Any) : Option [FluentValue] =
    part match  {
      case ("input_fluent" , value) => Some (value .toString)
      case otherwise => None
    }

  def parse_input (part : Any) : Option [FluentSet] =
    part match  {
      case ("input" , value) => parse_fluents (value)
      case otherwise => None
    }

  def parse_output (part : Any) : Option [FluentSet] =
    part match  {
      case ("output" , value) => parse_fluents (value)
      case otherwise => None
    }

  def parse_action (part : Any) : Option [Action] =
    part match  {
      case ("action" , value) => Some (value .toString)
      case otherwise => None
    }

  def parse_actions (part : Any) : Option [ActionSet] =
    part match  {
      case ("actions" , value) => parse_actions_with (value)
      case otherwise => None
    }

  def parse_CausesIfRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) (maybe_output : Option [FluentSet] ) : Option [CausesIfRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined && maybe_output .isDefined)
    ) Some (CausesIfRule (maybe_input .get , maybe_action .get , maybe_output .get) )
    else None

  def parse_IfRule (maybe_input : Option [FluentSet] ) (maybe_output : Option [FluentSet] ) : Option [IfRule] =
    if ( (maybe_input .isDefined && maybe_output .isDefined)
    ) Some (IfRule (maybe_input .get , maybe_output .get) )
    else None

  def parse_TriggersRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) : Option [TriggersRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined)
    ) Some (TriggersRule (maybe_input .get , maybe_action .get) )
    else None

  def parse_AllowsRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) : Option [AllowsRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined)
    ) Some (AllowsRule (maybe_input .get , maybe_action .get) )
    else None

  def parse_InhibitsRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) : Option [InhibitsRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined)
    ) Some (InhibitsRule (maybe_input .get , maybe_action .get) )
    else None

  def parse_NoConcurrencyRule (maybe_actions : Option [ActionSet] ) : Option [NoConcurrencyRule] =
    if ( (maybe_actions .isDefined)
    ) Some (NoConcurrencyRule (maybe_actions .get) )
    else None

  def parse_DefaultRule (maybe_fluent : Option [FluentValue] ) : Option [DefaultRule] =
    if ( (maybe_fluent .isDefined)
    ) Some (DefaultRule (maybe_fluent .get) )
    else None

  def parse_InfluencesIfRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) (maybe_output : Option [FluentSet] ) : Option [InfluencesIfRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined && maybe_output .isDefined)
    ) Some (InfluencesIfRule (maybe_input .get , maybe_action .get , maybe_output .get) )
    else None

  def parse_InfluencesRule (maybe_input : Option [FluentSet] ) (maybe_output : Option [FluentSet] ) : Option [InfluencesRule] =
    if ( (maybe_input .isDefined && maybe_output .isDefined)
    ) Some (InfluencesRule (maybe_input .get , maybe_output .get) )
    else None

  def parse_FacilitatesRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) : Option [FacilitatesRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined)
    ) Some (FacilitatesRule (maybe_input .get , maybe_action .get) )
    else None

  def parse_ContravenesRule (maybe_input : Option [FluentSet] ) (maybe_action : Option [Action] ) : Option [ContravenesRule] =
    if ( (maybe_input .isDefined && maybe_action .isDefined)
    ) Some (ContravenesRule (maybe_input .get , maybe_action .get) )
    else None

  def parse_ForbidsToCauseRule (maybe_input : Option [FluentSet] ) (maybe_output : Option [FluentSet] ) : Option [ForbidsToCauseRule] =
    if ( (maybe_input .isDefined && maybe_output .isDefined)
    ) Some (ForbidsToCauseRule (maybe_input .get , maybe_output .get) )
    else None

  def parse_rule_for_name (rule_name : Identifier) (s : Seq [Any] ) : Option [Rule] =
    rule_name match  {
      case "CausesIfRule" =>
         if ( (s .length == 4)
         ) parse_CausesIfRule (parse_input (s (1) ) ) (parse_action (s (2) ) ) (parse_output (s (3) ) )
         else None
      case "IfRule" =>
         if ( (s .length == 3)
         ) parse_IfRule (parse_input (s (1) ) ) (parse_output (s (2) ) )
         else None
      case "TriggersRule" =>
         if ( (s .length == 3)
         ) parse_TriggersRule (parse_input (s (1) ) ) (parse_action (s (2) ) )
         else None
      case "AllowsRule" =>
         if ( (s .length == 3)
         ) parse_AllowsRule (parse_input (s (1) ) ) (parse_action (s (2) ) )
         else None
      case "InhibitsRule" =>
         if ( (s .length == 3)
         ) parse_InhibitsRule (parse_input (s (1) ) ) (parse_action (s (2) ) )
         else None
      case "NoConcurrencyRule" =>
         if ( (s .length == 2)
         ) parse_NoConcurrencyRule (parse_actions (s (1) ) )
         else None
      case "DefaultRule" =>
         if ( (s .length == 2)
         ) parse_DefaultRule (parse_fluent (s (1) ) )
         else None
      case "InfluencesIfRule" =>
         if ( (s .length == 4)
         ) parse_InfluencesIfRule (parse_input (s (1) ) ) (parse_action (s (2) ) ) (parse_output (s (3) ) )
         else None
      case "InfluencesRule" =>
         if ( (s .length == 3)
         ) parse_InfluencesRule (parse_input (s (1) ) ) (parse_output (s (2) ) )
         else None
      case "FacilitatesRule" =>
         if ( (s .length == 3)
         ) parse_FacilitatesRule (parse_input (s (1) ) ) (parse_action (s (2) ) )
         else None
      case "ContravenesRule" =>
         if ( (s .length == 3)
         ) parse_ContravenesRule (parse_input (s (1) ) ) (parse_action (s (2) ) )
         else None
      case "ForbidsToCauseRule" =>
         if ( (s .length == 3)
         ) parse_ForbidsToCauseRule (parse_input (s (1) ) ) (parse_output (s (2) ) )
         else None
      case otherwise => None
    }

  def parse_rule_with (first : Any) (s : Seq [Any] ) : Option [Rule] =
    first match  {
      case (name , rule_name) =>
        if ( (name == keyword_rule)
        ) parse_rule_for_name (rule_name .toString) (s)
        else None
      case otherwise => None
    }

  def parse_rule (part : Any) : Option [Rule] =
    part match  {
      case s : Seq [Any] =>
        if ( (s .isEmpty)
        ) None
        else parse_rule_with (s (0) ) (s)
      case otherwise => None
    }

  def parse (part : Any) : Option [Rule] =
    parse_rule (part)

}

case class RuleParser_ () extends RuleParser

object RuleParser {
  def mk : RuleParser =
    RuleParser_ ()
}


trait RuleSeqParser
{



  lazy val identifier = "rules"

  lazy val rule_parser : RuleParser = RuleParser .mk

  def convert_to_map (s : Seq [Option [Rule] ] ) : Option [RuleSeq] =
    if ( (s .contains (None) )
    ) None
    else Some (s .flatten)

  def parse_sequence (part : Any) : Option [RuleSeq] =
    part match  {
      case s : Seq [Any] =>
        convert_to_map (s .map ( elem => rule_parser .parse (elem) ) )
      case otherwise => None
    }

  def parse_rules_with (part : Any)  : Option [RuleSeq] =
    part match  {
      case (a , s) =>
        if ( (a == identifier)
        ) parse_sequence (s)
        else None
      case otherwise => None
    }

  def parse_rules (part : Any)  : Option [RuleSeq] =
    part match  {
      case s : Seq [Any] =>
        if ( s .isEmpty
        ) None
        else parse_rules_with (s (0) )
      case otherwise => None
    }

  def parse (part : Any) : Option [RuleSeq] =
    parse_rules (part)

}

case class RuleSeqParser_ () extends RuleSeqParser

object RuleSeqParser {
  def mk : RuleSeqParser =
    RuleSeqParser_ ()
}


trait TrajectoryParser
{



  lazy val identifier = "trajectory"

  def parse_set_of_strings (part : Any) : Option [IdentifierSet] =
    part match  {
      case s : Seq [Any] =>
        Some (
          s .map ( elem => elem .toString)
            .toSet
        )
      case otherwise => None
    }

  def parse_state_of_action_with (part : Any) : Option [IdentifierSet] =
    part match  {
      case ("state" , set) => parse_set_of_strings (set)
      case ("actions" , set) => parse_set_of_strings (set)
      case otherwise => None
    }

  def parse_state_or_action (part : Any) : Option [IdentifierSet] =
    part match  {
      case s : Seq [Any] =>
        if ( (s .length == 1)
        ) parse_state_of_action_with (s (0) )
        else None
      case otherwise => None
    }

  def convert_to_seq (s : Seq [Option [IdentifierSet] ] ) : Option [Trajectory] =
    if ( (s .contains (None) )
    ) None
    else Some (s .flatten)

  def parse_sequence (part : Any) : Option [Trajectory] =
    part match  {
      case s : Seq [Any] =>
        convert_to_seq (s .map ( elem => parse_state_or_action (elem) ) )
      case otherwise => None
    }

  def parse_trajectory_with (part : Any)  : Option [Trajectory] =
    part match  {
      case (a , s) =>
        if ( (a == identifier)
        ) parse_sequence (s)
        else None
      case otherwise => None
    }

  def parse_trajectory (part : Any)  : Option [Trajectory] =
    part match  {
      case s : Seq [Any] =>
        if ( s .isEmpty
        ) None
        else parse_trajectory_with (s (0) )
      case otherwise => None
    }

  def parse (part : Any) : Option [Trajectory] =
    parse_trajectory (part)

}

case class TrajectoryParser_ () extends TrajectoryParser

object TrajectoryParser {
  def mk : TrajectoryParser =
    TrajectoryParser_ ()
}


/**
 * Parser for fluents, actions, rules, and trajectory.
 */

trait YamlParser
{



  import   java.io.Reader

  def parse (reader : Reader) : Option [Configuration] =
    ConfigurationParser .mk
      .parse(
        GenericYamlParser .mk
          .parse (reader)
      )

}

case class YamlParser_ () extends YamlParser

object YamlParser {
  def mk : YamlParser =
    YamlParser_ ()
}

