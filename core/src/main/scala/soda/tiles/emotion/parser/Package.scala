package soda.tiles.emotion.parser

import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.IdentifierSet
import   soda.tiles.emotion.entity.Instance
import   soda.tiles.emotion.entity.FluentMap
import   soda.tiles.emotion.entity.FluentName
import   soda.tiles.emotion.entity.FluentSet
import   soda.tiles.emotion.entity.FluentValue
import   soda.tiles.emotion.entity.TileMessage
import   soda.tiles.emotion.entity.Trajectory
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.RuleSeq





trait ActionSetParser
{



  lazy val identifier = "actions"

  def parse_actions_with (part : Any) : Option [ActionSet] =
    part match  {
      case (a , s) =>
        if ( (a == identifier)
        )
          ListParser .mk
            .parse_string_list (s)
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

  def parse (a : Any) : Option [ActionSet] =
    parse_actions (a)

}

case class ActionSetParser_ () extends ActionSetParser

object ActionSetParser {
  def mk : ActionSetParser =
    ActionSetParser_ ()
}


trait ConfigurationParser
{



  def parse (a : Any) : Option [Configuration] =
    None

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
      case s : Seq [FluentValue] =>
        Some (s .map ( elem => (elem , name) ) )
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

  def parse (a : Any) : Option [FluentMap] =
    parse_fluents (a)

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

  def parse_named_list_part (p : Tuple2 [Any, Any] ) : Option [Tuple2 [String, List [String] ] ] =
    p ._1 match  {
      case k : String => parse_string_list (p ._2) .map ( lst => Tuple2 (k , lst) )
      case otherwise => None
    }

  def parse_named_list (part : Any) : Option [Tuple2 [String, List [String] ] ] =
    part match  {
      case p : Tuple2 [Any, Any] => parse_named_list_part (p)
      case otherwise => None
    }

  def parse (a : Seq [Any] ) : Option [List [Any] ] =
    None

}

case class ListParser_ () extends ListParser

object ListParser {
  def mk : ListParser =
    ListParser_ ()
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


trait RuleSeqParser
{



  import   scala.jdk.CollectionConverters.CollectionHasAsScala
  import   scala.jdk.CollectionConverters.IteratorHasAsScala
  import   scala.jdk.CollectionConverters.MapHasAsScala

  lazy val identifier = "rules"

  def parse_tuple_type (rule_type : String) (part : Any) : Option [Tuple2 [String, java.util.Map [Any, Any] ] ] =
    part match  {
      case m : java.util.Map [Any, Any] => Some (Tuple2 (rule_type , m))
      case otherwise => None
    }

  def parse_rule_pair (p : Tuple2 [Any, Any] ) : Option [Tuple2 [String, java.util.Map [Any, Any] ] ] =
    p ._1 match  {
      case rule_type : String => parse_tuple_type (rule_type) (p ._2)
      case otherwise => None
    }

  def parse_rule (part : Any) : Option [Tuple2 [String, java.util.Map [Any, Any] ] ] =
    part match  {
      case p : Tuple2 [Any, Any] => parse_rule_pair (p)
      case otherwise => None
    }

  def parse_rules (part : Any) : List [Tuple2 [String, java.util.Map [Any, Any] ] ] =
    part match  {
      case p : Seq[Any] => p .flatMap ( r => parse_rule (r) ) .toList
      case otherwise => List ()
    }

  def parse (a : Any) : Option [RuleSeq] =
    None

}

case class RuleSeqParser_ () extends RuleSeqParser

object RuleSeqParser {
  def mk : RuleSeqParser =
    RuleSeqParser_ ()
}


trait TrajectoryParser
{



  lazy val identifier = "trajectory"

  def parse_trajectory (part : Any) : List [Any] =
    part match  {
      case p : Seq [Any] => p .toList
      case otherwise => List ()
    }

  def parse (a : Any) : Option [Trajectory] =
    None

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
  import   scala.jdk.CollectionConverters.CollectionHasAsScala
  import   scala.jdk.CollectionConverters.IteratorHasAsScala
  import   scala.jdk.CollectionConverters.MapHasAsScala

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

  def parse (reader : Reader) : Option [Configuration] =
    build_partial_configuration (
      GenericYamlParser .mk
        .parse (reader)
        .toList
    )
    .flatMap ( partial_conf => build_configuration (partial_conf) )

}

case class YamlParser_ () extends YamlParser

object YamlParser {
  def mk : YamlParser =
    YamlParser_ ()
}

