package soda.tiles.emotion.parser

import   soda.tiles.emotion.entity.Action
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.Identifier
import   soda.tiles.emotion.entity.Instance
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

  def parse_actions (part : Any) : List [String] =
    ListParser .mk
      .parse_string_list (part) .getOrElse (List () )

  def parse (a : Any) : Option [ActionSet] =
    None

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


trait FluentSetParser
{



  import   scala.jdk.CollectionConverters.CollectionHasAsScala
  import   scala.jdk.CollectionConverters.IteratorHasAsScala
  import   scala.jdk.CollectionConverters.MapHasAsScala

  lazy val identifier = "fluents"

  def parse_fluents (part : Any) : List [Tuple2 [String, List [String] ] ] =
    part match  {
      case p : java.util.Map [Any, Any] =>
        p .asScala .toSeq .flatMap ( kv => ListParser .mk .parse_named_list (kv) ) .toList
      case otherwise => List()
    }

  def parse (a : Any) : Option [FluentSet] =
    None

}

case class FluentSetParser_ () extends FluentSetParser

object FluentSetParser {
  def mk : FluentSetParser =
    FluentSetParser_ ()
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

  def   maybe_fluents : Option [FluentSet]
  def   maybe_actions : Option [ActionSet]
  def   maybe_rules : Option [RuleSeq]
  def   maybe_trajectory : Option [Trajectory]

}

case class PartialConfiguration_ (maybe_fluents : Option [FluentSet], maybe_actions : Option [ActionSet], maybe_rules : Option [RuleSeq], maybe_trajectory : Option [Trajectory]) extends PartialConfiguration

object PartialConfiguration {
  def mk (maybe_fluents : Option [FluentSet]) (maybe_actions : Option [ActionSet]) (maybe_rules : Option [RuleSeq]) (maybe_trajectory : Option [Trajectory]) : PartialConfiguration =
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

  def build_partial_configuration (s : Seq [Any] ) : Option [PartialConfiguration] =
    if ( (s .length == 4)
    )
      Some (
        PartialConfiguration .mk (
          FluentSetParser .mk .parse (s (0) ) ) (
          ActionSetParser .mk .parse (s (1) ) ) (
          RuleSeqParser .mk .parse (s (2) ) ) (
          TrajectoryParser .mk .parse (s (3) )
        )
      )
    else None

  def build_configuration (c : PartialConfiguration) : Option [Configuration] =
    if ( (c .maybe_fluents .isDefined &&
      c .maybe_actions .isDefined &&
      c .maybe_rules .isDefined &&
      c .maybe_trajectory .isDefined)
    )
      Some (
        Configuration .mk (
          c .maybe_fluents .get) (
          c .maybe_actions .get) (
          c .maybe_rules .get) (
          c .maybe_trajectory .get
        )
      )
    else None

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

