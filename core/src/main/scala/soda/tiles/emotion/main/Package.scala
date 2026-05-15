package soda.tiles.emotion.main

/*
 * This package is for the main class.
 * This is the entry point when the application is executed from a terminal.
 */

import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   scala.util.Try
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.InstanceBuilder
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.parser.YamlParser
import   soda.tiles.emotion.pipeline.EmotionalReasoningPipeline
import   soda.tiles.emotion.validator.ConfigurationValidator

trait FinalReport
{

  def   transitions : Seq [TransitionReport]
  def   errors : Seq [String]

}

case class FinalReport_ (transitions : Seq [TransitionReport], errors : Seq [String]) extends FinalReport

object FinalReport {
  def mk (transitions : Seq [TransitionReport]) (errors : Seq [String]) : FinalReport =
    FinalReport_ (transitions, errors)
}


trait InstanceProcessor
{



  lazy val error_undefined_result = "undefined result because the input instance is invalid"

  lazy val error_configuration_is_undefined = "the input instance is invalid or the input file cannot be read"

  def get_transition_index (test_index : Int) (rule_set_size : Int) : Int =
    if ( (rule_set_size > 0)
    ) test_index / rule_set_size
    else 0

  def process_configuration (configuration : Configuration) : Seq [TransitionReport] =
    EmotionalReasoningPipeline .mk
      .run (InstanceBuilder .mk .build (configuration) )
      .contents
      .zipWithIndex
      .map ( x =>
        TransitionReport .mk (x ._2) (get_transition_index (x ._2) (configuration .rules .size) ) (
          x ._1 .fst) (x ._1 .snd) (x ._1 .trd) (x ._1 .fth)
      )

  def process_instance_with (conf : Configuration) (errors : Seq [String] ) : FinalReport =
    if ( errors .isEmpty
    ) FinalReport .mk (process_configuration (conf) ) (errors)
    else FinalReport .mk (Seq .empty) (errors)

  def process_instance (maybe_conf : Option [Configuration] ) : FinalReport =
    maybe_conf match  {
      case Some (conf) => process_instance_with (conf) (ConfigurationValidator .mk .validate (conf) )
      case None => FinalReport .mk (Seq .empty) (Seq [String] (error_configuration_is_undefined) )
    }

}

case class InstanceProcessor_ () extends InstanceProcessor

object InstanceProcessor {
  def mk : InstanceProcessor =
    InstanceProcessor_ ()
}


/**
 * This is the main entry point.
 */

trait Main
{



  lazy val help = "" +
    "\nEmotional Reasoning" +
    "\n" +
    "\nCopyright 2026 Julian Alfredo Mendez" +
    "\nhttps://julianmendez.github.io/emotional-reasoning" +
    "\n" +
    "\nEmotional Reasoning is a project that instantiates the Tiles framework." +
    "\n" +
    "\nParameter: FILE_NAME" +
    "\n" +
    "\n  FILE_NAME     YAML file containing the instance" +
    "\n" +
    "\n"

  def try_read_file (file_name : String) : Try [String] =
    Try [String] (
      new String (Files .readAllBytes (Paths .get (file_name) ) )
    )

  def get_maybe_configuration (maybe_content : Try [String] ) : Option [Configuration] =
    if ( maybe_content .isSuccess
    ) YamlParser .mk .parse ( new StringReader (maybe_content .get) )
    else None

  def process_input_file (file_name : String) : String =
    Serializer .mk .serialize (
      InstanceProcessor .mk .process_instance (
        get_maybe_configuration (
          try_read_file (file_name)
        )
      )
    )

  def execute (arguments : List [String] ) : Unit =
    if ( (arguments .length > 0)
    ) println (process_input_file (arguments (0) ) )
    else println (help)

  def main (arguments : Array [String] ) : Unit =
    execute (arguments .toList)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}


trait Serializer
{



  lazy val error_parsing_error = "parsing error possibly caused by a misspelled rule name or YAML key"

  def serialize_transition (entry : TransitionReport) : String =
    "  - test_index: " + entry .test_index + "\n" +
    "    transition_index: " + entry .transition_index + "\n" +
    "    transition: " + entry .transition + "\n" +
    "    rule: " + entry .rule + "\n" +
    "    inhibiting_actions: " + entry .inhibiting_actions + "\n" +
    "    valid: " + entry .valid + "\n"

  def serialize_all_transitions (transitions : Seq [TransitionReport] ) :String =
    if ( (transitions .nonEmpty)
    )
      "- all_transitions:" + "\n" +
        transitions
          .map ( x => serialize_transition (x) )
          .mkString
    else ""

  def serialize_invalid_transitions (transitions : Seq [TransitionReport] ) :String =
    if ( (transitions
      .filter ( x => ! x .valid)
      .nonEmpty)
    )
      "- invalid_transitions:" + "\n" +
        transitions
          .filter ( x => ! x .valid)
          .map ( x => serialize_transition (x) )
          .mkString
    else ""

  def serialize_errors (errors : Seq [String] ) : String =
    if ( (errors .nonEmpty)
    )
      "- errors:" + "\n" +
        (errors
          .map ( x => "  - " + x)
          .mkString ("\n")
        ) + "\n"
    else ""

  def serialize (report : FinalReport) : String =
    "---" + "\n" +
    serialize_errors (report .errors) +
    serialize_invalid_transitions (report .transitions) +
    serialize_all_transitions (report .transitions)

}

case class Serializer_ () extends Serializer

object Serializer {
  def mk : Serializer =
    Serializer_ ()
}


trait TransitionReport
{

  def   test_index : Int
  def   transition_index : Int
  def   transition : Transition
  def   rule : Rule
  def   inhibiting_actions : ActionSet
  def   valid : Boolean

}

case class TransitionReport_ (test_index : Int, transition_index : Int, transition : Transition, rule : Rule, inhibiting_actions : ActionSet, valid : Boolean) extends TransitionReport

object TransitionReport {
  def mk (test_index : Int) (transition_index : Int) (transition : Transition) (rule : Rule) (inhibiting_actions : ActionSet) (valid : Boolean) : TransitionReport =
    TransitionReport_ (test_index, transition_index, transition, rule, inhibiting_actions, valid)
}

