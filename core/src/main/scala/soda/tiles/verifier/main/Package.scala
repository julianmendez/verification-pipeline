package soda.tiles.verifier.main

/*
 * This package is for the main class.
 * This is the entry point when the application is executed from a terminal.
 */

import   java.io.StringReader
import   java.io.PrintWriter
import   java.io.Writer
import   scala.util.Try
import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.Configuration
import   soda.tiles.verifier.entity.InstanceBuilder
import   soda.tiles.verifier.entity.Rule
import   soda.tiles.verifier.entity.Trajectory
import   soda.tiles.verifier.entity.Transition
import   soda.tiles.verifier.io.SimpleFileReader
import   soda.tiles.verifier.parser.YamlParser
import   soda.tiles.verifier.pipeline.VerificationPipeline
import   soda.tiles.verifier.validator.ConfigurationValidator

trait FinalReport
{

  def   iterations : Int
  def   transitions : Seq [TransitionReport]
  def   errors : Seq [String]
  def   reading_time : Long
  def   validation_time : Long
  def   execution_time : Long

}

case class FinalReport_ (iterations : Int, transitions : Seq [TransitionReport], errors : Seq [String], reading_time : Long, validation_time : Long, execution_time : Long) extends FinalReport

object FinalReport {
  def mk (iterations : Int) (transitions : Seq [TransitionReport]) (errors : Seq [String]) (reading_time : Long) (validation_time : Long) (execution_time : Long) : FinalReport =
    FinalReport_ (iterations, transitions, errors, reading_time, validation_time, execution_time)
}


trait InstanceProcessor
{



  lazy val error_undefined_result = "undefined result because the input instance is invalid"

  lazy val error_configuration_is_undefined = "the input file cannot be read or the instance read is invalid"

  lazy val error_processing_instance = "error processing instance maybe due to a misspelled keyword"

  def get_extended_trajectory (trajectory : Trajectory) (iterations : Int) : Trajectory =
    if ( (trajectory .size >= 3) && (! (trajectory .size % 2 == 0) )
    )
      Seq
        .fill (iterations) (trajectory .tail)
        .iterator
        .flatten
        .toSeq
        .+: (trajectory .head)
    else
      trajectory

  def get_extended_configuration (conf : Configuration) (iterations : Int) : Configuration =
    Configuration .mk (conf .fluents) (conf .actions) (conf .rules) (
      get_extended_trajectory (conf .trajectory) (iterations)
    )

  def get_transition_index (test_index : Int) (rule_set_size : Int) : Int =
    if ( (rule_set_size > 0)
    ) test_index / rule_set_size
    else 0

  def process_configuration (configuration : Configuration) : Seq [TransitionReport] =
    VerificationPipeline .mk
      .run (InstanceBuilder .mk .build (configuration) )
      .contents
      .zipWithIndex
      .map ( x =>
        TransitionReport .mk (x ._2) (get_transition_index (x ._2) (configuration .rules .size) ) (
          x ._1 .fst) (x ._1 .snd) (x ._1 .trd) (x ._1 .fth)
      )

  def mk_final_report (iterations : Int) (transitions : Seq [TransitionReport] ) (errors : Seq [String] ) (
      reading_start : Long) (validation_start : Long) (execution_start : Long) : FinalReport =
    FinalReport .mk (
      iterations) (
      transitions) (
      errors) (
      validation_start - reading_start) (
      execution_start - validation_start) (
      System .nanoTime () - execution_start
    )

  def process_after_computation (seq : Seq [TransitionReport] ) (errors : Seq [String] ) (iterations : Int) (
      reading_start : Long) (validation_start : Long) (execution_start : Long) : FinalReport =
    if ( seq .isEmpty
    )
      mk_final_report (iterations) (seq) (
        errors .++ (Seq [String] (error_processing_instance) )
      ) (reading_start) (validation_start) (execution_start)
    else mk_final_report (iterations) (seq) (errors) (reading_start) (validation_start) (execution_start)

  def process_validated_instance (conf : Configuration) (errors : Seq [String] ) (iterations : Int) (
      reading_start : Long) (validation_start : Long) (execution_start : Long) : FinalReport =
    if ( errors .isEmpty
    )
      process_after_computation (
        process_configuration (conf)
      ) (errors) (iterations) (reading_start) (validation_start) (execution_start)
    else mk_final_report (iterations) (Seq .empty) (errors) (reading_start) (validation_start) (execution_start)

  def process_instance (reading_start : Long) (conf : Configuration) (iterations : Int) (validation_start : Long)
      : FinalReport =
    process_validated_instance (conf) (
      ConfigurationValidator .mk .validate (conf)
    ) (iterations) (reading_start) (validation_start) (System .nanoTime () )

  def process_maybe_instance (reading_start : Long) (maybe_conf : Option [Configuration] ) (
      iterations : Int) (validation_start : Long) : FinalReport =
    maybe_conf match  {
      case Some (conf) =>
        process_instance (reading_start) (
          get_extended_configuration (conf) (iterations)
        ) (iterations) (validation_start)
      case None =>
        mk_final_report (iterations) (Seq .empty) (
          Seq [String] (error_configuration_is_undefined)
        ) (reading_start) (validation_start) (System .nanoTime () )
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
    "\nVerification Pipeline" +
    "\n" +
    "\nCopyright 2026 Julian Alfredo Mendez" +
    "\nhttps://julianmendez.github.io/verification-pipeline" +
    "\n" +
    "\nParameters:  FILE_NAME  [ITERATIONS]" +
    "\n" +
    "\n  FILE_NAME     YAML file containing the instance" +
    "\n  ITERATIONS    (optional) number of iterations; its default value is 1." +
    "\n" +
    "\n"

  lazy val help_file : String = "/docs/help.md"

  lazy val output = new PrintWriter (System .out)

  def get_maybe_configuration (maybe_content : Try [String] ) : Option [Configuration] =
    if ( maybe_content .isSuccess
    ) YamlParser .mk .parse ( new StringReader (maybe_content .get) )
    else None

  def process_input_file (file_name : String) (iterations : Int) : Boolean =
    ReportWriter .mk .write (
      InstanceProcessor .mk .process_maybe_instance (System .nanoTime () ) (
        get_maybe_configuration (
          SimpleFileReader .mk .try_read_file (file_name)
        )
      ) (iterations) (System .nanoTime () )
    ) (output)

  def execute (arguments : List [String] ) : Unit =
    if ( (arguments .length > 0)
    )
      process_input_file  (arguments (0) ) (
        if ( arguments .length > 1
        ) (arguments (1) ) .toIntOption .getOrElse (0)
        else 1
      )
    else
      println (
        SimpleFileReader .mk
          .try_read_resource (help_file)
          .getOrElse (help)
      )

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


trait ReportWriter
{



  lazy val sr = Serializer .mk

  def write (writer : Writer) (content : String) : Boolean =
    Some [Boolean] (true)
      .map ( x => writer .write (content) )
      .map ( x => true)
      .getOrElse (false)

  def write_all_transitions (writer : Writer) (transitions : Seq [TransitionReport] ) : Boolean =
    if ( (transitions .nonEmpty)
    )
      Some (true)
        .map ( x => write (writer) (sr .key_all_transitions + "\n") )
        .map ( x =>
          transitions
            .map ( x => write (writer) (sr .serialize_transition (x) ) )
        )
        .map ( x => true)
        .getOrElse (false)
    else false

  def write_invalid_transitions(writer : Writer)  (transitions : Seq [TransitionReport] ) : Boolean =
    if ( (transitions
      .filter ( x => ! x .valid)
      .nonEmpty)
    )
      Some (true)
        .map ( x => write (writer) (sr .key_invalid_transitions + "\n") )
        .map ( x =>
          transitions
            .filter ( x => ! x .valid)
            .map ( x => write (writer) (sr .serialize_transition (x) ) )
        )
        .map ( x => true)
        .getOrElse (false)
    else false

  def write (report : FinalReport) (writer : Writer) : Boolean =
    Some [Boolean] (true)
      .map ( x => write (writer) (sr .file_separator + "\n")
      )
      .map ( x => write (writer) (
        sr .serialize_errors (report .errors) )
      )
      .map ( x => write (writer) (
        sr .serialize_iterations (report .iterations) )
      )
      .map ( x => write (writer) (
        sr .serialize_time_measures (report .reading_time) (report .validation_time) (report .execution_time) )
      )
      .map ( x => write_invalid_transitions (writer) (report .transitions) )
      .map ( x => write_all_transitions (writer) (report .transitions) )
      .map ( x => writer .flush () )
      .map ( x => true)
      .getOrElse (false)

}

case class ReportWriter_ () extends ReportWriter

object ReportWriter {
  def mk : ReportWriter =
    ReportWriter_ ()
}


trait Serializer
{



  lazy val error_parsing_error = "parsing error possibly caused by a misspelled rule name or YAML key"

  lazy val key_all_transitions = "- all_transitions:"

  lazy val key_invalid_transitions = "- invalid_transitions:"

  lazy val key_iterations = "- iterations: "

  lazy val key_errors = "- errors:"

  lazy val key_reading_time = "- reading_time: "

  lazy val key_validation_time = "- validation_time: "

  lazy val key_execution_time = "- execution_time: "

  lazy val key_total_time = "- total_time: "

  lazy val file_separator = "---"

  def serialize_transition (entry : TransitionReport) : String =
    "  - test_index: " + entry .test_index + "\n" +
    "    transition_index: " + entry .transition_index + "\n" +
    "    transition: " + entry .transition + "\n" +
    "    inhibiting_actions: " + entry .inhibiting_actions + "\n" +
    "    rule: " + entry .rule + "\n" +
    "    valid: " + entry .valid + "\n"

  def serialize_all_transitions (transitions : Seq [TransitionReport] ) : String =
    if ( (transitions .nonEmpty)
    )
        key_all_transitions + "\n" +
        transitions
          .map ( x => serialize_transition (x) )
          .mkString
    else ""

  def serialize_invalid_transitions (transitions : Seq [TransitionReport] ) : String =
    if ( (transitions
      .filter ( x => ! x .valid)
      .nonEmpty)
    )
       key_invalid_transitions + "\n" +
        transitions
          .filter ( x => ! x .valid)
          .map ( x => serialize_transition (x) )
          .mkString
    else ""

  def format_nanoseconds (x : Long) : String =
    "" + (x / 1000000) + " ms"

  def serialize_time_measures (reading_time : Long) (validation_time : Long) (execution_time : Long) : String =
    key_reading_time + format_nanoseconds(reading_time) + "\n" +
    key_validation_time + format_nanoseconds(validation_time) + "\n" +
    key_execution_time + format_nanoseconds(execution_time) + "\n" +
    key_total_time + format_nanoseconds(reading_time + validation_time + execution_time) + "\n"

  def serialize_errors (errors : Seq [String] ) : String =
    if ( (errors .nonEmpty)
    )
      key_errors + "\n" +
        (errors
          .map ( x => "  - " + x)
          .mkString ("\n")
        ) + "\n"
    else ""

  def serialize_iterations (iterations : Int) : String =
    key_iterations + iterations + "\n"

  def serialize (report : FinalReport) : String =
    file_separator + "\n" +
    serialize_errors (report .errors) +
    serialize_iterations (report .iterations) +
    serialize_time_measures (report .reading_time) (report .validation_time) (report .execution_time) +
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
  def   inhibiting_actions : ActionSet
  def   rule : Rule
  def   valid : Boolean

}

case class TransitionReport_ (test_index : Int, transition_index : Int, transition : Transition, inhibiting_actions : ActionSet, rule : Rule, valid : Boolean) extends TransitionReport

object TransitionReport {
  def mk (test_index : Int) (transition_index : Int) (transition : Transition) (inhibiting_actions : ActionSet) (rule : Rule) (valid : Boolean) : TransitionReport =
    TransitionReport_ (test_index, transition_index, transition, inhibiting_actions, rule, valid)
}

