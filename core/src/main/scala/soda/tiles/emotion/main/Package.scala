package soda.tiles.emotion.main

/*
 * This package is for the main class.
 * This is the entry point when the application is executed from a terminal.
 */

import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.tiles.emotion.entity.ActionSet
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.entity.InstanceBuilder
import   soda.tiles.emotion.entity.Rule
import   soda.tiles.emotion.entity.TileQuad
import   soda.tiles.emotion.entity.Transition
import   soda.tiles.emotion.parser.YamlParser
import   soda.tiles.emotion.pipeline.EmotionalReasoningPipeline
import   soda.tiles.emotion.validator.ConfigurationValidator

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

  lazy val error_undefined_result = "Undefined result - The input instance is invalid."

  lazy val error_configuration_is_undefined = "Error : the instance is invalid."

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  lazy val yaml_parser = YamlParser .mk

  lazy val serializer = Serializer .mk

  lazy val instance_builder = InstanceBuilder .mk

  lazy val cv = ConfigurationValidator .mk

  lazy val emotional_reasoning_pipeline = EmotionalReasoningPipeline .mk

  def process_configuration (configuration : Configuration)
      : Seq [TileQuad [Transition, Rule, ActionSet, Boolean] ] =
    emotional_reasoning_pipeline .run (
      instance_builder
        .build (configuration)
    ) .contents

  def process_instance_with (conf : Configuration) (errors : Option [String] ) : String =
    errors match  {
      case Some (error) => serializer .serialize_errors (error)
      case None => serializer .serialize_response (process_configuration (conf) )
    }

  def process_instance (maybe_conf : Option [Configuration] ) : String =
    maybe_conf match  {
      case Some (conf) => process_instance_with (conf) (cv .validate (conf) )
      case None => error_configuration_is_undefined
    }

  def get_maybe_configuration (file_name : String) : Option [Configuration] =
    yaml_parser .parse ( new StringReader (read_file (file_name) ) )

  def process_input_file (file_name : String) : String =
    process_instance (
      get_maybe_configuration (file_name)
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



  def show_entry (entry : TileQuad [Transition, Rule, ActionSet, Boolean] ) : String =
    "- transition : " + entry .fst + "\n" +
    "  rule : " + entry .snd + "\n" +
    "  inhibiting_actions : " + entry .trd + "\n" +
    "  valid : " + entry .fth + "\n"

  def serialize_response (seq : Seq [TileQuad [Transition, Rule, ActionSet, Boolean] ] ) : String =
    "---" + "\n" +
    (seq
      .map ( x => show_entry (x) )
      .mkString
    ) + "\n"

  def serialize_errors (error : String) : String =
    "---" + "\n" +
    "- errors:" + "\n" +
      error
        .split ("\n")
        .map ( x => "  - " + x + "\n")
        .mkString ("\n")

}

case class Serializer_ () extends Serializer

object Serializer {
  def mk : Serializer =
    Serializer_ ()
}

