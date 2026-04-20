package soda.tiles.emotion.main

/*
 * This package is for the main class.
 * This is the entry point when the application is executed from a terminal.
 */

import   java.nio.file.Files
import   java.nio.file.Paths
import   java.io.StringReader
import   soda.tiles.emotion.entity.Configuration
import   soda.tiles.emotion.parser.YamlParser





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

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  lazy val yaml_parser = YamlParser .mk

  lazy val serializer = Serializer .mk

  /** TODO **/

  def process_configuration (conf : Configuration) : Option [Seq [Boolean] ] =
    Some (
      conf
        .trajectory
        .map ( x => false)
    )

  def process_maybe_configuration (maybe_conf : Option [Configuration] ) : Option [Seq [Boolean] ] =
    if ( (maybe_conf .isDefined)
    ) process_configuration (maybe_conf .get)
    else None

  def process_file (file_name : String) : Option [Seq [Boolean] ] =
    process_maybe_configuration (
      yaml_parser .parse ( new StringReader (read_file (file_name) ) )
    )

  def serialize_output (maybe_output : Option [Seq [Boolean] ] ) : String =
    maybe_output match  {
      case Some (instance) => serializer .serialize (instance)
      case None => "Undefined result"
    }

  def execute (arguments : List [String] ) : Unit =
    if ( (arguments .length > 0)
    ) println (serialize_output (process_file (arguments (0) ) ) )
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



  def serialize (seq : Seq [Boolean] ) : String =
    (seq
      .map ( x => x .toString + " ")
      .mkString ) + "\n"

}

case class Serializer_ () extends Serializer

object Serializer {
  def mk : Serializer =
    Serializer_ ()
}

