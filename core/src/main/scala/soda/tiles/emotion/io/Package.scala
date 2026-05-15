package soda.tiles.emotion.io

/*
 * This package is to handle the input/output.
 */

import   java.io.BufferedReader
import   java.io.InputStream
import   java.io.InputStreamReader
import   java.nio.file.Files
import   java.nio.file.Paths
import   java.util.stream.Collectors
import   scala.util.Try





/**
 * This is an auxiliary class to read small files.
 */

trait SimpleFileReader
{



  import   java.io.BufferedReader
  import   java.io.InputStream
  import   java.io.InputStreamReader
  import   java.nio.file.Files
  import   java.nio.file.Paths
  import   java.util.stream.Collectors
  import   scala.util.Try

  lazy val new_line = "\n"

  def read_file (file_name : String) : String =
    new String (Files .readAllBytes (Paths .get (file_name) ) )

  def try_read_file (file_name : String) : Try [String] =
    Try [String] (read_file (file_name) )

  private def _read_reader_content (reader : BufferedReader) : String =
    reader .lines () .collect (Collectors .joining (new_line) )

  def read_input_stream (input_stream : InputStream) : String =
    _read_reader_content ( new BufferedReader ( new InputStreamReader (input_stream) ) )

  def read_resource (file_name : String) : String =
    read_input_stream (getClass .getResourceAsStream (file_name) )

  def try_read_resource (file_name : String) : Try [String] =
    Try [String] (read_resource (file_name) )

}

case class SimpleFileReader_ () extends SimpleFileReader

object SimpleFileReader {
  def mk : SimpleFileReader =
    SimpleFileReader_ ()
}

