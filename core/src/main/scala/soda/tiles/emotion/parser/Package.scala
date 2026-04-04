package soda.tiles.emotion.parser





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

