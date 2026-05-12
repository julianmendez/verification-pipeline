package soda.tiles.emotion.validator

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.emotion.entity.FluentMap
import   soda.tiles.emotion.entity.FluentSet





case class FluentValidatorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fluent_set_0 : FluentSet =
    Seq ("a" , "b" , "c") .toSet

  lazy val fluent_map_0 : FluentMap =
    Seq (
      ("a" , "A"),
      ("c" , "C")
    ) .toMap

  test ("get_fluents returns only mapped fluents") (
    check (
      obtained = FluentValidator .mk .get_fluents (fluent_set_0) (fluent_map_0)
    ) (
      expected = (Seq("A" , "C")) .toSet
    )
  )

  lazy val fluent_set_1 : FluentSet =
    Seq ("x" , "y") .toSet

  lazy val fluent_map_1 : FluentMap =
    Seq (
      ("x" , "X"),
      ("y" , "Y")
    ) .toMap

  test ("is_valid returns true when all fluents are present") (
    check (
      obtained = FluentValidator .mk .is_valid (fluent_set_1) (fluent_map_1)
    ) (
      expected = true
    )
  )

  lazy val fluent_set_2 : FluentSet =
    Seq ("x" , "y" , "z") .toSet

  lazy val fluent_map_2 : FluentMap =
    Seq (
      ("x" , "X"),
      ("z" , "Z")
    ) .toMap

  test ("is_valid returns false when some fluents are missing") (
    check (
      obtained = FluentValidator .mk .is_valid (fluent_set_2)(fluent_map_2)
    ) (
      expected = false
    )
  )

  lazy val fluent_set_3 : FluentSet =
    Seq ("p" , "q") .toSet

  lazy val fluent_map_3 : FluentMap =
    Seq (
      ("x" , "X")
    ) .toMap

  test ("get_fluents returns empty set when no fluents match") (
    check (
      obtained = FluentValidator .mk.get_fluents (fluent_set_3) (fluent_map_3)
    ) (
      expected = Set.empty
    )
  )

  lazy val fluent_set_4 : FluentSet =
    Seq ("a" , "b") .toSet

  lazy val fluent_map_4 : FluentMap =
    Seq (
      ("a" , "X"),
      ("b" , "X")
    ) .toMap

  test ("get_fluents collapses duplicate mapped values into a set") (
    check (
      obtained = FluentValidator .mk .get_fluents(fluent_set_4)(fluent_map_4)
    ) (
      expected = Seq("X") .toSet
    )
  )

}



