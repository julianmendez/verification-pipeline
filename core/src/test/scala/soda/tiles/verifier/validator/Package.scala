package soda.tiles.verifier.validator

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.AllowsRule
import   soda.tiles.verifier.entity.CausesIfRule
import   soda.tiles.verifier.entity.ContravenesRule
import   soda.tiles.verifier.entity.DefaultRule
import   soda.tiles.verifier.entity.FacilitatesRule
import   soda.tiles.verifier.entity.FluentMap
import   soda.tiles.verifier.entity.FluentSet
import   soda.tiles.verifier.entity.ForbidsToCauseRule
import   soda.tiles.verifier.entity.IfRule
import   soda.tiles.verifier.entity.InfluencesIfRule
import   soda.tiles.verifier.entity.InfluencesRule
import   soda.tiles.verifier.entity.InhibitsRule
import   soda.tiles.verifier.entity.NoConcurrencyRule
import   soda.tiles.verifier.entity.TriggersRule





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

  test ("mapped returns only mapped fluents") (
    check (
      obtained = FluentValidator .mk .mapped (fluent_set_0) (fluent_map_0)
    ) (
      expected = (Seq ("A" , "C") )
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
      obtained = FluentValidator .mk .is_valid (fluent_set_2) (fluent_map_2)
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

  test ("mapped returns empty set when no fluents match") (
    check (
      obtained = FluentValidator .mk .mapped (fluent_set_3) (fluent_map_3)
    ) (
      expected = Seq .empty
    )
  )

  lazy val fluent_set_4 : FluentSet =
    Seq ("a" , "b") .toSet

  lazy val fluent_map_4 : FluentMap =
    Seq (
      ("a" , "X") ,
      ("b" , "X")
    ) .toMap

  test ("mapped does not collapse duplicate mapped values into a set") (
    check (
      obtained = FluentValidator .mk .mapped (fluent_set_4) (fluent_map_4)
    ) (
      expected = Seq ("X" , "X")
    )
  )

}




case class RuleValidatorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val rv = RuleValidator .mk

  lazy val fluent_map_ok : FluentMap =
    Seq (
      ("a" , "A"),
      ("b" , "B"),
      ("c" , "C")
    ) .toMap

  lazy val fluent_map_bad : FluentMap =
    Seq (
      ("a" , "A")
    ) .toMap

  lazy val action_set_ok : ActionSet =
    Seq ("act1" , "act2") .toSet

  lazy val action_set_bad : ActionSet =
    Seq ("actX") .toSet

  lazy val input_set : FluentSet =
    Seq ("a" , "b") .toSet

  lazy val output_set : FluentSet =
    Seq ("c") .toSet

  test ("CausesIfRule is valid when fluents and action are valid") (
    check (
      obtained = rv .is_valid (
        CausesIfRule (input_set , "act1" , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("CausesIfRule is invalid when fluents are missing") (
    check (
      obtained = rv .is_valid (
        CausesIfRule (Seq ("x") .toSet , "act1" , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("CausesIfRule is invalid when action is missing") (
    check (
      obtained = rv .is_valid (
        CausesIfRule (input_set , "missingAction" , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("IfRule is valid when fluents exist") (
    check (
      obtained = rv .is_valid (
        IfRule (input_set , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("IfRule is invalid when output fluents missing") (
    check (
      obtained = rv .is_valid (
        IfRule (input_set , Seq ("z") .toSet)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("TriggersRule invalid when action missing") (
    check (
      obtained = rv .is_valid (
        TriggersRule (input_set , "badAction")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("AllowsRule valid when fluents and action valid") (
    check (
      obtained = rv .is_valid (
        AllowsRule (input_set , "act2")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("InhibitsRule invalid when fluents missing") (
    check (
      obtained = rv .is_valid (
        InhibitsRule (Seq ("x") .toSet , "act1")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("FacilitatesRule invalid when action missing") (
    check (
      obtained = rv .is_valid (
        FacilitatesRule (input_set , "nope")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("ContravenesRule valid when all good") (
    check (
      obtained = rv .is_valid (
        ContravenesRule (input_set , "act1")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("NoConcurrencyRule valid when all actions exist") (
    check (
      obtained = rv .is_valid (
        NoConcurrencyRule (Seq ("act1" , "act2") .toSet)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("NoConcurrencyRule invalid when some actions missing") (
    check (
      obtained = rv .is_valid (
        NoConcurrencyRule (Seq ("act1" , "badAction") .toSet)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("DefaultRule valid when fluent exists") (
    check (
      obtained = rv .is_valid (
        DefaultRule ("a")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("DefaultRule invalid when fluent missing") (
    check (
      obtained = rv .is_valid (
        DefaultRule ("z")
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("InfluencesIfRule valid when all components valid") (
    check (
      obtained = rv .is_valid (
        InfluencesIfRule (input_set , "act1" , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("InfluencesRule invalid when output missing") (
    check (
      obtained = rv .is_valid (
        InfluencesRule (input_set , Seq ("missing") .toSet)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("ForbidsToCauseRule valid when fluents valid") (
    check (
      obtained = rv .is_valid (
        ForbidsToCauseRule (input_set , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("ForbidsToCauseRule invalid when input missing") (
    check (
      obtained = rv .is_valid (
        ForbidsToCauseRule (Seq ("x") .toSet , output_set)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

}

