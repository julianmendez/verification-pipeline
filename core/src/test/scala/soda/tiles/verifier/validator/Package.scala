package soda.tiles.verifier.validator

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.verifier.entity.ActionSet
import   soda.tiles.verifier.entity.AllowsRule
import   soda.tiles.verifier.entity.CausesIfRule
import   soda.tiles.verifier.entity.Configuration
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
import   soda.tiles.verifier.entity.Trajectory
import   soda.tiles.verifier.entity.TriggersRule





case class ActionValidatorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val av = ActionValidator .mk

  lazy val action_set_ok : ActionSet =
    Seq ("a1" , "a2" , "a3") .toSet

  lazy val action_set_small : ActionSet =
    Seq ("a1") .toSet

  test ("validate_action returns empty when action exists") (
    check (
      obtained = av .validate_action ("a1") (action_set_ok)
    ) (
      expected = Seq .empty
    )
  )

  test ("validate_action returns error when action missing") (
    check (
      obtained = av .validate_action ("missing") (action_set_ok)
    ) (
      expected = Seq (ActionValidator .mk .error_unknown_action + "missing")
    )
  )

  test ("validate returns empty when all actions exist") (
    check (
      obtained = av .validate (Seq ("a1" , "a2") .toSet) (action_set_ok)
    ) (
      expected = Seq .empty
    )
  )

  test ("validate returns errors for each missing action") (
    check (
      obtained = av .validate (Seq ("a1" , "bad1" , "bad2") .toSet) (action_set_ok)
    ) (
      expected = Seq (
        ActionValidator .mk .error_unknown_action + "bad1",
        ActionValidator .mk .error_unknown_action + "bad2"
      )
    )
  )

  test ("is_valid returns true when all actions exist") (
    check (
      obtained = av .is_valid (Seq ("a1") .toSet) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("is_valid returns false when any action missing") (
    check (
      obtained = av .is_valid (Seq ("a1" , "missing") .toSet) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("validate on empty action set returns empty") (
    check (
      obtained = av .validate (Seq .empty .toSet) (action_set_ok)
    ) (
      expected = Seq .empty
    )
  )

  test ("is_valid on empty action set returns true") (
    check (
      obtained = av .is_valid (Seq .empty .toSet) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("validate_action fails when action_set is empty") (
    check (
      obtained = av .validate_action ("a1") (Seq .empty .toSet)
    ) (
      expected = Seq (ActionValidator .mk .error_unknown_action + "a1")
    )
  )

}


case class ConfigurationValidatorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val cv = ConfigurationValidator .mk

  lazy val fluent_map_ok : FluentMap =
    Seq (
      ("f1" , "F1"),
      ("f2" , "F2"),
      ("f3" , "F3")
    ) .toMap

  lazy val fluent_map_bad : FluentMap =
    Seq (
      ("f1" , "F1")
    ) .toMap

  lazy val action_set_ok : ActionSet =
    Seq ("a1" , "a2") .toSet

  lazy val action_set_bad : ActionSet =
    Seq ("aX") .toSet

  lazy val valid_trajectory : Trajectory =
    Seq (
      Seq ("f1") .toSet ,
      Seq ("a1") .toSet ,
      Seq ("f2") .toSet
    )

  lazy val invalid_trajectory : Trajectory =
    Seq (
      Seq ("badF") .toSet ,
      Seq ("a1") .toSet ,
      Seq ("f2") .toSet
    )

  test ("valid configuration produces no errors") (
    check (
      obtained =
        cv.validate (
          Configuration.mk (fluent_map_ok) (action_set_ok) (
            Seq (
              IfRule (Seq ("f1") .toSet , Seq ("f2") .toSet)
            )
          ) (valid_trajectory)
        )
    ) (
      expected = Seq .empty
    )
  )

  test ("is_valid returns true for valid configuration") (
    check (
      obtained =
        cv.is_valid (
          Configuration.mk (
            fluent_map_ok) (
            action_set_ok) (
            Seq (
              AllowsRule (Seq ("f1") .toSet , "a1")
            )
          ) (valid_trajectory)
        )
    ) (
      expected = true
    )
  )

  test ("invalid rule produces rule validation errors") (
    check (
      obtained =
        cv.validate(
          Configuration.mk(
            fluent_map_ok) (
            action_set_ok) (
            Seq (
              AllowsRule (Seq ("badF") .toSet , "a1")
            )
          ) (valid_trajectory)
        )
    ) (
      expected = Seq (
        FluentValidator .mk .error_unknown_fluent + "badF"
      )
    )
  )

  test ("invalid trajectory produces trajectory validation errors") (
    check (
      obtained =
        cv.validate(
          Configuration.mk(
            fluent_map_ok) (
            action_set_ok) (
            Seq ()
          ) (invalid_trajectory)
        )
    ) (
      expected = Seq (
        FluentValidator .mk .error_unknown_fluent + "badF"
      )
    )
  )

  test ("configuration accumulates both rule and trajectory errors") (
    check (
      obtained =
        cv.validate(
          Configuration.mk(
            fluent_map_ok) (
            action_set_ok) (
            Seq (
              AllowsRule (Seq ("badF1") .toSet , "badA")
            )
          ) (invalid_trajectory)
        )
    ) (
      expected = Seq (
        FluentValidator .mk .error_unknown_fluent + "badF1",
        ActionValidator .mk .error_unknown_action + "badA",
        FluentValidator .mk .error_unknown_fluent + "badF"
      )
    )
  )

  test ("is_valid returns false when rule invalid") (
    check (
      obtained =
        cv.is_valid(
          Configuration.mk(
            fluent_map_ok) (
            action_set_ok) (
            Seq (
              IfRule (Seq ("badF") .toSet , Seq ("f1") .toSet)
            )
          ) (valid_trajectory)
        )
    ) (
      expected = false
    )
  )

  test ("is_valid returns false when trajectory invalid") (
    check (
      obtained =
        cv.is_valid(
          Configuration.mk(
            fluent_map_ok) (
            action_set_ok) (
            Seq ()
          ) (invalid_trajectory)
        )
    ) (
      expected = false
    )
  )

}


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


case class TrajectoryValidatorSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val tv = TrajectoryValidator .mk

  lazy val fluent_map_ok : FluentMap =
    Seq (
      ("f1" , "F1"),
      ("f2" , "F2"),
      ("f3" , "F3")
    ) .toMap

  lazy val fluent_map_bad : FluentMap =
    Seq (
      ("f1" , "F1")
    ) .toMap

  lazy val action_set_ok : ActionSet =
    Seq ("a1" , "a2") .toSet

  lazy val action_set_bad : ActionSet =
    Seq ("aX") .toSet

  test ("trajectory too short (< 3) produces error") (
    check (
      obtained = tv .validate (
        Seq (Seq ("f1") .toSet , Seq ("a1") .toSet)
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq (TrajectoryValidator .mk .error_trajectory_is_too_short)
    )
  )

  test ("trajectory of even length produces error") (
    check (
      obtained = tv .validate (
        Seq (
          Seq ("f1") .toSet,
          Seq ("a1") .toSet,
          Seq ("f2") .toSet,
          Seq ("a2") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq (TrajectoryValidator .mk .error_trajectory_length_should_be_odd)
    )
  )

  test ("valid trajectory with correct fluents and actions returns empty error list") (
    check (
      obtained = tv .validate (
        Seq (
          Seq ("f1") .toSet,   // fluents
          Seq ("a1") .toSet,   // actions
          Seq ("f2") .toSet    // fluents
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq .empty
    )
  )

  test ("is_valid returns true for valid trajectory") (
    check (
      obtained = tv .is_valid (
        Seq (
          Seq ("f1") .toSet,
          Seq ("a1") .toSet,
          Seq ("f2") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = true
    )
  )

  test ("invalid fluent in fluent window produces fluent error") (
    check (
      obtained = tv .validate (
        Seq (
          Seq ("badF") .toSet,
          Seq ("a1") .toSet,
          Seq ("f1") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq (
        FluentValidator .mk .error_unknown_fluent + "badF"
      )
    )
  )

  test ("invalid action in action window produces action error") (
    check (
      obtained = tv .validate (
        Seq (
          Seq ("f1") .toSet,
          Seq ("badA") .toSet,
          Seq ("f2") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq (
        ActionValidator .mk .error_unknown_action + "badA"
      )
    )
  )

  test ("trajectory stops accumulating errors after first invalid window") (
    check (
      obtained = tv .validate (
        Seq (
          Seq ("badF") .toSet,   // invalid fluent
          Seq ("badA") .toSet,   // would be invalid action, but should be ignored
          Seq ("badF2") .toSet   // ignored
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = Seq (
        FluentValidator .mk .error_unknown_fluent + "badF"
      )
    )
  )

  test ("is_valid returns false when any fluent window invalid") (
    check (
      obtained = tv .is_valid (
        Seq (
          Seq ("badF") .toSet,
          Seq ("a1") .toSet,
          Seq ("f1") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

  test ("is_valid returns false when any action window invalid") (
    check (
      obtained = tv .is_valid (
        Seq (
          Seq ("f1") .toSet,
          Seq ("badA") .toSet,
          Seq ("f2") .toSet
        )
      ) (fluent_map_ok) (action_set_ok)
    ) (
      expected = false
    )
  )

}

