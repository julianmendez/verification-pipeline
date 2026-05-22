## YAML file

This is an example of a YAML input file

```yaml
---
- fluents:
  - fluent: fluent_name
    values:
    - fluent_value_0
    - fluent_value_1
- actions:
  - action_0
  - action_1
- rules:
  - rule: CausesIfRule
    input:
    - fluent_value_0
    action: action_0
    output:
    - fluent_value_1
  - rule: NoConcurrencyRule
    actions:
    - action_0
    - action_1
  - rule: DefaultRule
    input_fluent: fluent_value_0
- trajectory:
  - state:
    - fluent_value_0
  - actions:
    - action_0
  - state:
    - fluent_value_1
  - actions:
    - action_1
  - state:
    - fluent_value_0
```

The available rules are:

- `CausesIfRule` *input* *action* *output*
- `IfRule` *input* *output*
- `TriggersRule` *input* *action*
- `AllowsRule` *input* *action*
- `InhibitsRule` *input* *action*
- `NoConcurrencyRule` *actions*
- `DefaultRule` *input_fluent*
- `InfluencesIfRule` *input* *action* *output*
- `InfluencesRule` *input* *output*
- `FacilitatesRule` *input* *action*
- `ContravenesRule` *input* *action*
- `ForbidsToCauseRule` *input* *output*

where *input_fluent* is a fluent, *action* is an action, *input* and *output* are sets of fluents, and *actions* is a
set of actions.

Fluent values and actions need to be all different.


