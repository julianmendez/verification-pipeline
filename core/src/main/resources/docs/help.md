


# Verification Pipeline

Copyright 2026 Julian Alfredo Mendez
https://julianmendez.github.io/verification-pipeline

Verification Pipeline is a tool that provides a modular way
to model how emotions change in intelligent systems.
It does this by composing functions into pipelines
using the [Tiles][tiles] framework, specifying behavior in
the [Soda][soda] language. It checks whether emotional constraints
hold across state-action trajectories.

Parameters:  FILE_NAME  [ITERATIONS]

   FILE_NAME     YAML file containing the instance
   ITERATIONS    (optional) number of iterations; its default value is 1

[tiles]: https://julianmendez.github.io/tiles

[soda]: https://julianmendez.github.io/soda


## Rules

The available rules are:

  - rule: CausesIfRule
    input: FLUENTS
    action: ACTION
    output: FLUENTS

  - rule: IfRule
    input: FLUENTS
    output: FLUENTS

  - rule: TriggersRule
    input: FLUENTS
    action: ACTION

  - rule: AllowsRule
    input: FLUENTS
    action: ACTION

  - rule: InhibitsRule
    input: FLUENTS
    action: ACTION

  - rule: NoConcurrencyRule
    actions: ACTIONS

  - rule: DefaultRule
    input_fluent: FLUENT

  - rule: InfluencesIfRule
    input: FLUENTS
    action: ACTION
    output: FLUENTS

  - rule: InfluencesRule
    input: FLUENTS
    output: FLUENTS

  - rule: FacilitatesRule
    input: FLUENTS
    action: ACTION

  - rule: ContravenesRule
    input: FLUENTS
    action: ACTION

  - rule: ForbidsToCauseRule
    input: FLUENTS
    output: FLUENTS

where
  FLUENT is a fluent value
  FLUENTS is a set of fluent values
  ACTION is an action
  ACTIONS is a set of actions


## Example

This is an example of a valid YAML input file that contains invalid transitions.

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


