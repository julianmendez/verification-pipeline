#!/bin/bash

#
# This script runs an instance through several iterations.
# Each iteration has a number of cyclically replicated repetitions
# of the transitions in an instance.
#
# This script has only one parameter: the input file name.
# If no input file name is provided, it will use a default value.
#
#


## Initial number of cyclic replications.
start=100

## Increment between experiments.
increment=100

## Number of iterations.
repetitions=10

## Location of the binary file.
application="./verify"

## YAML input file with the instance.
defaultInput="example.yaml"

## Pattern of relevant time to retrieve.
pattern="execution_time:"

## Unit of time measured.
milliseconds="ms"


if [[ -n "$1" ]]; then
  input="$1"
else
  input="${defaultInput}"
fi


iterations=${start}
for (( i=0 ; i < repetitions ; i++ )); do
  executionTime=$( \
    "${application}" "${input}" "${iterations}" \
      | grep ${pattern} \
      | sed "s/.*${pattern}//g" \
      | sed "s/${milliseconds}//g" )
  echo "${iterations}, ${executionTime}"
  iterations=$(( iterations + increment ))
done


