#!/bin/bash

#
# This script runs an instance with several iterations.
# Its only parameter is the input file.
#


start=100
increment=100
repetitions=10
application="emotion"
defaultInput="example.yaml"
pattern="execution_time:"
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


