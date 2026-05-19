#!/bin/bash

#
# This script builds the binary files.
# It requires `sbt` [https://www.scala-sbt.org/].
#

scalaVersion="3.3.7"
executableStub="exec java -jar \$0 \"\$@\" ; exit"

sbt scalaVersion sbtVersion version clean compile test package assembly

# Build the main binary file

mainBinaryFile="verify"
mainJarFile="target/scala-${scalaVersion}/verifier-*.jar"

echo ${executableStub} >${mainBinaryFile}
cat ${mainJarFile} >>${mainBinaryFile}
chmod u+x ${mainBinaryFile}


# Prepare directory for benchmarks

benchmarkDirectory="target/benchmarks/"
benchmarkScript="core/src/main/bash/run_benchmarks.sh"
exampleFile="core/src/test/resources/example/example.yaml"

if [ ! -d ${benchmarkDirectory} ]; then
  mkdir -p ${benchmarkDirectory}
fi

cp -p ${mainBinaryFile} ${benchmarkDirectory}
cp -p ${exampleFile} ${benchmarkDirectory}
cp -p ${benchmarkScript} ${benchmarkDirectory}

