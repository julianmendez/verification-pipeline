#!/bin/bash

#
# This script builds the binary files.
# It requires `sbt` [https://www.scala-sbt.org/].
#

scalaVersion="3.3.8"
executableStub="exec java -jar \$0 \"\$@\" ; exit"

executableSbt="sbt"

# If sbt is not present, download
# https://github.com/dwijnand/sbt-extras/blob/master/sbt
# and make it executable. Then, include the following line.
# executableSbt="./sbt"

${executableSbt} scalaVersion sbtVersion version clean compile test package assembly

# Build the main binary file

mainBinaryFile="verify"
mainJarFile="target/scala-${scalaVersion}/verifier-*.jar"

echo ${executableStub} >${mainBinaryFile}
cat ${mainJarFile} >>${mainBinaryFile}
chmod u+x ${mainBinaryFile}


# Prepare directory for benchmarks

benchmarkScript="run_benchmarks.sh"
benchmarkDirectory="target/benchmarks/"
benchmarkScriptSource="core/src/main/bash/${benchmarkScript}"
exampleFile="core/src/test/resources/example/example.yaml"

if [ ! -d ${benchmarkDirectory} ]; then
  mkdir -p ${benchmarkDirectory}
fi

cp -p ${mainBinaryFile} ${benchmarkDirectory}
cp -p ${exampleFile} ${benchmarkDirectory}
cp -p ${benchmarkScriptSource} ${benchmarkDirectory}
chmod u+x ${benchmarkDirectory}/${benchmarkScript}


