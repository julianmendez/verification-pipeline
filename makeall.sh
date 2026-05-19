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


# Copy an example

exampleFile="core/src/test/resources/example/example-4.yaml"
localExampleFile="example.yaml"

if [ ! -f ${localExampleFile} ]; then
  cp -p ${exampleFile} ${localExampleFile}
fi


