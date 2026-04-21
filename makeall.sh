#!/bin/bash

#
# This script builds the binary files.
# It requires `sbt` [https://www.scala-sbt.org/].
#

scalaVersion="3.3.7"
executableStub="exec java -jar \$0 \"\$@\" ; exit"

sbt scalaVersion sbtVersion version clean compile test package assembly

# Build the main binary file

mainBinaryFile="emotion"
mainJarFile="target/scala-${scalaVersion}/${mainBinaryFile}-*.jar"

echo ${executableStub} >${mainBinaryFile}
cat ${mainJarFile} >>${mainBinaryFile}
chmod u+x ${mainBinaryFile}


# Copy an example

exampleFile="core/src/test/resources/example/example-1.yaml"
localExampleFile="example-1.yaml"

if [ ! -f ${localExampleFile} ]; then
  cp -p ${exampleFile} ${localExampleFile}
fi


