#!/usr/bin/env bash
set -eu
set -o nounset
SCALA_VERSION="$1"


sbt -Dfile.encoding=UTF-8 \
  -J-XX:ReservedCodeCacheSize=256M \
  -J-Xmx3046M -J-Xms3046M -J-server \
  +mimaReportBinaryIssues \
  scalafmt::test \
  test:scalafmt::test \
  zincRoot/test:compile \
  crossTestBridges \
  "publishBridgesAndSet $SCALA_VERSION" \
  zincRoot/test \
  zincRoot/scripted
