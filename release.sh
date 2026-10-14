#!/bin/bash

set -euxo pipefail

sbt --server cleanFull +testFull ci-release
