#!/bin/env bash

mk-freeze() {
  # Generate a cabal.config on stdout from the list of fully-qualified
  # packages on stdin, sorting them in the C locale (so we get ASCII
  # order), reformatting as Cabal version constraints on stdout.
  echo "constraints: base>=0,"
  LC_ALL=C sort | sed s/^/"             "/                \
        | sed 's/-\([^-]*\)$/ ==\1/' | sed '$!s/$/,/'
}

stack --stack-yaml stack-7.10.3.yaml list-dependencies    \
    | egrep -v 'rts|atlas'                                \
    | tr ' ' '-'                                          \
    | mk-freeze > build/cabal/cabal-7.10.3.config
