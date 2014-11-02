#!/bin/bash
RET=0

if [ -f "`git root`/project.clj" ]
then
    cd `git root`
    RET=$(lein deploy clojars)
fi

exit $RET
