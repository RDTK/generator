#!/bin/bash

# Lisp Version
sbcl_version="1.0.45"
if [ $(uname -m) == "x86_64" ] ; then
  sbcl_home=/homes/jmoringe/opt/sbcl/lib/sbcl
  sbcl_bin=/homes/jmoringe/opt/sbcl/bin/sbcl
else
  sbcl_home=/vol/ai/releases/trunk/lib/sbcl
  sbcl_bin=/vol/ai/releases/trunk/bin/sbcl
fi

# ASDF Setup
export CL_SOURCE_REGISTRY="${WORKSPACE}//:"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (t \"${WORKSPACE}/../.fasl-cache-${sbcl_version}-${JOB_NAME}\") :ignore-inherited-configuration)"

# TODO temp
mkdir -p "${WORKSPACE}/doc/coverage-report/"

# Condition handler and payload
while read line ; do
  request="${request} ${line}"
done
read code <<EOF
(require 'asdf)							\
(%load-silently :trivial-backtrace)				\
(proclaim '(optimize (debug 3) (safety 3)))			\
(handler-case							\
    (progn							\
      ${request}						\
      (values))							\
  (error (condition)						\
    (trivial-backtrace:print-backtrace				\
     condition							\
     :output *standard-output*)					\
    (sb-ext:quit :recklessly-p t			        \
                 :unix-status -1)))
EOF

# Set home and execute code
export SBCL_HOME="${sbcl_home}"
echo "${code}" | "${sbcl_bin}" --userinit "${WORKSPACE}/../../sbclrc" $*
