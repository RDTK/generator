(cl:defpackage #:jenkins.language-server
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus)

  (:export
   #:language-server))

#+no (uiop:symbol-call '#:swank '#:swank-require
                       '("SWANK-ARGLISTS"
                         "SWANK-FANCY-INSPECTOR" "SWANK-FUZZY" "SWANK-C-P-C" "SWANK-UTIL"
                         "SWANK-MACROSTEP" "SWANK-PRESENTATIONS" "SWANK-REPL" "SWANK-PACKAGE-FU"
                         "SWANK-TRACE-DIALOG" "SWANK-ASDF" "SWANK-SPROF" "SB-SPROF" "SWANK-MEDIA"
                         "SWANK-QUICKLISP"))
