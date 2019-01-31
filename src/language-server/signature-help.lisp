;;;; signature-help.lisp --- Signature help contributors for different contexts.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.language-server)

;;; Variable

(defclass variable-signature-contributor () ())

(defmethod contrib:signature-contributions ((workspace   t)
                                            (document    t)
                                            (context     variable-value-context)
                                            (contributor variable-signature-contributor))
  (let+ (((&accessors-r/o variable-node) context))
    (values (list (proto::make-signature-information
                   (format nil "~A foo bar baz" (var:variable-info-name variable-node))
                   (list (proto::make-parameter-information "foo" "You must realize that there is no parameter"))
                   :documentation (or (var:variable-info-documentation variable-node)
                                      "«undocumented variable»")))
            0
            0)))
