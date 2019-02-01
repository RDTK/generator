(cl:in-package #:jenkins.project.commandline-interface.test)

(esrap:parse 'multi "dependency-error=>abort:object-error=>abort:instantiation-error=>abort:abort")

(configuration.options:raw->value-using-type
 nil
 "object-error=>abort:instantiation-error=>abort:abort"
 'error-policy)

(configuration.options:raw->value-using-type
 nil
 "abort"
 'error-policy)

(configuration.options:value->string-using-type
 nil
 '((error . :fail) (t . :abort))
 'error-policy)

(configuration.options:value->string-using-type
 nil
 '((caused-by-unfulfilled-project-dependency-error . :fail) (t . :abort))
 'error-policy)
