(cl:in-package #:jenkins.language-server)

(defgeneric process-method (object method &key &allow-other-keys))

(defgeneric process-interface-method (object interface method &key &allow-other-keys))
