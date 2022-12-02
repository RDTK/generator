;;;; main.lisp --- Entry-point of commandline-interface module.
;;;;
;;;; Copyright (C) 2013-2022 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:build-generator.commandline-interface)

(defun call-with-condition-printing (thunk &key (stream *error-output*) color)
  (flet (#+unix
         (sgr (code)
           (let* ((string (format nil "~C[~Dm" #\Escape code))
                  (length (length string)))
             (write-string string stream)
             (finish-output stream)
             #+sbcl (labels ((resolve (stream)
                               (typecase stream
                                 (synonym-stream
                                  (resolve (symbol-value
                                            (synonym-stream-symbol stream))))
                                 (t
                                  stream))))
                      (let ((stream (resolve stream)))
                        (when (sb-impl::fd-stream-p stream)
                          (let ((column (sb-impl::fd-stream-output-column stream)))
                            ;; Intervening output may have changed the
                            ;; column.
                            (when (and column (>= column length))
                              (setf (sb-impl::fd-stream-output-column stream)
                                    (- column length)))))))))
         (do-it ()
           (funcall thunk stream)))
    (if color
        (unwind-protect
             (progn
               #+unix (sgr (ecase color
                             (:red    31)
                             (:yellow 33)))
               (do-it))
          #+unix (sgr 0))
        (do-it))))

(defun pretty-print-condition (condition stream)
  (format stream "~@<~A~@:>~2%" condition))

(defun make-error-policy (policy &key (lock (bt:make-recursive-lock "output and debug"))
                                      debug? color? fail)
  (let+ (((&flet flame (condition &key (debug? debug?))
            (bt:with-recursive-lock-held (lock)
              (call-with-condition-printing
               (curry #'pretty-print-condition condition)
               :color (when color? :red))
              (when debug?
                #+sbcl (sb-debug:print-backtrace)))))
         ;; Specific actions.
         ((&flet do-continue (condition)
            (when-let ((restart (find-restart 'commands::defer condition))) ; TODO should just call defer
              (invoke-restart restart condition :debug? debug?))

            (when (typep condition 'util:continuable-error)
              (when-let ((restart (util:find-continue-restart condition)))
                (flame condition :debug? debug?)
                (invoke-restart restart)))))
         ((&flet do-fail (condition)
            (when fail
              (funcall fail condition))
            (do-continue condition)))
         ((&flet do-abort (condition)
            (when-let ((restart (find-restart 'abort condition)))
              (flame condition :debug? debug?)
              (invoke-restart restart))))
         #+sbcl (main-thread sb-thread:*current-thread*)
         ((&flet do-debug (condition)
            (bt:with-recursive-lock-held (lock)
              (let (#+sbcl (sb-ext:*invoke-debugger-hook* nil))
                #+sbcl (unless (eq sb-thread:*current-thread* main-thread)
                         (bt:interrupt-thread main-thread #'sb-thread:release-foreground))
                (invoke-debugger condition))))))
    (lambda (condition)
      (typecase condition
        (commands::deferred-phase-error
         (bt:with-recursive-lock-held (lock)
           (format t "~A~2%" condition))
         (continue))
        (warning
         (bt:with-recursive-lock-held (lock)
           (call-with-condition-printing
            (curry #'pretty-print-condition condition)
            :color (when color? :yellow)))
         (muffle-warning)))

      (log:info "~@<Handling ~A~@[ (caused by ~A)~]:~@:_~A~@:>"
                (type-of condition)
                (let ((cause (more-conditions:root-cause condition)))
                  (unless (eq cause condition)
                    (type-of cause)))
                condition)
      (loop :for (condition-type . action) :in policy
            :do (log:debug "Considering rule: ~S → ~S" condition-type action)
            :when (typep condition condition-type)
              :do (log:info "Applying rule: ~S → ~S" condition-type action)
                  (ecase action
                    (:continue (do-continue condition))
                    (:fail     (do-fail     condition))
                    (:abort    (do-abort    condition))
                    (:debug    (do-debug    condition)))
                  (return)))))

(defun main ()
  (log:config :thread :warn)

  (let+ ((smart-terminal? (adapt-configuration-for-environment))
         (arguments       (uiop:command-line-arguments))
         ((&flet execute-command-and-quit (code command &rest args)
            (commands:command-execute
             (apply #'commands:make-command command args))
            (uiop:quit code)))
         (debugging? nil)
         (lock       (bt:make-recursive-lock "output and debug"))
         ((&flet die (condition &optional usage? context (brief? t))
            (bt:with-recursive-lock-held (lock)
              (when condition
                (call-with-condition-printing
                 (curry #'pretty-print-condition condition)
                 :color (when smart-terminal? :red))
                (when debugging?
                  #+sbcl (sb-debug:print-backtrace)))
              (if usage?
                  (apply #'execute-command-and-quit
                         1 :help :brief? brief?
                         (when (and context (not (equal context "global")))
                           (list :command context)))
                  (uiop:quit 1)))))
         ((&values option-value &ign configuration &ign
                   (&plist-r/o
                    (version? :version?) (help? :help?) (debug? :debug?)))
          (handler-bind ((commandline:option-argument-error
                           (lambda (condition)
                             (let ((cause (more-conditions:cause condition)))
                               (when (typep cause 'commandline:option-not-found-error)
                                 (let ((option (commandline:option cause)))
                                   (when (or (string= "-h" option)
                                             (string= "--help" option))
                                     (die nil t (commandline:context condition) nil)))))))
                         ((and error commandline:option-context-condition)
                          (lambda (condition)
                            (die condition t (commandline:context condition))))
                         (error (rcurry #'die t "global")))
            (process-configuration-and-commandline-arguments arguments))) ; TODO this calls configure-command but reported conditions are not right. e.g. report -D foo.distribution produces "The "-D" option requires a VARIABLE-NAME=VALUE argument." and the generic help. does not mention the command and does not print the command-specific help
         ((&flet option-value (&rest args)
            (apply option-value args))))
    (cond (version? (execute-command-and-quit 0 :version))
          (help?    (execute-command-and-quit 0 :help))
          (debug?   (setf debugging? t)))
    (handler-bind ((commands:command-not-found-error
                     (rcurry #'die t "global"))
                   (commands:command-configuration-problem
                     (lambda (condition)
                       (die condition t (commands:command condition))))
                   (error #'die)
                   #+sbcl (sb-sys:interactive-interrupt #'die))
      (let* ((command-configuration (options:sub-configuration
                                     "commands.**" configuration))
             (command               (commands:make-command
                                     command-configuration))
             (fail?                 nil))
        (commands:execute-command
         command
         :configuration   configuration
         :num-processes   (option-value "global" "num-processes")
         :error-policy    (make-error-policy
                           (option-value "global" "on-error")
                           :lock   lock
                           :debug? debugging?
                           :color? (option-value "global" "colored-output")
                           :fail   (lambda (condition)
                                     (declare (ignore condition))
                                     (setf fail? t)))
         :progress-style  (option-value "global" "progress-style")
         :temp-directory  (option-value "global" "temp-directory")
         :cache-directory (option-value "global" "cache-directory")
         :age-limit       (option-value "global" "cache-age-limit")
         :trace-variables (option-value "global" "trace-variable"))
        (uiop:quit (if fail? 1 0))))))

(eval-when (:load-toplevel)
  (var:check-variable-liveness))
