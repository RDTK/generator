;;;; lsp-builder-generator.el --- Language Server Protocol for the build-generator

;; Copyright (C) 2017, 2018, 2019 Jan Moringen

;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Maintainer: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Package-Requires: ((emacs "24.1")
;;                    (yaml-mode)
;;                    (lsp-mode))
;; Package-Version: 20180409.607
;; Keywords: lsp yaml recipe jenkins build-generator
;; Version: 0.0.1


;;; Commentary:

(require 'derived)
(require 'yaml-mode)

(require 'lsp-mode)


;;; Code:

;;; Language Server Protocol

(defcustom lsp-build-generator-program
  "~/opt/build-generator/bin/build-generator"
  "Path of the build-generator program to use as language server."
  :group 'build-generator
  :type  'pathname
  :link  '(url-link "https://github.com/rdtk/generator"))

(lsp-register-client
 (make-lsp-client
  :server-id      'build-generator
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `("sh" "-c"
                       ,(format "BUILD_GENERATOR_CONFIG_FILES= %s language-server"
                                (expand-file-name lsp-build-generator-program)))))
  :major-modes    '(template-recipe-mode
                    project-recipe-mode
                    distribution-recipe-mode
                    person-recipe-mode)))

;;; Language Modes

(defmacro lsp-build-generator--define-language-mode (type)
  "Define a recipe mode for TYPE."
  (let* ((mode-name      (concat type "-recipe-mode"))
         (function-name  (intern mode-name))
         (pretty-name    (capitalize type))
         (language-name  (concat type "-recipe"))
         (hook-name      (intern (concat mode-name "-hook")))
         (file-extension type)
         (filename-regex (concat "\\." file-extension "\\'")))
    `(progn
       (define-derived-mode ,function-name yaml-mode ,pretty-name)

       (add-to-list 'auto-mode-alist '(,filename-regex . ,function-name))

       (push '(,function-name . ,language-name)
             lsp-language-id-configuration))))

(lsp-build-generator--define-language-mode "template")
(lsp-build-generator--define-language-mode "project")
(lsp-build-generator--define-language-mode "distribution")
(lsp-build-generator--define-language-mode "person")

;;; lsp-build-generator.el ends here
