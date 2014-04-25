;;; sync-packages.el --- Ensure a set list of packages are installed on emacs startup.
;; -*- fill-column: 90; -*-
;;; Commentary:

;;; Code:
(require 'package)
(require 'cl-lib)
(package-initialize)

;; Guarantee all packages are installed on start
;; NOTE: this duplicates functionality of el-get, may switch
(defvar sync-packages-list
  '(
    ;; autopair            ;;Automagically pair braces and quotes like TextMate
    ;; bash-completion     ;;BASH completion for the shell buffer
    ;; cython-mode         ;;Major mode for editing Cython files
    dash                ;;A modern list library for Emacs
    dash-functional        ;;Collection of useful combinators for Emacs Lisp
    ;; discover            ;;discover more of Emacs
    ;; ecb                 ;;a code browser for Emacs
    ;; fuzzy               ;;dependency of elpy
    ;; elpy                ;;Emacs Python Development Environment
    el-get                  ;;Manage the external elisp bits and pieces you depend upon
    ;; epc                 ;;dependency of jedi
    ;; epl                 ;;Emacs Package Library
    flycheck               ;;On-the-fly syntax checking (Flymake done right)
    ;; goto-chg            ;;goto last change
    ;; icicles             ;;Minibuffer input completion and cycling.
    ;; jedi                ;;Python auto-completion for Emacs
    know-your-http-well    ;;Look up the meaning of HTTP headers, methods, relations,
    ;;status codes
    paradox                ;;Display Package Ratings on the *Packages* buffer.
    markdown-mode       ;;Emacs Major mode for Markdown-formatted text files
    paradox             ;;Display Package Ratings on the *Packages* buffer.
    ;; pep8                ;;run the python pep8 checker putting hits in a grep buffer
    popup               ;;Visual Popup User Interface
    ;; python-environment  ;;virtualenv API for Emacs Lisp
    rainbow-delimiters     ;;Highlight nested parens, brackets, braces a different color
    ;;at each depth.
    ;; rainbow-mode        ;;Colorize color names in buffers
    starter-kit            ;;Saner defaults and goodies.
    starter-kit-bindings   ;;Saner defaults and goodies: bindings
    starter-kit-eshell     ;;Saner defaults and goodies: eshell tweaks
    starter-kit-js         ;;Saner defaults and goodies for Javascript
    starter-kit-lisp       ;;Saner defaults and goodies for lisp languages
    starter-kit-ruby       ;;Saner defaults and goodies for Ruby
    ;;ternjs looks like it would be great, but setup is ...
    ;;problematic (http://ternjs.net)
    ;;tern-auto-complete     ;;Tern Completion by auto-complete.el
    ;;tern                   ;;Tern-powered JavaScript integration
    web-mode               ;;major mode for editing html templates
    yasnippet              ;;Yet another snippet extension for Emacs.

)
  "List of packages to verify at launch.")

(defun reduce-and (a_list)
  "Reduce a list using and -- i.e. return nil if any element is nil, else return
last element"
  ;;only needed because you can't (apply 'and (...))
  (if (null a_list)
      t
    (and (car a_list) (reduce-and (cdr a_list)))
    )
  )

(defun has-package-not-installed ()
  "Returns t if any package in sync-packages-list is not actually installed"
  (not (reduce-and (mapcar 'package-installed-p sync-packages-list)))
)

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (mapc (lambda (p) (if (not (package-installed-p p))
                        (package-install p))
          )
        sync-packages-list)
  )

(provide 'sync-packages)
;;; sync_packages.el ends here
