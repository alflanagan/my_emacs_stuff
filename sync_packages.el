;;; sync-packages.el --- Ensure a set list of packages are installed on emacs startup.
;; -*- fill-column: 120; -*-
;;; Commentary:

;;; Code:
(require 'package)
(require 'cl-lib)
(package-initialize)

;; Guarantee all packages are installed on start
;; NOTE: this duplicates functionality of el-get, may switch
(defvar sync-packages-list
  '(
    ;; autopair               ;;Automagically pair braces and quotes like TextMate
    ;; bash-completion        ;;BASH completion for the shell buffer
    ;; cython-mode            ;;Major mode for editing Cython files
    ;; discover               ;;discover more of Emacs
    ;; ecb                    ;;a code browser for Emacs
    ;; elpy                   ;;Emacs Python Development Environment
    ;; epl                    ;;Emacs Package Library
    ;; fuzzy                  ;;dependency of elpy
    ;; goto-chg               ;;goto last change
    ;; icicles                ;;Minibuffer input completion and cycling.
    ;; jedi                   ;;Python auto-completion for Emacs
    ;; pep8                   ;;run the python pep8 checker putting hits in a grep buffer
    ;; python-environment     ;;virtualenv API for Emacs Lisp
    ;; rainbow-mode           ;;Colorize color names in buffers
    ;; ternjs looks like it would be great, but setup is ... problematic (http://ternjs.net)
    ;; tern                   ;;Tern-powered JavaScript integration
    ;; tern-auto-complete     ;;Tern Completion by auto-complete.el
    dash                   ;;A modern list library for Emacs
    ;;dash-functional        ;;Collection of useful combinators for Emacs Lisp
    ;;el-get                 ;;Manage the external elisp bits and pieces you depend upon
    flycheck               ;;On-the-fly syntax checking (Flymake done right)
    json-mode              ;;Major mode for editing JSON files
    ;;know-your-http-well    ;;Look up the meaning of HTTP headers, methods, relations, status codes
    markdown-mode          ;;Emacs Major mode for Markdown-formatted text files
    paradox                ;;Display Package Ratings on the *Packages* buffer.
    popup                  ;;Visual Popup User Interface
    rainbow-delimiters     ;;Highlight nested parens, brackets, braces a different color at each depth.
    starter-kit            ;;Saner defaults and goodies.
    starter-kit-bindings   ;;Saner defaults and goodies: bindings
    starter-kit-eshell     ;;Saner defaults and goodies: eshell tweaks
    starter-kit-js         ;;Saner defaults and goodies for Javascript
    starter-kit-lisp       ;;Saner defaults and goodies for lisp languages
    starter-kit-ruby       ;;Saner defaults and goodies for Ruby
    web-mode               ;;major mode for editing html templates
    yasnippet              ;;Yet another snippet extension for Emacs.
)
  "List of packages to verify at launch, and install if not present.")

(defun reduce-and (a_list)
  "Reduce a list using and -- i.e. return nil if any element is nil, else return
last element"
  ;;only needed because you can't (apply 'and (...))
  (if (null a_list)
      t
    (and (car a_list) (reduce-and (cdr a_list)))
    )
  )

(defun has-package-not-installed
    ()
  "Returns t if any package in sync-packages-list is not actually installed"
  (not
   (reduce-and
    (mapcar 'package-installed-p sync-packages-list)))
  )

(when
    (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (mapc
   (lambda
     (p)
     (if
         (not
          (package-installed-p p))
         (package-install p))
     )
   sync-packages-list)
  )

(defun packages-not-in-sync-list ()
  "Incomplete function to create a list of package names whose
package is not built-in and not in sync-packages-list"
  (message "%s" "Not implemented yet"))

(defun do-with-installed-package (package something)
  "calls function something if package is not a built-in package"
  (if (not (package-built-in-p package))
      (funcall something package)))

;;print names of all installed packages which are not built-in
(package--mapc
 (lambda
   (x)
   (if
       (and
        (package-installed-p x)
        (not
         (package-built-in-p x)))
       (let
           ((package-name
             (package-desc-name x)))
         (if
             (not
              (memq package-name sync-packages-list))
             (progn
               (princ package-name)
               (princ "   ")))
         )
     )
   )
 )

(defun append-if-installed
    (pkg-desc pkg-list)
  "If pkg-desc is an installed (but not built-in) package, append its name to pkg-list."
  (if
      (and
       (package-installed-p pkg-desc)
       (not
        (package-built-in-p pkg-desc)))
      (append
       (pkg-desc-name pkg-desc)
       pkg-list)
    )
  )


;;; This almost tests append-if-installed
;; (setq a '())
;; (setq all-packages `())
;; (package--mapc (lambda (pkg-desc) (setq all-packages (append (list (package-desc-name pkg-desc)) all-packages))))
;; (setq first-package (car all-packages))
;; (append-if-installed first-package a)

;;; OK, this will get a list of all the package names known to system
;; (let ((packages-found '()))
;;   (package--mapc (lambda (pkg-desc) (setq packages-found (append packages-found  (list (package-desc-name pkg-desc)) ))))
;;   packages-found
;;   )

(provide 'sync-packages)
;;; sync_packages.el ends here
