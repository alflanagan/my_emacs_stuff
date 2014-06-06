;;; sync-packages.el --- Ensure a set list of packages are installed on emacs startup.
;; -*- fill-column: 120; -*-
;;; Commentary:

;;; Code:
(require 'package)
(require 'cl-lib)
(package-initialize)

;; '(package-archives
;;   '(("gnu" . "http://elpa.gnu.org/packages/")
;;     ("melpa" . "http://melpa.milkbox.net/packages/")
;;     ("elpy" . "http://jorgenschaefer.github.io/packages/")))

;; Guarantee all packages are installed on start
(defvar sync-packages-list
  '(
    ;;;Packages to auto-install
    auto-complete          ;; Auto Completion for GNU Emacs
    elpy                   ;; Emacs Python Development Environment
    flycheck               ;; On-the-fly syntax checking (Flymake done right)
    ipretty                ;; Interactive Emacs Lisp pretty-printing
    json-mode              ;; Major mode for editing JSON files
    markdown-mode          ;; Emacs Major mode for Markdown-formatted text files
    paradox                ;; Display Package Ratings on the *Packages* buffer.
    popup                  ;; Visual Popup User Interface
    rainbow-delimiters     ;; Highlight nested parens, brackets, braces a different color at each depth.
    starter-kit            ;; Saner defaults and goodies.
    starter-kit-bindings   ;; Saner defaults and goodies: bindings
    starter-kit-eshell     ;; Saner defaults and goodies: eshell tweaks
    starter-kit-js         ;; Saner defaults and goodies for Javascript
    starter-kit-lisp       ;; Saner defaults and goodies for lisp languages
    starter-kit-ruby       ;; Saner defaults and goodies for Ruby
    undo-tree              ;; Treat undo history as a tree
    web-mode               ;; major mode for editing html templates
    yasnippet              ;; Yet another snippet extension for Emacs.

    ;; note that to *properly* detect packages installed but not
    ;; included in sync-packages-list, we'll need to store
    ;; dependencies or use code to determine them.

    ;; listing dependencies protects against uninstall of dependent
    ;; package (which the package system should detect and warn about,
    ;; but.. baby steps)

    ;;; dependencies of starter-kit
    
    find-file-in-project  ;;Find files in a project quickly.
    git-commit-mode       ;;Major mode for editing git commit messages
    git-rebase-mode       ;;Major mode for editing git rebase files
    idle-highlight-mode   ;;highlight the word the point is on
    ido-ubiquitous        ;;Use ido (nearly) everywhere.
    magit                 ;;control Git from Emacs
    paredit               ;;minor mode for editing parentheses
    smex                  ;;M-x interface with Ido-style fuzzy matching.

    ;;; dependencies of flycheck
    
    dash                  ;;A modern list library for Emacs
    epl                   ;;Emacs Package Library
    f                     ;;Modern API for working with files and directories
    pkg-info              ;;Information about packages
    s                     ;;The long lost Emacs string manipulation library.

    ;;; dependencies of starter-kit-lisp
    
    elisp-slime-nav       ;;Make M-. and M-, work in elisp like they do in slime

    ;;; dependency of starter-kit-ruby

    inf-ruby               ;;Run a Ruby process in a buffer

    ;;; dependency of json-mode

    json-reformat          ;;Reformatting tool for JSON

    ;;; dependencies of elpy
    
    ;; auto-complete
    ;; find-file-in-project
    ;; yasnippet
    fuzzy                  ;;Fuzzy Matching
    highlight-indentation  ;;Minor modes for highlighting indentation
    idomenu                ;;imenu tag selection a la ido
    iedit                  ;;Edit multiple regions in the same way simultaneously.
    nose                   ;;Easy Python test running in Emacs
    pyvenv                 ;;Python virtual environment interface

    ;;; dependencies of paradox

    tabulated-list         ;;generic major mode for tabulated lists.

    ;;; other packages of interest but not automatically installed
    
    ;; bash-completion        ;;BASH completion for the shell buffer
    ;; cython-mode            ;;Major mode for editing Cython files
    ;; discover               ;;discover more of Emacs
    ;; ecb                    ;;a code browser for Emacs
    ;; el-get                 ;;Manage the external elisp bits and pieces you depend upon
    ;; goto-chg               ;;goto last change
    ;; helm                   ;; Helm is an Emacs incremental and narrowing framework
    ;; icicles                ;;Minibuffer input completion and cycling.
    ;; jedi                   ;;Python auto-completion for Emacs
    ;; outline-magic          ;;outline mode extensions for Emacs
    ;; pep8                   ;;run the python pep8 checker putting hits in a grep buffer
    ;; python-environment     ;;virtualenv API for Emacs Lisp
    ;; rainbow-mode           ;;Colorize color names in buffers
    ;; sphinx-doc             ;;Sphinx friendly docstrings for Python functions

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

(defun get-dependent-packages (package-name)
  "Returns list of packages which are automatically installed if package package-name is installed."
  (popup-tip "I don't know how to implement this yet."))

(defun packages-not-in-sync-list ()
  "Incomplete function to create a list of package names whose
package is not built-in and not in sync-packages-list"
  (message "%s" "Not implemented yet"))


(defun quick-popup (message)
  (let ((popup (popup-create (point) 10 10))
        )
    (popup-set-list popup '(message))
    (popup-draw popup)
    (sleep-for 5)
    (popup-delete popup)))

;;(quick-popup "this is a test")  ;; doesn't work

(defun do-with-installed-package (package something)
  "calls function something if package is not a built-in package"
  (if (not (package-built-in-p package))
      (funcall something package)))

;;print names of all installed packages which are not built-in
(defun print-installed-packages ()
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
                 (princ "   "))))))))

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

(defun install-missing-packages ()
  "Install each package in sync-packages-list which is not installed."
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
     sync-packages-list)))

;;; OK, this will get a list of all the package names known to system
;; (let ((packages-found '()))
;;   (package--mapc (lambda (pkg-desc) (setq packages-found (append packages-found  (list (package-desc-name pkg-desc)) ))))
;;   packages-found
;;   )

(provide 'sync-packages)
;;; sync_packages.el ends here
