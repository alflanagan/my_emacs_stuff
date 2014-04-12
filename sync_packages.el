;;; sync-packages.el --- Ensure a set list of packages are installed on emacs startup.

;;; Commentary:

;;; Code:
(require 'package)
(require 'cl-lib)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(
    arduino-mode          ;;Major mode for the Arduino language
    autopair            ;;Automagically pair braces and quotes like TextMate
    bash-completion 	;;BASH completion for the shell buffer
    ;charmap             ;;Unicode table for Emacs -- sadly quite buggy
    cython-mode         ;;Major mode for editing Cython files
    dash                ;;A modern list library for Emacs
    discover            ;;discover more of Emacs
    ecb                 ;;a code browser for Emacs
    elpy                  ;;Emacs Python Development Environment
    epl                 ;;Emacs Package Library
    ;fic-mode            ;;Show FIXME/TODO/BUG/KLUDGE... (not working; why?)
    fic-ext-mode        ;;Show FIXME/TODO/BUG/KLUDGE... in special face only in comments and strings
    flycheck            ;;On-the-fly syntax checking (Flymake done right)
    git-commit-mode     ;;Major mode for editing git commit messages
    git-rebase-mode     ;;mode for editing git rebase files
    goto-chg            ;;goto last change
    icicles             ;;Minibuffer input completion and cycling.
    jedi                ;;Python auto-completion for Emacs
    magit               ;;Git from Emacs
    markdown-mode       ;;Emacs Major mode for Markdown-formatted text files
    pep8                ;;run the python pep8 checker putting hits in a grep buffer
    ;pylint              ;;minor mode for running `pylint' (superseded  by flycheck)
    python-environment  ;;virtualenv API for Emacs Lisp
    rainbow-delimiters  ;;Highlight nested parens, brackets, braces a different color at each depth.
    rainbow-mode        ;;Colorize color names in buffers
    slime               ;;Superior Lisp Interaction Mode for Emacs
    slime-repl          ;;Read-Eval-Print Loop written in Emacs Lisp
    zenburn-theme       ;;A low contrast color theme for Emacs.
)
  "List of packages to verify at launch.")

;;replaced ugly common-lisp macro form from source with
(defun has-package-not-installed ()
  "Define a version of has-package-not-installed which does not use ugly cl macros."
  ;;except reduce, that is. haven't found a replacement.
  (cl-reduce (lambda (a b) (and a b))  (mapcar 'package-installed-p packages-list))
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
        packages-list)
  )

(provide 'sync-packages)
;;; sync_packages.el ends here
