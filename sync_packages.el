(require 'cl)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(autopair            ;;Automagically pair braces and quotes like TextMate
    zenburn-theme       ;;A low contrast color theme for Emacs.
    flycheck            ;;On-the-fly syntax checking (Flymake done right)
    ;fic-mode            ;;Show FIXME/TODO/BUG/KLUDGE... (not working; why?)
    fic-ext-mode        ;;Show FIXME/TODO/BUG/KLUDGE... in special face only in comments and strings
    epl                 ;;Emacs Package Library
    ecb                 ;;a code browser for Emacs
    ;charmap             ;;Unicode table for Emacs -- sadly quite buggy
    bash-completion 	;;BASH completion for the shell buffer
    dash                ;;A modern list library for Emacs
    discover            ;;discover more of Emacs
    goto-chg            ;;goto last change
    python-environment  ;;virtualenv API for Emacs Lisp
    jedi                ;;Python auto-completion for Emacs
    git-commit-mode     ;;Major mode for editing git commit messages
    git-rebase-mode     ;;mode for editing git rebase files
    icicles             ;;Minibuffer input completion and cycling.
    magit               ;;Git from Emacs
    pep8                ;;run the python pep8 checker putting hits in a grep buffer
    ;pylint              ;;minor mode for running `pylint' (superseded  by flycheck)
    rainbow-mode          ;;Colorize color names in buffers
    rainbow-delimiters  ;;Highlight nested parens, brackets, braces a different color at each depth.
    slime               ;;Superior Lisp Interaction Mode for Emacs
    slime-repl          ;;Read-Eval-Print Loop written in Emacs Lisp
    markdown-mode       ;;Emacs Major mode for Markdown-formatted text files
)
  "List of packages to verify at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))
