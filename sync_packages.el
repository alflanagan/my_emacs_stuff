
;; Guarantee all packages are installed on start
(defvar packages-list
 '( autopair            ;;Automagically pair braces and quotes like TextMate
    bash-completion 	;;BASH completion for the shell buffer
    ;charmap             ;;Unicode table for Emacs -- sadly quite buggy
    dash                ;;A modern list library for Emacs
    discover            ;;discover more of Emacs
    ecb                 ;;a code browser for Emacs
    epl                 ;;Emacs Package Library
    fic-mode            ;;Show FIXME/TODO/BUG/KLUDGE in special face only in comments and strings
    flycheck            ;;On-the-fly syntax checking (Flymake done right)
    git-commit-mode     ;;Major mode for editing git commit messages
    git-rebase-mode     ;;mode for editing git rebase files
    icicles             ;;Minibuffer input completion and cycling.
    jedi                ;;Python auto-completion for Emacs
    magit               ;;Git from Emacs
    markdown-mode       ;;Emacs Major mode for Markdown-formatted text files
    pep8                ;;run the python pep8 checker putting hits in a grep buffer
    ;pylint              ;;minor mode for running `pylint' (superseded  by flycheck)
    python-environment  ;;virtualenv API for Emacs Lisp
    rainbow-delimiters  ;;Highlight nested parens, brackets, braces a different color at each depth.
    rainbow-mode          ;;Colorize color names in buffers
    slime               ;;Superior Lisp Interaction Mode for Emacs
    slime-repl          ;;Read-Eval-Print Loop written in Emacs Lisp
    zenburn-theme       ;;A low contrast color theme for Emacs.
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
