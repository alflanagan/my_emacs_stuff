;;EmacsWiki: http://www.emacswiki.org/emacs/ParEdit#toc6
;; by John Wiegley

(defun paredit-barf-all-the-way-backward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

  (defun paredit-barf-all-the-way-forward ()
    (interactive)
    (paredit-split-sexp)
    (paredit-forward-down)
    (paredit-splice-sexp)
    (if (eolp) (delete-horizontal-space)))

  (defun paredit-slurp-all-the-way-backward ()
    (interactive)
    (catch 'done
      (while (not (bobp))
        (save-excursion
          (paredit-backward-up)
          (if (eq (char-before) ?\()
              (throw 'done t)))
        (paredit-backward-slurp-sexp))))

  (defun paredit-slurp-all-the-way-forward ()
    (interactive)
    (catch 'done
      (while (not (eobp))
        (save-excursion
          (paredit-forward-up)
          (if (eq (char-after) ?\))
              (throw 'done t)))
        (paredit-forward-slurp-sexp))))

  (nconc paredit-commands
         '("Extreme Barfage & Slurpage"
           (("C-M-)")
                        paredit-slurp-all-the-way-forward
                        ("(foo (bar |baz) quux zot)"
                         "(foo (bar |baz quux zot))")
                        ("(a b ((c| d)) e f)"
                         "(a b ((c| d)) e f)"))
           (("C-M-}" "M-F")
                        paredit-barf-all-the-way-forward
                        ("(foo (bar |baz quux) zot)"
                         "(foo (bar|) baz quux zot)"))
           (("C-M-(")
                        paredit-slurp-all-the-way-backward
                        ("(foo bar (baz| quux) zot)"
                         "((foo bar baz| quux) zot)")
                        ("(a b ((c| d)) e f)"
                         "(a b ((c| d)) e f)"))
           (("C-M-{" "M-B")
                        paredit-barf-all-the-way-backward
                        ("(foo (bar baz |quux) zot)"
                         "(foo bar baz (|quux) zot)"))))

  (paredit-define-keys)
  (paredit-annotate-mode-with-examples)
  (paredit-annotate-functions-with-examples)
