;;; file-mode-exp -- test setting up emacs to recognize "node"
;;; interpreter file as javascript

;;; Commentary:

;;; Code:

;; alternative implementation, because i thought it was interesting
;; (condition-case nil
;;     (setf (cdr (assoc "fred" interpreter-mode-alist)) "wilma")
;;   (wrong-type-argument (setq interpreter-mode-alist
;;                              (append interpreter-mode-alist (list (cons "fred" "wilma"))))))

(defun file-mode-exp-set-interpreter-mode (interpreter-string major-mode)
  "When a file's interpreter is INTERPRETER-STRING, set MAJOR-MODE.

See Info node `(elisp)Auto Major Mode' and variable `interpreter-mode-alist'."
  (if (assoc interpreter-string interpreter-mode-alist)
      ;; already in list, replace its value
      (setf (cdr (assoc interpreter-string interpreter-mode-alist)) major-mode)
    ;; not in, so add it
    (setq interpreter-mode-alist
            (append interpreter-mode-alist
                    (list (cons interpreter-string major-mode))))))

;; (file-mode-exp-set-interpreter-mode "node" 'js2-mode)

(provide 'file-mode-exp)

;;; file-mode-exp.el ends here
