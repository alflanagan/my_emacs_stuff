;;; file-mode-exp -- test setting up emacs to recognize "node"
;;; interpreter file as javascript

;;; Commentary:

;;; Code:

(if (assoc "jruby" interpreter-mode-alist)
    (setf (cdr (assoc "jruby" interpreter-mode-alist)) 'js2-mode))

(defun set-interpreter-mode (interpreter-string major-mode)
  (if (assoc interpreter-string interpreter-mode-alist)
      (setf (cdr (assoc interpreter-string interpreter-mode-alist)) major-mode)
    (setq interpreter-mode-alist (append (cons interpreter-string  major-mode) interpreter-mode-alist))
    )
  )


(set-interpreter-mode "node" 'js2-mode)

(assoc "jruby" interpreter-mode-alist)
