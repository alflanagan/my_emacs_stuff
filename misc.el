;;; misc.el -- some generally-useful functions

;;; Commentary:

;;; Code:

;;TODO: An implementation of list comprehension ala Python
;;(defmacro list-comprehension (expr list condition) ...)
;; "Generate a list by evaluating EXPR with each member of LIST that
;;satisfies CONDITION.
;;
;;EXPR should be an expression with a variable named "it".

(defun members-with-suffix (source-list suffix-string)
  "Return a list composed of every member of SOURCE-LIST for which `string-suffix-p' detects suffix SUFFIX-STRING."
  (if (null source-list)
      nil
    (if (string-suffix-p suffix-string (car source-list))
        (cons (car source-list) (members-with-suffix (cdr  source-list) suffix-string))
      (members-with-suffix (cdr source-list) suffix-string))))

(defun file-name-paths (directory file-list)
  "Return the list created by prepending DIRECTORY to each member FILE-LIST."
  (mapcar (lambda (it) (expand-file-name it directory))
          file-list))

(provide 'misc)
;;; misc.el ends here

