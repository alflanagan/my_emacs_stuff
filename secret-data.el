;;; secret-data -- a small simple package to get/save key-value pairs
;;; of information I don't want to actually store in repository, like
;;; passwords/keys, etc.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defcustom secret-data-store-file "~/.emacs-secret"
  "The file name used to store secret (well, semi-secret) data."
  :group 'secret-data
  :type 'file
  :risky t
  )

(defun secret-data-parse ()
  "Parse contents of current buffer, which should contain lines of the form KEY=VALUE, into a standard a-list."
  (mapcar
   (lambda (x) (list (car (split-string x "="))
                ;;value may have embedded equal signs; put them back
                (string-join (cdr (split-string x "=")) "=")))
   (split-string (buffer-string) "[\f\n\r]+"))
  )

(defun secret-data-get-key (some-key &optional file-name)
  "Retrieve the value associated with SOME-KEY from the data stored in FILE-NAME.  FILE-NAME defaults to custom variable `secret-data-store-file'."
  (condition-case err-data
      (with-temp-buffer
        (insert-file-contents (expand-file-name "~/.emacs-secret"))
        (cadr (assoc some-key (secret-data-parse)))
        )
    (file-error (message "Can't get secret data with key [%s]: can't open file %s: (%s)."
                         some-key
                         (expand-file-name secret-data-store-file)
                         (cl-caddr err-data))))
  )

(secret-data-get-key "paradox-github-token")
(secret-data-get-key "pinboard-password")
(secret-data-get-key "this-is-a-test")
(secret-data-get-key "test-whitespace")

(defun secret-data-store-value (some-key some-value)
  "Update the data store so that retrieval of SOME-KEY will provide value SOME-VALUE."
  
  "data store goes here")

(provide 'secret-data)

;;; secret-data.el ends here
