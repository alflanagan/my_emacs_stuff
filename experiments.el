;; -*- lexical-binding: t -*-

;;; for now, just a set of experimental stuff
(defun parse-bookmark-file (file-name) nil)
(require 'json)
(require 'time-date)
(require 'cl-lib)  ;;OK, maybe CL isn't all bad


(defconst test-bookmarks-file "bookmarks-2014-04-25.json"
  "A Firefox JSON export file for testing purposes")

(setq json-object (json-read-file test-bookmarks-file))

;;clean up previous output, if any
(with-current-buffer (get-buffer-create "*experiments*") (delete-region (point-min) (point-max)) )

(defun convert-moz-time (time-value)
  "Convert mozilla time value to standard emacs (HIGH LOW USEC PSEC)"
  (seconds-to-time (/ time-value 1000000.0)))

(defun format-moz-time-iso-8601 (time-value)
  "Format time-value from mozilla JSON to ISO 8601 standard format, return as string"
  (format-time-string "%FT%T%z"  (convert-moz-time time-value))
  )

(defun make-string (any-value)
  "Format any-value into a string. (Surely there's a standard function for this?)"
  (cond ((eql (type-of any-value) "string") any-value)
        ;;assume floating-point value is date if would convert to
        ;;later than 3/3/1973
        ((eql (type-of any-value) "float") (cond (and ((>  100000000000000.0 any-value))
                                                      ((<  2147450000000000.0 any-value)))
                                                 (format-moz-time-iso-8601  any-value)
                                                 (number-to-string any-value)))
        ((eql (type-of any-value) "integer") (number-to-string any-value)))
  )

(defun make-float-date (any-value)
  (cond (and (>  100000000000000.0 any-value)
             (<  2147450000000000.0 any-value))
        (format-moz-time-iso-8601  any-value)
        (t any-value)))

(defun my-output (&rest output-values)
  "Output text  by writing to my custom buffer"
  (setq outbuffer  (get-buffer-create "*experiments*"))
  (let ((output-strings (mapcar 'make-string output-values)))
    (setq output-strings (cl-remove-if 'null output-strings))
    (princ (type-of output-strings))
    (with-current-buffer outbuffer (goto-char (point-max)) (mapcar 'insert output-strings) (insert "\n"))))

(defun handle-bookmark (bookmark)
  ;;bookmark is a dotted pair
  (if (not (atom (cdr bookmark))) (error "Argument must be dotted pair, got %s" (type-of bookmark)))
  (pcase (car bookmark)
    (`title (my-output "Title is " (cdr bookmark)))
    (`id  (my-output "ID is " (number-to-string (cdr bookmark))))
    (`dateAdded  (my-output "Date added is " (format-moz-time-iso-8601 (cdr bookmark) )))
    (`lastModified (my-output "Date modified is " (format-moz-time-iso-8601 (cdr bookmark) )))
    (`root (my-output "root is " (cdr bookmark)))
    (`type (my-output "type is " (cdr bookmark)))
    (`children (my-output "children " (number-to-string (length (cdr bookmark))))))
  nil)

(mapcar 'handle-bookmark json-object)

;;(mapcar 'handle-bookmark '((fred . wilma) (title . "gone with the")))



