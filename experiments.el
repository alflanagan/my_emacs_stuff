;; -*- lexical-binding: t -*-

;;; for now, just a set of experimental stuff
(require 'json)
(require 'time-date)
(require 'cl-lib)  ;;OK, maybe CL isn't all bad

(defconst test-bookmarks-file "bookmarks-2014-04-25.json"
  "A Firefox JSON export file for testing purposes")

(setq json-object (json-read-file test-bookmarks-file))

;;clean up previous output, if any
(with-current-buffer (get-buffer-create "*experiments*")
  (delete-region (point-min) (point-max)) )

(defun convert-moz-time (time-value)
  "Convert mozilla time value to standard emacs (HIGH LOW USEC PSEC)"
  (seconds-to-time (/ time-value 1000000.0)))

;;(convert-moz-time  100000000000001.0) ==> (1525 57600 0 998378)
;;(convert-moz-time  100000000000001) ==> (1525 57600 0 998378)
;;(convert-moz-time 0.0) ==> (0 0 0 0)

(defun format-moz-time-iso-8601 (time-value)
  "Format time-value from mozilla JSON to ISO 8601 standard format, return as string"
  (format-time-string "%FT%T%z"  (convert-moz-time time-value))
  )

;;(format-moz-time-iso-8601 0.0) ==> "1969-12-31T19:00:00-0500"
;;(format-moz-time-iso-8601 (* (float-time) 1000000.0)) ==> "2014-05-06T17:57:43-0400" (YMMV)

(defun fuzzy-float-string (any-float)
  "Format a float as a date string if it would result in a
reasonable value, as float otherwise"
  ;;assume floating-point value is date if would convert to
  ;;later than 3/3/1973
  (if (and (> any-float  1.0e14)
           (< any-float 2.14745e15))
      (format-moz-time-iso-8601 any-float)
    (number-to-string any-float)))

;;(fuzzy-float-string 7.0) ==> "7.0"
;;(fuzzy-float-string  100000000000001.0) ==> "1973-03-03T04:46:40-0500"
;;(fuzzy-float-string  2147449999999999.0) ==> "2038-01-18T12:53:19-0500"
;;(fuzzy-float-string  2147450000000000.0) ==> "2.14745e+15"

(defun make-string (any-value)
  "Format any-value into a string. (Surely there's a standard function for this?)"
  (cond ((equal (type-of any-value) 'string)
          any-value)
        ((equal (type-of any-value) 'integer)
         (number-to-string any-value))
        ((equal (type-of any-value) 'float)
         (fuzzy-float-string any-value))
        ('t
         "unknown type")
        )
  )

;;(make-string "title") ==> "title"
;;(make-string 7) ==> "7"
;;(make-string 7.0) ==> "7.0"
;;(make-string 1137450000000000.0) ==> "2006-01-16T17:20:00-0500"
;;(make-string '(1 2)) ==> "unknown type"

(defun my-output-string (a-string)
  "Writes a single string or character to the output-buffer *experiments*"
  (let ((outputbuffer (get-buffer-create "*experiments*")))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (insert a-string)
      )
    )
  )

;;(my-output-string "fred") --> |fred
;;(my-output-string 7) --> |^G

(defun my-output-strings (&rest output-strings)
  "Write a list of strings to the output buffer *experiments*"
  (let ((outputbuffer (get-buffer-create "*experiments*")))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (mapc 'insert output-strings)
      )
    )
  )

;;(my-output-strings "fred" "barney" "wilma") --> fredbarneywilma

(defun my-output (&rest output-values)
  "Output text  by writing to my custom buffer"
  (setq outbuffer  (get-buffer-create "*experiments*"))
  (let ((output-strings (mapcar 'make-string output-values)))
    (setq output-strings (cl-remove-if 'null output-strings))
    (with-current-buffer outbuffer (goto-char (point-max)) (mapc 'insert output-strings) (insert "\n"))))

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

(mapc 'handle-bookmark json-object)

