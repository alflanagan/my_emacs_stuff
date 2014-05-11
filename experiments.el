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

(defun my-make-output-string (any-value)
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

;;(my-make-output-string "title") ==> "title"
;;(my-make-output-string 7) ==> "7"
;;(my-make-output-string 7.0) ==> "7.0"
;;(my-make-output-string 1137450000000000.0) ==> "2006-01-16T17:20:00-0500"
;;(my-make-output-string '(1 2)) ==> "unknown type"

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

(defun my-output
    (&rest output-values)
  "Output text  by writing to my custom buffer"
  (let
      ((output-strings
        (mapcar 'my-make-output-string output-values)))
    (setq output-strings
          (cl-remove-if 'null output-strings))
    (apply 'my-output-strings output-strings)))


;;; bookmarklet for displaying bookmark json files in browser:
;; javascript:
;; (function(){var E=document.getElementsByTagName('PRE')[0],
;;          T=E.innerHTML,
;;          i=0,r1,r2;
;;          t=new Array();
;;          while(/("uri":"([^"]*)")/g.exec(T)){
;;            r1=RegExp.$1;
;;            r2=RegExp.$2;
;;            if(/^https?:/.exec(r2)){
;;              t[i++]='['+(i)+']:<a href='+r2+'>'+r2+'<\/a>';
;;            }
;;          } with (window.open().document){
;;            for(i=0;t[i];i++)
;;              write(t[i]+'<br>');
;;            close();
;;          }}
;; )();


(defun handle-bookmark (bookmark)
  ;;bookmark is a dotted pair
  (if (not (atom (cdr bookmark))) (error "Argument must be dotted pair, got %s" (type-of bookmark)))
  (pcase (car bookmark)
    (`title (my-output "Title is " (cdr bookmark) "\n"))
    (`id  (my-output "ID is " (number-to-string (cdr bookmark)) "\n"))
    (`dateAdded  (my-output "Date added is " (format-moz-time-iso-8601 (cdr bookmark)) "\n"))
    (`lastModified (my-output "Date modified is " (format-moz-time-iso-8601 (cdr bookmark)) "\n"))
    (`root (my-output "root is " (cdr bookmark) "\n"))
    (`type (my-output "type is " (cdr bookmark) "\n")
           (if (equal (cdr bookmark) "text/x-moz-place")
               (my-output "found place")))
    ;; type serves to distinguish 'object' classes
    ;; Known values:
    ;;  1 (looks like internal directives: read-only?)
    ;;  3 properties: see "name" field for which property
    ;; text/x-moz-place -- normal bookmark entry/URL
    ;; text/x-moz-place-container
    ;; text/x-moz-place-separator

    (`children (my-output "children " (number-to-string (length (cdr bookmark))) "\n")
               (my-output (pp-to-string (mapcar (lambda (x) (list (car x) (cadr x))) (cdr bookmark))))
               ))
  nil)

(mapc 'handle-bookmark json-object)


(cl-defstruct x-moz-place uri type lastModified dateAdded parent id title index)

;;(make-x-moz-place) ==> [cl-struct-x-moz-place nil nil nil nil nil nil nil nil]
;;(make-x-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
;;                   :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
;;                   :index 9) ==> [cl-struct-x-moz-place "http://www.example.com" "text"
;;                   1.340392082e+15 1.340391622e+15 3860 4153 "NINA - Devbox" 9]

(setq test-bookmark
     (make-x-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
                        :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
                        :index 9))
;; (setq test-bookmark2 (copy-x-moz-place test-bookmark))
;; (x-moz-place-p test-bookmark2) ==> t
;;(equal test-bookmark test-bookmark2) ==> t
;;(eq test-bookmark test-bookmark2) ==> nil
;;(x-moz-place-title test-bookmark) ==> "NINA - Devbox"
;;(x-moz-place-type test-bookmark) ==> "text"
