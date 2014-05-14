;; -*- lexical-binding: t -*-
;; namespace prefix for this file: lbkmk-

;;; for now, just a set of experimental stuff
(eval-when-compile (require 'json))
(eval-when-compile (require 'time-date))
(eval-when-compile (require 'cl-lib)) ;;OK, maybe CL isn't all bad

(defconst lbkmk-test-bookmarks-file "bookmarks-2014-04-25.json"
  "A Firefox JSON export file for testing purposes")

(setq lbkmk-json-object (json-read-file lbkmk-test-bookmarks-file))

;;clean up previous output, if any
(with-current-buffer (get-buffer-create "*experiments*")
  (delete-region (point-min) (point-max)) )

(defun lbkmk-convert-moz-time (time-value)
  "Convert mozilla time value to standard emacs (HIGH LOW USEC PSEC)"
  (seconds-to-time (/ time-value 1000000.0)))

(defun lbkmk-format-moz-time-iso-8601 (time-value)
  "Format time-value from mozilla JSON to ISO 8601 standard format, return as string"
  (format-time-string "%FT%T%z"  (lbkmk-convert-moz-time time-value))
  )

(defun lbkmk-fuzzy-float-str (any-float)
  "Format a float as a date string if it would result in a
reasonable value, as float otherwise"
  ;;assume floating-point value is date if would convert to
  ;;later than 3/3/1973
  (if (and (> any-float  1.0e14)
           (< any-float 2.14745e15))
      (lbkmk-format-moz-time-iso-8601 any-float)
    (number-to-string any-float)))

(defun lbkmk-make-str (any-value)
  "Format any-value into a string. (Surely there's a standard function for this?)"
  (cond ((equal (type-of any-value) 'string)
          any-value)
        ((equal (type-of any-value) 'integer)
         (number-to-string any-value))
        ((equal (type-of any-value) 'float)
         (lbkmk-fuzzy-float-str any-value))
        ('t
         "unknown type")
        )
  )

;;(lbkmk-make-str "title") ==> "title"
;;(lbkmk-make-str 7) ==> "7"
;;(lbkmk-make-str 7.0) ==> "7.0"
;;(lbkmk-make-str 1137450000000000.0) ==> "2006-01-16T17:20:00-0500"
;;(lbkmk-make-str '(1 2)) ==> "unknown type"

(defun lbkmk-output-str (a-string)
  "Writes a single string or character to the output-buffer *experiments*"
  (let ((outputbuffer (get-buffer-create "*experiments*")))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (insert a-string)
      )
    )
  )

;;(lbkmk-output-str "fred") --> |fred
;;(lbkmk-output-str 7) --> |^G

(defun lbkmk-output-strs (&rest output-strings)
  "Write a list of strings to the output buffer *experiments*"
  (let ((outputbuffer (get-buffer-create "*experiments*")))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (mapc 'insert output-strings)
      )
    )
  )

;;(lbkmk-output-strs "fred" "barney" "wilma") --> fredbarneywilma

(defun lbkmk-output
    (&rest output-values)
  "Output text  by writing to my custom buffer"
  (let
      ((output-strings
        (mapcar 'lbkmk-make-str output-values)))
    (setq output-strings
          (cl-remove-if 'null output-strings))
    (apply 'lbkmk-output-strs output-strings)))


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


;; (defun lbkmk-handle-bookmark (bookmark)
;;   ;;bookmark is a dotted pair
;;   (if (not (atom (cdr bookmark))) (error "Argument must be dotted pair, got %s" (type-of bookmark)))
;;   (pcase (car bookmark)
;;     (`title (lbkmk-output "Title is " (cdr bookmark) "\n"))
;;     (`id  (lbkmk-output "ID is " (number-to-string (cdr bookmark)) "\n"))
;;     (`dateAdded  (lbkmk-output "Date added is " (lbkmk-format-moz-time-iso-8601 (cdr bookmark)) "\n"))
;;     (`lastModified (lbkmk-output "Date modified is " (lbkmk-format-moz-time-iso-8601 (cdr bookmark)) "\n"))
;;     (`root (lbkmk-output "root is " (cdr bookmark) "\n"))
;;     (`type (lbkmk-output "type is " (cdr bookmark) "\n")
;;            (if (equal (cdr bookmark) "text/x-moz-place")
;;                (lbkmk-output "found place")))
;;     ;; type serves to distinguish 'object' classes
;;     ;; Known values:
;;     ;;  1 (looks like internal directives: read-only?)
;;     ;;  3 properties: see "name" field for which property
;;     ;; text/x-moz-place -- normal bookmark entry/URL
;;     ;; text/x-moz-place-container
;;     ;; text/x-moz-place-separator

;;     (`children (lbkmk-output "children " (number-to-string (length (cdr bookmark))) "\n")
;;                (lbkmk-output (pp-to-string (mapcar (lambda (x) (list (car x) (cadr x))) (cdr bookmark))))
;;                ))
;;   ;;children is a vector of lists
;;   (lbkmk-output (pp-to-string bookmark))
;;   nil)

;; (mapc 'lbkmk-handle-bookmark lbkmk-json-object)


(cl-defstruct lbkmk-moz-place uri type lastModified dateAdded parent id title index)

;;(make-lbkmk-moz-place) ==> [cl-struct-lbkmk-moz-place nil nil nil nil nil nil nil nil]
;;(make-lbkmk-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
;;                   :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
;;                   :index 9) ==> [cl-struct-lbkmk-moz-place "http://www.example.com" "text"
;;                   1.340392082e+15 1.340391622e+15 3860 4153 "NINA - Devbox" 9]

(setq lbkmk-test-bookmark
     (make-lbkmk-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
                        :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
                        :index 9))
;; (setq lbkmk-test-bookmark2 (copy-lbkmk-moz-place lbkmk-test-bookmark))
;; (lbkmk-moz-place-p lbkmk-test-bookmark2) ==> t
;;(equal lbkmk-test-bookmark lbkmk-test-bookmark2) ==> t
;;(eq lbkmk-test-bookmark lbkmk-test-bookmark2) ==> nil
;;(lbkmk-moz-place-title lbkmk-test-bookmark) ==> "NINA - Devbox"
;;(lbkmk-moz-place-type lbkmk-test-bookmark) ==> "text"

(defun lbkmk-make-lbkmk-moz-place-list (list-of-lists)
  (let (url lastModified dateAdded parent id title index)
    (mapc (lambda (bkmk-assoc) (pcase (car bkmk-assoc)
                            (`title (setq title (cdr bkmk-assoc)))
                            (`uri (setq url (cdr bkmk-assoc)))
                            (`type (if (not (equal (cdr bkmk-assoc) "text/x-moz-place"))
                                       (error "lbkmk-make-lbkmk-moz-place-list got record of type %s" (cdr bkmk-assoc))))
                            (`parent (setq parent (cdr bkmk-assoc)))
                            (`id (setq id (cdr bkmk-assoc)))
                            (`dateAdded (setq dateAdded (cdr bkmk-assoc)))
                            (`lastModified (setq lastKModified (cdr bkmk-assoc)))
                            (_ (error "lbkmk-make-lbkmk-moz-place-list got alist with car of %s" (car bkmk-assoc)))
                            )) list-of-lists)))

(assert (listp lbkmk-json-object))
(assert (listp (cdr lbkmk-json-object)))
(assert (listp (cddr lbkmk-json-object)))
(lbkmk-output (type-of lbkmk-json-object) (cddr lbkmk-json-object))
;; (lbkmk-make-lbkmk-moz-place-list (aref lbkmk-json-object 1))
