;;; lib-bookmark.el --- Browser bookmark file browsing & conversion -*- lexical-binding: t -*-
;; namespace prefix for this file: lbkmk-

;; Copyright (C) 2014 A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan
;; Version: 0.1
;; Keywords: web

;; This file is NOT part of GNU Emacs.

;; lib-bookmark.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; lib-bookmark.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lib-bookmark.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These functions are geared to reading a list of web sit bookmarks
;; from a JSON file produced using Firefox's bookmarks export feature.

;;; History:


;;; Code:
(require 'json)
(eval-when-compile (require 'time-date))
(require 'cl-lib) ;;OK, maybe CL isn't all bad

(defconst lbkmk-test-bookmarks-file "bookmarks-2014-04-25.json"
  "A Firefox JSON export file for testing purposes.")


(defvar lbkmk-json-object
  (json-read-file lbkmk-test-bookmarks-file)
  "JSON object from Firefox export file.")

(defconst lbkmk-test-chrome-bkmk-file "gchrome_bookmarks_9_17_14.html"
  "A Google Chrome export to Netscape HTML format.")

(defvar lbkmk-test-chrome-object
  (with-temp-buffer
    (insert-file-contents lbkmk-test-chrome-bkmk-file )
    (libxml-parse-html-region (point-min) (point-max)))
  "Parse tree from test file in `lbkmk-test-chrome-bkmk-file'.")

(defun process-kids (parse-node)
  "Look at child nodes of PARSE-NODE and return bookmark/folder structure."
  (mapcar (lambda (a-node)
            (cl-case (car a-node)
              ('(nil "head") nil)
              (t (car a-node) )))
          (cdr parse-node)))

(defun process-chrome-parse-tree (parse-tree)
   "Read parse tree PARSE-TREE and extract bookmark structure."
   (let ((html-tree (car (cdr (cdr (cdr parse-tree))))))
     (cl-assert (string-equal (car html-tree) "html"))
     (cdr (cdr (cdr html-tree)))
     ;;(mapcar (lambda (a) a) (cdr (cdr (cdr html-tree))))
     ;;(mapcar 'process-kids html-tree)
     ))

;;(length (nth 3 (nth 3 lbkmk-test-chrome-object)))
;;(nth 3 (cdr (car (process-chrome-parse-tree lbkmk-test-chrome-object))))
 ;; (prin1 lbkmk-json-object (get-buffer-create "*parsed-json*"))

(defun lbkmk-get-output-buffer ()
  "Return the bookmark output buffer, if it exists."
  (get-buffer "*web_bookmarks*"))


(defun lbkmk-get-create-output-buffer ()
  "Return the bookmark output buffer, creating it if does not exist."
  (get-buffer-create "*web_bookmarks*"))


(defun lbkmk-clear-output-buffer () 
  "Remove all previous output, if any."
  (with-current-buffer (lbkmk-get-create-output-buffer)
    (delete-region (point-min) (point-max))))


(lbkmk-clear-output-buffer)


(defun lbkmk-convert-moz-time (time-value)
  "Convert mozilla time TIME-VALUE to standard Emacs (HIGH LOW USEC PSEC)."
  (if time-value
      (seconds-to-time (/ time-value 1000000.0))
    nil))


(defun lbkmk-format-moz-time-iso-8601 (time-value)
  "Format TIME-VALUE from mozilla JSON to ISO 8601 standard format, return as string."
  (if time-value
      (format-time-string "%FT%T%z"  (lbkmk-convert-moz-time time-value))
    ""))


(defun lbkmk-fuzzy-float-str (any-float)
  "Format float ANY-FLOAT as a date string if it would result in a reasonable value, as float otherwise."
  ;;assume floating-point value is date if would convert to
  ;;later than 3/3/1973
  (if (and (> any-float  1.0e14)
           (< any-float 2.14745e15))
      (lbkmk-format-moz-time-iso-8601 any-float)
    (number-to-string any-float)))


(defun lbkmk-output-str (a-string)
  "Write A-STRING to the output buffer."
  (with-current-buffer (lbkmk-get-create-output-buffer)
    (goto-char (point-max))
    (insert a-string)))

(defun lbkmk-output-strs (&rest output-strings)
  "Write list of strings OUTPUT-STRINGS to the output buffer."
  (with-current-buffer  (lbkmk-get-create-output-buffer)
    (goto-char (point-max))
    (mapc 'insert output-strings)))

(defun lbkmk-output (&rest output-values)
  "Output OUTPUT-VALUES as text by writing to my custom buffer."
  (apply 'lbkmk-output-strs
         (cl-remove-if 'null
                       (mapcar (lambda (x) (format "%s" x)) output-values))))

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


(defun lbkmk-make-lbkmk-moz-place-list (list-of-lists)
  "Build and return a lbkmk-moz-place object from LIST-OF-LISTS."
  (let (url lastModified dateAdded parent id title index)
    (mapc (lambda (bkmk-assoc) (pcase (car bkmk-assoc)
                            (`title (setq title (cdr bkmk-assoc)))
                            (`uri (setq url (cdr bkmk-assoc)))
                            (`type (if (not (equal (cdr bkmk-assoc) "text/x-moz-place"))
                                       (error "Function lbkmk-make-lbkmk-moz-place-list got record of type %s" (cdr bkmk-assoc))))
                            (`parent (setq parent (cdr bkmk-assoc)))
                            (`id (setq id (cdr bkmk-assoc)))
                            (`dateAdded (setq dateAdded (cdr bkmk-assoc)))
                            (`lastModified (setq lastModified (cdr bkmk-assoc)))
                            (`_ (error "Function lbkmk-make-lbkmk-moz-place-list got alist with car of %s" (car bkmk-assoc)))
                            )) list-of-lists)))


(cl-assert (listp lbkmk-json-object))
(cl-assert (listp (cdr lbkmk-json-object)))
(cl-assert (listp (cddr lbkmk-json-object)))
(lbkmk-output (type-of lbkmk-json-object) (cddr lbkmk-json-object))
;; (lbkmk-make-lbkmk-moz-place-list (aref lbkmk-json-object 1))

;;JSON:
;; {
;; "uri": "https:\/\/www.google.com\/webfonts",
;; "type": "text\/x-moz-place",
;; "dateAdded": 1362163315000000,
;; "parent": 3881,
;; "id": 4190,
;; "title": "Google Web Fonts",
;; "index": 1
;; },

;;parsed:
;; ((index . 1)
;;  (title . "Google Web Fonts")
;;  (id . 4190)
;;  (parent . 3881)
;;  (dateAdded . 1.362163315e+15)
;;  (type . "text/x-moz-place")
;;  (uri . "https://www.google.com/webfonts"))

(defun lbkmk-make-moz-place-from-json (json-node)
  "Convert a parsed bookmark in JSON-NODE to lbkmk-moz-place structure."
  (make-lbkmk-moz-place :uri  (assoc-default 'uri json-node)
                        :type  (assoc-default 'type json-node)
                        :lastModified  (lbkmk-convert-moz-time (assoc-default  'lastModified json-node))
                        :dateAdded (lbkmk-convert-moz-time (assoc-default 'dateAdded json-node))
                        :parent (assoc-default 'parent json-node)
                        :id (assoc-default 'id json-node)
                        :title (assoc-default 'title json-node)
                        :index (assoc-default 'index json-node)))

;; ((title . "")
;;  (id . 1)
;;  (dateAdded . 1328812310196978)
;;  (lastModified . 1359143022769056)
;;  (type . "text/x-moz-place-container")
;;  (root . "placesRoot")
;;  (children .
(cl-defstruct lbkmk-moz-root id dateAdded lastModified type root children)

(defun lbkmk-make-moz-root-children-from-json (json-node)
  "Currently a place-holder; process children of JSON-NODE, a moz-root node instance."
  json-node)

(defun lbkmk-make-moz-root-from-json (json-node)
  "Given a parsed bookmark file in JSON-NODE, create the bookmark tree from root."
  (make-lbkmk-moz-root :id (assoc-default 'id json-node)
                       :type  (assoc-default 'type json-node)
                       :lastModified  (lbkmk-convert-moz-time (assoc-default  'lastModified json-node))
                       :dateAdded (lbkmk-convert-moz-time (assoc-default 'dateAdded json-node))
                       :root (assoc-default 'root json-node)
                       :children (lbkmk-make-moz-root-children-from-json (assoc-default 'children json-node))))

;; (with-current-buffer (get-buffer-create "*parsed-json*")
;;   (insert (pp-to-string lbkmk-json-object)))
;;; # Goals #
;;; Parse json structure
;;; create buffer showing bookmarks
;;; save bookmarks in multiple formats

(provide 'lib-bookmark)
;;; lib-bookmark.el ends here
