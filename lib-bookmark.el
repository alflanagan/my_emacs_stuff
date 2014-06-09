;; -*- coding: utf-8; lexical-binding: t -*-
;;; lib-bookmark.el --- Browser bookmark file browsing & conversion
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

(eval-when-compile (require 'json))
(eval-when-compile (require 'time-date))
(eval-when-compile (require 'cl-lib)) ;;OK, maybe CL isn't all bad

(defconst lbkmk-test-bookmarks-file "bookmarks-2014-04-25.json"
  "A Firefox JSON export file for testing purposes")

(defvar lbkmk-json-object
  (json-read-file lbkmk-test-bookmarks-file)
  "JSON object from Firefox export file.")

;; (prin1 lbkmk-json-object (get-buffer-create "*parsed-json*"))

(defun lbkmk-get-output-buffer ()
  "Returns the bookmark output buffer, if it exists."
  (get-buffer "*web_bookmarks*"))

(defun lbkmk-get-create-output-buffer ()
  "Returns the bookmark output buffer, creating it if does not exist."
  (get-buffer-create "*web_bookmarks*"))

(defun lbkmk-clear-output-buffer () 
  "Remove all previous output, if any"
  (with-current-buffer (lbkmk-get-create-output-buffer)
    (delete-region (point-min) (point-max))))

(lbkmk-clear-output-buffer)

(defun lbkmk-convert-moz-time (time-value)
  "Convert mozilla time value to standard emacs (HIGH LOW USEC PSEC)"
  (if time-value
      (seconds-to-time (/ time-value 1000000.0))
    nil))

(defun lbkmk-format-moz-time-iso-8601 (time-value)
  "Format time-value from mozilla JSON to ISO 8601 standard format, return as string"
  (format-time-string "%FT%T%z"  (lbkmk-convert-moz-time time-value)))

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
         "unknown type")))

(defun lbkmk-output-str (a-string)
  "Writes a single string or character to the output buffer."
  (let ((outputbuffer (lbkmk-get-create-output-buffer)))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (insert a-string))))

(defun lbkmk-output-strs (&rest output-strings)
  "Write a list of strings to the output buffer *experiments*"
  (let ((outputbuffer (lbkmk-get-create-output-buffer)))
    (with-current-buffer outputbuffer
      (goto-char (point-max))
      (mapc 'insert output-strings))))

(defun lbkmk-output
    (&rest output-values)
  "Output text  by writing to my custom buffer"
  (let
      ((output-strings
        (mapcar 'lbkmk-make-str output-values)))
    (setq output-strings
          (cl-remove-if 'null output-strings))
    (apply 'lbkmk-output-strs output-strings)))

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
  (let (url lastModified dateAdded parent id title index)
    (mapc (lambda (bkmk-assoc) (pcase (car bkmk-assoc)
                            (`title (setq title (cdr bkmk-assoc)))
                            (`uri (setq url (cdr bkmk-assoc)))
                            (`type (if (not (equal (cdr bkmk-assoc) "text/x-moz-place"))
                                       (error "lbkmk-make-lbkmk-moz-place-list got record of type %s" (cdr bkmk-assoc))))
                            (`parent (setq parent (cdr bkmk-assoc)))
                            (`id (setq id (cdr bkmk-assoc)))
                            (`dateAdded (setq dateAdded (cdr bkmk-assoc)))
                            (`lastModified (setq lastModified (cdr bkmk-assoc)))
                            (`_ (error "lbkmk-make-lbkmk-moz-place-list got alist with car of %s" (car bkmk-assoc)))
                            )) list-of-lists)))

(assert (listp lbkmk-json-object))
(assert (listp (cdr lbkmk-json-object)))
(assert (listp (cddr lbkmk-json-object)))
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
  "Given a parsed JSON representation of a web bookmark, returns a lbkmk-moz-place structure."
  (make-lbkmk-moz-place :uri  (assoc-default 'uri json-node)
                        :type  (assoc-default 'type json-node)
                        :lastModified  (lbkmk-convert-moz-time (assoc-default  'lastModified json-node))
                        :dateAdded (lbkmk-convert-moz-time (assoc-default 'dateAdded json-node))
                        :parent (assoc-default 'parent json-node)
                        :id (assoc-default 'id json-node)
                        :title (assoc-default 'title json-node)
                        :index (assoc-default 'index json-node)))

(defun lbkmk-get-sample-json-parse ()
  (with-temp-buffer
    (insert "{
\"uri\": \"https://www.google.com/webfonts\",
\"type\": \"text/x-moz-place\",
\"dateAdded\": 1362163315000000,
\"parent\": 3881,
\"id\": 4190,
\"title\": \"Google Web Fonts\",
\"index\": 1
},")
    (goto-char    (point-min))
    (json-read-object)))

(lbkmk-make-moz-place-from-json (lbkmk-get-sample-json-parse))

;; ((title . "")
;;  (id . 1)
;;  (dateAdded . 1328812310196978)
;;  (lastModified . 1359143022769056)
;;  (type . "text/x-moz-place-container")
;;  (root . "placesRoot")
;;  (children .
(cl-defstruct lbkmk-moz-root id dateAdded lastModified type root children)

(defun lbkmk-make-moz-root-children-from-json (json-node)
  "Currently a place-holder; process children of a moz-root node."
  json-node)

(defun lbkmk-make-moz-root-from-json (json-node)
  "Given a parsed bookmark file, create the bookmark tree from root."
  (make-lbkmk-moz-root :id (assoc-default 'id json-node)
                       :type  (assoc-default 'type json-node)
                       :lastModified  (assoc-default  'lastModified json-node)
                       :dateAdded (assoc-default 'dateAdded json-node)
                       :root (assoc-default 'root json-node)
                       :children (lbmk-make-moz-root-children-from-json (assoc-default 'children json-node))))

;; (with-current-buffer (get-buffer-create "*parsed-json*")
;;   (insert (pp-to-string lbkmk-json-object)))
;;; # Goals #
;;; Parse json structure
;;; create buffer showing bookmarks
;;; save bookmarks in multiple formats
