;; -*- lexical-binding: t -*-

;;; for now, just a set of experimental stuff
(defun parse-bookmark-file (file-name) nil)
(require 'json)
(require 'time-date)

(setq test-bookmarks-file "bookmarks-2014-04-25.json")

(setq json-object (json-read-file test-bookmarks-file))

(defun handle-bookmark (bookmark)
  ;;bookmark is a dotted pair
  (if (not (atom (cdr bookmark))) (error "Argument must be dotted pair, got %s" (type-of bookmark)))
  (pcase (car bookmark)
    (`title (prin1 (concat "Title is " (cdr bookmark))))
    (`id (prin1 (concat "ID is " (number-to-string (cdr bookmark)))))
    (`dateAdded (mapc 'princ (cons (quote "Date added is ") (list (cdr bookmark)))))))
  
(mapcar 'handle-bookmark json-object)

;;(mapcar 'handle-bookmark '((fred . wilma) (title . "gone with the")))



