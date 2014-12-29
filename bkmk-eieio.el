;;; bkmk-eieio.el --- Interpret/manipulate browser bookmarks  -*- lexical-binding: t; coding: utf-8; -*-
;;
;; Copyright (C) 2014 A. Lloyd Flanagan
;;
;; Author: A. Lloyd Flanagan <lloyd@lloyd-ThinkPad-R400>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


;;; Commentary:
;;
;; A bookmark file has (at least) 5 types of records, differentiated
;; by the "type" field. The known types are:

;; text\/x-moz-place -- a stored URL, with some basic info about the
;; destination.

;; text\/x-moz-place-container -- groups records, may be nested. has a
;; property "children" which is an array of child records.

;; text\/x-moz-place-separator -- a visual separator between bookmarks
;; inside an x-moz-place-container

;; 1 & 3 -- these are records defining RSS feeds, haven't figured it
;; all out just yet. found in "annos" fields -- short for
;; "annotations"??


;;; Code:
;;

(require 'eieio)



(defun bkmk-convert-moz-time (time-value)
  "Convert mozilla time TIME-VALUE to standard Emacs (HIGH LOW USEC PSEC)."
  (if time-value
      (seconds-to-time (/ time-value 1000000.0))
    nil))

;;; Class bkmk-container -- represent an x-moz-place-container
;; "type": "text\/x-moz-place-container",
;; "parent": 2,
;; "id": 3676,
;; "title": "Bookmarks Toolbar",
;; "index": 3

(defclass bkmk-container ()
  ((id :initarg :id
       :documentation
       "")
   (parent :initarg :parent
           :documentation
           "")
   (index :initarg :index
          :initform 0
          :type integer
          :documentation
          "")
   (title :initarg :title
          :initform "" ;; not sure empty title is legal
          :type string
          :documentation
          "")
   (children :initarg :children
             :type list
             :documentation
             "")
   )
  "A container for bookmarks.")

;;; Class bkmk-place -- represent an x-moz-place

;; "uri": "http:\/\/retailmerchants.com\/member_search\/admin\/default.aspx",
;; "type": "text\/x-moz-place",
;; "lastModified": 1364579681000000,
;; "dateAdded": 1364579668000000,
;; "parent": 3864,
;; "id": 4173,
;; "title": "retailmerchants admin",
;; "index": 1
(defclass bkmk-place ()
  ((id :initarg :id
       :type integer
       :documentation
       "A unique identifier for this `bkmk-place' in a `bkmk-container' heirarchy.")
   (uri :initarg :uri
        :type string
        :documentation
        "The target of this bookmark, a standard URL.")
   (last-modified :initarg :last-modified
                  ;; TODO -- write validator for date values
                  :type list
                  :initform (0 0 0 0)
                  :documentation
                  "The date/time at which the bookmark was last modified, in standard Emacs lisp format (see `current-time').")
   (date-added :initarg :date-added
               :initform (0 0 0 0)
               :type list
               :documentation
               "The date/time at which the bookmark was created, in standard Emacs lisp format (see `current-time').")
   (parent :initarg :parent
           :type bkmk-container
           :initform nil
           :documentation
           "The parent record for this bookmark.")
   (title :initarg :title
          :initform ""
          :type string
          :documentation
          "Short description of place, shows in menu.")
   (index :initarg :index
          :initform 0
          :type integer
          :documentation
          "Orders places within a container.")
   )
  "Class bkmk-place")

(defmethod bkmk-format-date-added ((a-bkmk bkmk-place) format-string)
  "Retrun string with date-added field of A-BKMK formatted according to FORMAT-STRING. See `format-time-string' for format specifications."
  (if (slot-value a-bkmk :date-added)
      (format-time-string format-string (slot-value a-bkmk :date-added))
    ""))

(defclass bkmk-separator ()
  ((parent :initarg :parent
           :type integer
           :documentation
           "")
   (index initarg :index
          :type integer
          :documentation
          ""))
  "A useful placeholder for a menu separator.")

(provide 'bkmk-eieio)
;;; bkmk-eieio.el ends here
