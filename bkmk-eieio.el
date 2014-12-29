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


;;; Code:
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


;;; Class bkmk-bookmark -- represent an x-moz-place

;; "uri": "http:\/\/retailmerchants.com\/member_search\/admin\/default.aspx",
;; "type": "text\/x-moz-place",
;; "lastModified": 1364579681000000,
;; "dateAdded": 1364579668000000,
;; "parent": 3864,
;; "id": 4173,
;; "title": "retailmerchants admin",
;; "index": 1
(defclass bkmk-bookmark ()
  ((id :initarg :id
       :initform nil
       :type list
       :documentation
       "")
   (uri :initarg :uri
        :initform nil
        :type list
        :documentation
        "The target of this bookmark, a standard URL.")
   (lastModified :initarg :lastModified
                 :initform nil
                 :type list
                 :documentation
                 "")
   (dateAdded :initarg :dateAdded
              :initform nil
              :type list
              :documentation
              "")
   (parent :initarg :parent
           :initform nil
           :type list
           :documentation
           "")
   (title :initarg :title
          :initform nil
          :type list
          :documentation
          "Short description of place, shows in menu.")
   (index :initarg "0"
          :initform nil
          :type 
          :documentation
          "Orders places within a container.")
   )
  "Class bkmk-bookmark ")
