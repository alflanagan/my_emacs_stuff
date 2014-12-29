;;; ert-bkmk-eieio.el --- ERT tests for bkmk-eieio objects
;;
;; Copyright (C) 2014 Adrian Lloyd Flanagan
;;
;; Author: Adrian Lloyd Flanagan <aflanagan@mgvwdm003.wme.owhc.net>
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

(require 'ert)
(require 'bkmk-eieio "/home/aflanagan/Devel/my_emacs_stuff/bkmk-eieio.el")


(ert-deftest test-bkmk-place-constructor ()
  "Test construction of a bkmk-place object."
  (let (( test1 (bkmk-place "test1" :id 1 :parent 0 :uri "http:\/\/updates.enginehosting.com\/"
                            :date-added (bkmk-convert-moz-time 1352820536000000)
                            :title "Enginehosting.com Status")))
    (should (= (slot-value test1 :id) 1))
    (should (equal (slot-value test1 :uri) "http:\/\/updates.enginehosting.com\/"))
    (should (= (oref test1 parent) 0))
    (should (equal (oref test1 date-added) '(20642 26424 0 0)))
    (should (equal (bkmk-format-date-added test1 "%m%d%Y%H%M%S") "11132012102856"))
    ))

(ert-deftest test-bkmk-place-format-date-added ()
  "Test the `bkmk-format-date-added' method for `bkmk-place' objects."
  )

