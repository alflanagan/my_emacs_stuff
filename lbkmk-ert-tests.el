;; -*- coding: utf-8; lexical-binding: t -*-
;;; Unit tests for functions in the namespace lbkmk-

(require 'ert)

(load-file (locate-file "lib-bookmark.el" load-path))

;;; Helper functions
(defun output-buffer-has-text (expected-text)
  (with-current-buffer (lbkmk-get-output-buffer)
    (equal (buffer-substring (point-min) (point-max)) expected-text)))

;;; **** lbkmk-convert-moz-time ****
(ert-deftest lbkmk-test-convert-moz-time ()
  "Tests functionality of lbkmk-convert-moz-time."
  (should (equal (lbkmk-convert-moz-time  100000000000001.0) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  100000000000001) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  0.0) '(0 0 0 0)))
  (should (equal (lbkmk-convert-moz-time nil) nil)))


;;; **** lbkmk-format-moz-time-iso-8601 ****
(ert-deftest lbkmk-test-format-moz-time-iso-8601 ()
  "Unit tests for lbkmk-format-moz-time-iso-8601."
  (should (equal
           (lbkmk-format-moz-time-iso-8601 0.0)
           "1969-12-31T19:00:00-0500"))
  (should (equal
           (lbkmk-format-moz-time-iso-8601 (* 1000000  1399864605.1600919))
           "2014-05-11T23:16:45-0400")))

;;; **** lbkmk-fuzzy-float-str ****
(ert-deftest lbkmk-test-fuzzy-float-str ()
  "Unit tests for lbkmk-fuzzy-float-str."
  (should (equal (lbkmk-fuzzy-float-str 7.0) "7.0"))
  (should (equal (lbkmk-fuzzy-float-str  100000000000001.0) "1973-03-03T04:46:40-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147449999999999.0) "2038-01-18T12:53:19-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147450000000000.0) "2.14745e+15")))

;;; **** lbkmk-make-str ****
(ert-deftest lbkmk-test-make-str ()
  (should (equal (lbkmk-make-str "title") "title"))
  (should (equal (lbkmk-make-str 7) "7"))
  (should (equal (lbkmk-make-str 7.0) "7.0"))
  (should (equal (lbkmk-make-str 1137450000000000.0) "2006-01-16T17:20:00-0500"))
  (should (equal (lbkmk-make-str '(1 2)) "unknown type")))

;;; **** lbkmk-output-str ****
(ert-deftest lbkmk-test-output-str ()
  (lbkmk-clear-output-buffer)
  (should (output-buffer-has-text ""))
  (lbkmk-output-str "fred")
  (should (output-buffer-has-text "fred"))
  (lbkmk-output-str 7)
  (should (output-buffer-has-text "fred"))
  (lbkmk-clear-output-buffer))

;;; **** lbkmk-output-strs ****
(ert-deftest lbkmk-test-output-strs ()
  (lbkmk-clear-output-buffer)
  (should (output-buffer-has-text ""))
  (lbkmk-output-str "fred")
  (should (output-buffer-has-text "fred"))
  (lbkmk-clear-output-buffer)
  (lbkmk-output-strs "fred" "barney" "wilma")
  (should (output-buffer-has-text "fredbarneywilma"))
  (lbkmk-clear-output-buffer))

;;; **** lbkmk-output ****

;;; **** Structure lbkmk-moz-place ****
;; make-lbkmk-moz-place
(ert-deftest lbkmk-test-make-moz-place ()
  (should (equal  (make-lbkmk-moz-place) [cl-struct-lbkmk-moz-place nil nil nil nil nil nil nil nil]))
  (should (equal (make-lbkmk-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
                                       :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
                                       :index 9)
                 [cl-struct-lbkmk-moz-place "http://www.example.com" "text" 1340392082000000 1340391622000000 3860
                                            4153 "NINA - Devbox" 9]))
  )

;; copy-lbkmk-moz-place
(ert-deftest lbkmk-test-copy-moz-place ()
  (let ((lbkmk-test-bookmark
         (make-lbkmk-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
                               :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
                               :index 9))
        lbkmk-test-bookmark2)
    (setq lbkmk-test-bookmark2 (copy-lbkmk-moz-place lbkmk-test-bookmark))
    (should (lbkmk-moz-place-p lbkmk-test-bookmark2))
    (should (equal lbkmk-test-bookmark lbkmk-test-bookmark2))
    (should (not (eq lbkmk-test-bookmark lbkmk-test-bookmark2)))
    ))

;; lbkmk-moz-place-fieldname
(ert-deftest lbkmk-test-fields-moz-place ()
  (let ((lbkmk-test-bookmark
         (make-lbkmk-moz-place :uri "http://www.example.com" :type "text" :lastModified 1340392082000000
                               :dateAdded 1340391622000000 :parent 3860 :id 4153 :title "NINA - Devbox"
                               :index 9)))
    (should (equal (lbkmk-moz-place-title lbkmk-test-bookmark) "NINA - Devbox"))
    (should (equal (lbkmk-moz-place-type lbkmk-test-bookmark) "text"))))


(progn (ert t) nil)
