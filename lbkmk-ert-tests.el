;; -*- lexical-binding: t -*-
;;; Unit tests for functions in the namespace lbkmk-

(load-file "experiments.el")

(ert-deftest lbkmk-test-convert-moz-time ()
  "Tests functionality of lbkmk-convert-moz-time."
  (should (equal (lbkmk-convert-moz-time  100000000000001.0) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  100000000000001) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  0.0) '(0 0 0 0))))

(ert-deftest lbkmk-test-format-moz-time-iso-8601 ()
  (should (equal
           (lbkmk-format-moz-time-iso-8601 0.0)
           "1969-12-31T19:00:00-0500"))
  (should (equal
           (lbkmk-format-moz-time-iso-8601 (* 1000000  1399864605.1600919))
           "2014-05-11T23:16:45-0400")))

(ert-deftest lbkmk-test-fuzzy-float-str ()
  (should (equal (lbkmk-fuzzy-float-str 7.0) "7.0"))
  (should (equal (lbkmk-fuzzy-float-str  100000000000001.0) "1973-03-03T04:46:40-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147449999999999.0) "2038-01-18T12:53:19-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147450000000000.0) "2.14745e+15")))
