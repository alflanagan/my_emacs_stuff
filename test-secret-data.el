;; test-secret-data -- Unit tests for module secret-data
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'secret-data)

(let ((temp-file-name  (make-temp-file "secret-data-test"))) temp-file-name)
(defun test-secret-data-fixture (a-function &rest args)
  "Call A-FUNCTION with arguments ARGS and the name of a temporary file containing test data."
  (let ((temp-file-name  (make-temp-file "secret-data-test")))
    (unwind-protect
        (progn (with-temp-file temp-file-name
                 (insert "value1=somerandomstring\npinboard-user-name=a.lloyd.flanagan\nsome-password=aA@#%&*$@#$@#$\\_KRr\nthis-is-a-test=has=equal=signs\ntest-whitespace=this is spaced\tand\ttabbed"))
               (message "created %s." temp-file-name)
               (apply a-function (append args '(temp-file-name))))
      (delete-file temp-file-name))))

(test-secret-data-fixture 'split-string "=")
(ert-deftest test-secret-data-key-retrieval
  "Tests functionality of secret-data-get-key."
  (should (equal (secret-data-get-key "value1") "fred"))
  (should (equal (lbkmk-convert-moz-time  100000000000001) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  0.0) '(0 0 0 0)))
  (should (equal (lbkmk-convert-moz-time nil) nil)))

(secret-data-get-key "paradox-github-token")
(secret-data-get-key "pinboard-password")
(secret-data-get-key "this-is-a-test")
(secret-data-get-key "test-whitespace")

(provide 'test-secret-data)
;;; test-secret-data.el ends here
