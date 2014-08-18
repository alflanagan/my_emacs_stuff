;; test-secret-data -- Unit tests for module secret-data
;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'secret-data)


(defun test-secret-data-fixture (a-function &rest args)
  "Call A-FUNCTION with arguments ARGS and the name of a temporary file containing test data."
  ;; TODO: this is severely over-complicated -- simplify
  (let ((temp-file-name  (make-temp-file "secret-data-test"))
        (return-value nil))
    ;;(message "TEMP-FILE-NAME is '%s'" temp-file-name)
    (unwind-protect
        (progn (with-temp-file temp-file-name
                 (insert "value1=somerandomstring\npinboard-user-name=a.lloyd.flanagan\nsome-password=aA@#%&*$@#$@#$\\_KRr\nthis-is-a-test=has=equal=signs\ntest-whitespace=this is spaced\tand\ttabbed"))
               ;;(message "created %s." temp-file-name)
               ;;(mapc (lambda (arg) (message "ARG is %s" arg)) args)
               (setq return-value (apply a-function (append args (list temp-file-name))))
               (delete-file temp-file-name)
               return-value)
    )))

(ert-deftest test-secret-data-key-retrieval ()
    "Tests functionality of secret-data-get-key."
  
    (should (equal (test-secret-data-fixture 'secret-data-get-key "value1") "somerandomstring"))
    (should (equal (test-secret-data-fixture 'secret-data-get-key "pinboard-user-name") "a.lloyd.flanagan"))
    (should (equal (test-secret-data-fixture 'secret-data-get-key "some-password") "aA@#%&*$@#$@#$\\_KRr")))

(ert-deftest test-secret-embedded-equal-signs ()
  (should (equal (test-secret-data-fixture 'secret-data-get-key "this-is-a-test") "has=equal=signs")))

(ert-deftest test-secret-embedded-whitespace ()
  (should (equal (test-secret-data-fixture 'secret-data-get-key "test-whitespace") "this is spaced\tand\ttabbed")))

(ert-deftest test-secret-data-split-on-equal-if-key ()
  (should (equal (split-on-equal-if-key "") nil))
  (should (equal (split-on-equal-if-key "=fred") nil))
  (should (equal (split-on-equal-if-key "fred=") '("fred" "")))
  (should (equal (split-on-equal-if-key "fred=wilma") '("fred" "wilma"))))

(provide 'test-secret-data)
;;; test-secret-data.el ends here
