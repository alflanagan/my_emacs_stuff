;;; test-lib-bookmark -- unit tests for lib-bookmark module  -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;; Unit tests for functions in the namespace lbkmk-

;;; Code:
(require 'ert)
(require 'lib-bookmark)

;;; Helper functions
(defun output-buffer-has-text-p (expected-text)
  "Does the current output buffer contain the string in EXPECTED-TEXT?"
  (with-current-buffer (lbkmk-get-output-buffer)
    (equal (buffer-substring (point-min) (point-max)) expected-text)))

;;; **** lbkmk-convert-moz-time ****
(ert-deftest lbkmk-test-convert-moz-time ()
  "Tests functionality of lbkmk-convert-moz-time."
  (should (equal (lbkmk-convert-moz-time  100000000000001.0) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  100000000000001) '(1525 57600 0 998378)))
  (should (equal (lbkmk-convert-moz-time  0.0) '(0 0 0 0)))
  (should (equal (lbkmk-convert-moz-time nil) nil)))


;; (defun environment-has-key-p (key)
;;   "True if the process' initial environment has a value for key string KEY."
;;   (let ((sprefix (format "%s=" (upcase key))))
;;     (-any?
;;      '---truthy?
;;      (mapcar
;;       (lambda (x) (string-prefix-p sprefix x))
;;       initial-environment))))


(defun value-from-initial-environment (key)
  "Return the value assigned to KEY in the process initial environment, or nil."
  (cadr (assoc key  (mapcar (lambda (x) (split-string x "=")) initial-environment))))


;;; **** lbkmk-format-moz-time-iso-8601 ****
(defun lbkmk-test-format-moz-time-iso-8601-UTC ()
  "Test `lbkmk-format-moz-time-iso-8601' assuming server is set to UTC time."
  (should (equal
           (lbkmk-format-moz-time-iso-8601 0.0)
           "1970-01-01T00:00:00-0000"))
  (should (equal
           (lbkmk-format-moz-time-iso-8601 (* 1000000  1399864605.1600919))
           "2014-05-11T23:16:45-0400")))

(ert-deftest lbkmk-test-format-moz-time-iso-8601 ()
  "Unit tests for lbkmk-format-moz-time-iso-8601."
  ;; right thing to do: adjust expected result for local time zone
  ;; doing instead: special check for Travis ( = UTC)
  (if (not (value-from-initial-environment "TRAVIS"))
      (progn (should (equal
                      (lbkmk-format-moz-time-iso-8601 0.0)
                      "1969-12-31T19:00:00-0500"))
             (should (equal
                      (lbkmk-format-moz-time-iso-8601 (* 1000000  1399864605.1600919))
                      "2014-05-11T23:16:45-0400")))
    (lbkmk-test-format-moz-time-iso-8601-UTC))
  )

;;; **** lbkmk-fuzzy-float-str ****
(ert-deftest lbkmk-test-fuzzy-float-str ()
  "Unit tests for lbkmk-fuzzy-float-str."
  (should (equal (lbkmk-fuzzy-float-str 7.0) "7.0"))
  (should (equal (lbkmk-fuzzy-float-str  100000000000001.0) "1973-03-03T04:46:40-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147449999999999.0) "2038-01-18T12:53:19-0500"))
  (should (equal (lbkmk-fuzzy-float-str  2147450000000000.0) "2.14745e+15")))

;;; **** lbkmk-output-str ****
(ert-deftest lbkmk-test-output-str ()
  (lbkmk-clear-output-buffer)
  (should (output-buffer-has-text-p ""))
  (lbkmk-output-str "fred")
  (should (output-buffer-has-text-p "fred"))
  (lbkmk-output-str 7)
  (should (output-buffer-has-text-p "fred"))
  (lbkmk-clear-output-buffer))

;;; **** lbkmk-output-strs ****
(ert-deftest lbkmk-test-output-strs ()
  (lbkmk-clear-output-buffer)
  (should (output-buffer-has-text-p ""))
  (lbkmk-output-str "fred")
  (should (output-buffer-has-text-p "fred"))
  (lbkmk-clear-output-buffer)
  (lbkmk-output-strs "fred" "barney" "wilma")
  (should (output-buffer-has-text-p "fredbarneywilma"))
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


(defun lbkmk-make-json-from-str (sample-json)
  (with-temp-buffer
    (insert sample-json)
    (goto-char    (point-min))
    (json-read-object)))

(ert-deftest lbkmk-test-make-moz-place-from-json ()
    (let ((sample-json 
           (lbkmk-make-json-from-str "{
\"uri\": \"https://www.google.com/webfonts\",
\"type\": \"text/x-moz-place\",
\"dateAdded\": 1362163315000000,
\"parent\": 3881,
\"id\": 4190,
\"title\": \"Google Web Fonts\",
\"index\": 1
},")))
      (should (equal (lbkmk-make-moz-place-from-json sample-json)
                     (make-lbkmk-moz-place :uri "https://www.google.com/webfonts"
                                           :type  "text/x-moz-place"
                                           :lastModified  (lbkmk-convert-moz-time nil)
                                           :dateAdded (lbkmk-convert-moz-time 1362163315000000)
                                           :parent 3881
                                           :id 4190
                                           :title "Google Web Fonts"
                                           :index 1)))))

(ert-deftest  lbkmk-test-make-moz-root-from-json ()
  (let ((sample-json (lbkmk-make-json-from-str
                      "{\"children\": [],
  \"root\": \"placesRoot\",
  \"type\": \"text\\/x-moz-place-container\",
  \"lastModified\": 1359143022769056,
  \"dateAdded\": 1328812310196978,
  \"id\": 1,
  \"title\": \"\"
}"
                      )))
    (should (equal (lbkmk-make-moz-root-from-json sample-json)
                   (make-lbkmk-moz-root :id 1
                                        :dateAdded (lbkmk-convert-moz-time 1328812310196978)
                                        :lastModified (lbkmk-convert-moz-time 1359143022769056)
                                        :type "text/x-moz-place-container"
                                        :root "placesRoot"
                                        :children [])))))
(progn (ert t) nil)

(provide 'test-lib-bookmark)
;;; test-lib-bookmark.el ends here
