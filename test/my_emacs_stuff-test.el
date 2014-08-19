;;; my_emacs_stuff-test -- master test file for ert-runner -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(add-to-list 'load-path (expand-file-name "."))
(add-to-list 'load-path (expand-file-name "test"))

;; debug: print everything in load-path not under directories named
;; '.cask' or 'opt'
;; (mapc (lambda (x) (when  (not (or (string-match ".*\\.cask.*" x) (string-match ".*/opt/.*" x))) (message x)))
;;       load-path)

(require 'test-secret-data)
(require 'test-eieio)
(require 'test-lib-bookmark)
(require 'test-file-mode-exp)
(ert t)

(provide 'my_emacs_stuff-test)
;;; my_emacs_stuff-test.el ends here
