;; test-file-mode-exp -- tests for file-mode-exp.el -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:


;;; Code:

(require 'ert)
(require 'file-mode-exp)

(ert-deftest test-add-list-entry ()
  (should (equal (assoc "fred" interpreter-mode-alist) nil))
  (file-mode-exp-set-interpreter-mode "fred" 'emacs-lisp-mode)
  (should (equal (assoc "fred" interpreter-mode-alist) '("fred" . emacs-lisp-mode)))
  (setq interpreter-mode-alist
        (delq (assoc "fred" interpreter-mode-alist) interpreter-mode-alist)))

(ert-deftest test-change-list-entry ()
  (should (equal (assoc "pike" interpreter-mode-alist) '("pike" . pike-mode)))
  (file-mode-exp-set-interpreter-mode "pike" 'python-mode)
  (should (equal (assoc "pike" interpreter-mode-alist) '("pike" . python-mode)))
  (file-mode-exp-set-interpreter-mode "pike" 'pike-mode)
  (should (equal (assoc "pike" interpreter-mode-alist) '("pike" . pike-mode))))

(provide 'test-file-mode-exp)
;;; test-file-mode-exp.el ends here
