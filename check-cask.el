;;; check-cask -- compare loaded packages to contents of Cask file  -*- lexical-binding: t; -*-
;;; specified

;;; Commentary:


;;; Code:

(require 'package)
(require 'dash)

(setq check-cask-installed-package-names (mapcar 'car package-alist))

(defun get-cask-package-names (file-name)
  "Read cask file FILE-NAME and extract the package names from depends functions, return as list."
  (with-temp-buffer (insert-file-contents file-name)
                    (-filter (lambda (a-str) (string-prefix-p "(depends" a-str))
                             (split-string (buffer-string) "[\n\r]+"))
                    ))

(get-cask-package-names "Cask.full")

(provide 'check-cask)
;;; check-cask.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; End:
