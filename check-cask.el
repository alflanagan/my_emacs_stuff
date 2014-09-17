;;; check-cask -- compare loaded packages to contents of Cask file  -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(require 'package)
(require 'dash)
(require 's)

(setq check-cask-installed-package-names (mapcar 'car package-alist))
(setq check-cask-installed-count (length check-cask-installed-package-names))


(defun get-cask-package-names (file-name)
  "Read cask file FILE-NAME and extract the package names from depends functions, return as list."
  (with-temp-buffer (insert-file-contents file-name)
                    (mapcar (lambda (a-line) (s-replace "\")" "" (s-replace "(depends-on \"" "" a-line)))
                            (-filter (lambda (a-str) (string-prefix-p "(depends" a-str))
                                     (split-string (buffer-string) "[\n\r]+")))))

(setq check-cask-file-package-names (get-cask-package-names "Cask.full"))
(length check-cask-file-package-names)
(setq check-cask-file-package-names (delete-dups check-cask-file-package-names))
(length check-cask-file-package-names)

(mapcar (lambda (a-line) (s-replace "(depends-on \"" "" a-line))
        '("(depends-on \"fred\")" "(depends-on \"wilma\")"))

(provide 'check-cask)
;;; check-cask.el ends here

;; Local Variables:
;; coding: utf-8-unix
;; End:
