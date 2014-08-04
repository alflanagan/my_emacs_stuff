;;; .emacs -- Master customization file. -*- lexical-binding: t -*-

;;; Commentary:
;; Note that most customization is located in init.el, which is
;; source-controlled (git) and suitable for all my emacs installs

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2)))))

(put 'scroll-left 'disabled nil)

(if (file-exists-p "~/.emacs-site.el")
    (load-file "~/.emacs-site.el")
  (message "[init] no local .emacs-site.el file found."))

(if (boundp 'emacs-sync-directory)
    (add-to-list 'load-path emacs-sync-directory)
  (message "[init] emacs-sync-directory not set, not added to load-path."))

(let ((init-file (locate-file "init.el" load-path)))
  (if init-file
      (load-file init-file)
    (message "[init] skipped loading init.el, file not found.")))

(provide '.emacs)

;;; .emacs ends here
