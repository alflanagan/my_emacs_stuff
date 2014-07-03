;;; .emacs -- Master customization file.

;;; Commentary:

;; TODO: customizations that should be synchronized across emacs
;; installs should be done outside customize; customize works best for
;; local settings only.

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
 )
(put 'scroll-left 'disabled nil)

;; must be after custom-set-variables as melpa archive is required
(add-to-list 'load-path "~/Devel/emacs/")
(load-file (locate-file "init.el" load-path))

(provide '.emacs)

;;; .emacs ends here
