;; .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(autopair-autowrap t)
 '(autopair-global-mode t)
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" "11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default)))
 '(delete-by-moving-to-trash t)
 '(ecb-major-modes-show-or-hide (quote ((python))))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#383838")
 '(global-auto-complete-mode t)
 '(global-ede-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(isearchb-idle-timeout nil)
 '(isearchb-show-completions nil)
 '(iswitchb-mode t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(python-check-command "pylint")
 '(python-skeleton-autoinsert t)
 '(python-use-skeletons t)
 '(semantic-mode t)
 '(speedbar-track-mouse-flag t)
 '(tab-always-indent nil)
 '(track-eol t)
 '(trash-directory "~/.trash")
 '(undo-limit 100000)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs-site.el")
(if (boundp 'dropbox-location) 
    (load-file (concat dropbox-location "/emacs/init.el"))
        (load-file "~/Dropbox/emacs/init.el"))
