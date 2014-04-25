;;; init.el -- Non-site-specific initialization not controlled by customize.
;; Copyright © 2014, A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Maintainer: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 2014
;; Version: 0.02

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;;; Code:

;;(filesets-init)

(defun add-hooks-for-packages ()
  "Set up hooks which depend on packages that may not be synched on startup"
  (add-hook 'sh-mode-hook '(turn-off-auto-fill))
  ;;because I find it annoying for shell scripts
  
  ;; (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

  (add-hook 'python-mode-hook 'flycheck-mode)
  ;; (add-hook 'python-mode-hook 'auto-complete-mode)
)

(add-hook 'after-init-hook 'add-hooks-for-packages)

;;We need do-sync-packages to run *before* add-hooks-for-packages
;;so we have to add it to hook *after*
(when (boundp 'emacs-sync-directory)
 (defun do-sync-packages() (load-file (concat emacs-sync-directory "sync_packages.el")))
 (add-hook 'after-init-hook 'do-sync-packages)
)

;(eval-after-load "dash" 'dash-enable-font-lock)


;;; init.el ends here
