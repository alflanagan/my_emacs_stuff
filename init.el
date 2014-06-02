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

;; Any emacs setup which I want to occur on startup for every emacs
;;install I use goes here.

;;; Code:

(defun add-hooks-for-packages
    ()
  "Set up hooks which depend on packages that may not be synched on startup"
  (add-hook 'emacs-lisp-mode-hook
            (lambda
              ()
              ;; Use spaces, not tabs.
              (setq indent-tabs-mode nil)))
  (add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'auto-complete-mode))

(add-hook 'after-init-hook 'add-hooks-for-packages)

(require 'sync-packages)
(eval-after-load 'package '(install-missing-packages))
;;(eval-after-load 'package '(add-hooks-for-packages))??

(require 'eldoc) 
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;don't call server-start if server is already running
;;test copied from server.el
(require 'server)
(let
    ((file
      (expand-file-name "server"
                        (if server-use-tcp
                            server-auth-dir
                          server-socket-dir))))
  (if
      (not
       (file-exists-p file))
      (server-start)))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)))

;;; init.el ends here
