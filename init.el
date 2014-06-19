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
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; token "paradox emacs packages"
(setq paradox-github-token "203b6e30c0c11af83706cc718380ca09c7edb7ae")

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
  (add-hook 'python-mode-hook 'auto-complete-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'semantic-mode)
  (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook  'delete-trailing-whitespace nil t)))
  ;; because ido-ubiquitous doesn't get options right
  (add-hook 'ert-simple-view-mode-hook 'ido-ubiquitous-mode))

(add-hook 'after-init-hook 'add-hooks-for-packages)
;; (require 'sync-packages)
;; (eval-after-load 'package '(syncpack-install-missing-packages))
;;(eval-after-load 'package '(add-hooks-for-packages))??

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;don't call server-start if server is already running
;;test copied from server.el
(require 'server)
(let ((file (expand-file-name "server"
                              (if server-use-tcp
                                  server-auth-dir
                                server-socket-dir))))
  (if (not (file-exists-p file))
      (server-start)))

(defun setup-elisp-prettify ()
  "Add to words auto-converted to unicode symbols."
  (if (boundp 'prettify-symbols-alist)
      (progn
        (push '("<=" . ?≤) prettify-symbols-alist)
        (push '(">=" . ?≥) prettify-symbols-alist))))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp-prettify)
(eval-after-load "rst" '(auto-complete-rst-init))

(require 'elpy)
(elpy-enable)
;;(elpy-use-ipython)
(elpy-clean-modeline)
;; default of 1 often times out of (elpy-refactor)
(setq elpy-rpc--timeout 5)

;;https://github.com/jorgenschaefer/elpy/issues/137
(when (require 'flycheck nil t)
  (setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))
  (add-to-list 'elpy-default-minor-modes 'flycheck-mode))

(global-undo-tree-mode)
;;; init.el ends here
