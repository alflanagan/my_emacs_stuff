;;; init.el -- Non-site-specific initialization - * - lexical-binding: t -*-
;; lexical bind causes problems with add-hook??
;; Copyright © 2014, A. Lloyd Flanagan

;; Author: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Maintainer: A. Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 2014
;; Version: 0.03

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
;; install I use goes here.

;; Note that mainly operations are added to `after-init-hook` to
;; ensure that packages are setup before they're called.

;;; Code:
(when (and (require 'cask "~/.cask/cask.el" t)
           ; should we initialize even without Cask?
           (file-exists-p  (expand-file-name "Cask" user-emacs-directory)))
  (declare-function cask-initialize "cask" (&optional project-path))
  (cask-initialize))

(defmacro add-hook-if-exists (a-hook a-function &rest args)
   "Add to hook A-HOOK an expression to call A-FUNCTION with arguments ARGS only if A-FUNCTION is a defined function when the hook is run."
                     `(add-hook ,a-hook (lambda () (if (functionp ,a-function)
						       (funcall ,a-function ,@args)))))

;; (defmacro add-hook-if-exists (a-hook a-function &rest args)
;;   `(add-hook ,a-hook 'wilma))

;; (macroexpand  (add-hook-if-exists 'js2-mode-hook 'prettify-symbols-mode))

(defun setup-elisp-pretty ()
  "Add to words auto-converted to unicode symbols."
  ;; if we've got pretty-symbols-mode, enable multiple categories
  (if (boundp 'pretty-symbol-categories)
      (setq pretty-symbol-categories '(lambda relational logical)))
  ;; prettify-symbols-alist is part of prog-mode, but only
  ;; on recent versions of emacs, so check
  ;; it's automatically buffer-local, so add to mode hook
  (if (boundp 'prettify-symbols-alist)
      (progn
        (push '("<=" . ?≤) prettify-symbols-alist)
        (push '(">=" . ?≥) prettify-symbols-alist))))

(defun add-hooks-for-packages ()
  "Set up hooks which depend on packages that need to be initialized by package system."
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil))) ;; Use spaces, not tabs.
  (add-hook-if-exists 'emacs-lisp-mode-hook 'auto-complete-mode)
  (add-hook-if-exists 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook-if-exists 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'semantic-mode)
  (add-hook-if-exists 'python-mode-hook 'flycheck-mode)
  (add-hook-if-exists 'python-mode-hook 'auto-complete-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'semantic-mode)
  (add-hook 'python-mode-hook (lambda () (add-hook 'before-save-hook  'delete-trailing-whitespace nil t)))
  ;; because ido-ubiquitous doesn't get options right
  (add-hook 'ert-simple-view-mode-hook 'ido-ubiquitous-mode)
  ;;  (add-hook 'emacs-lisp-mode-hook 'setup-elisp-prettify)
  (add-hook-if-exists 'emacs-lisp-mode-hook 'pretty-symbols-mode)
  (add-hook-if-exists 'emacs-lisp-mode-hook 'ipretty-mode)
  (add-hook-if-exists 'js2-mode-hook 'pretty-symbols-mode))

(add-hook 'after-init-hook 'add-hooks-for-packages)

(defun set-up-paradox-variables ()
  "Set up config variables for paradox package ratings."
  ;; don't use require, should load through cask
  (when (functionp 'paradox-list-packages)
    (defvar paradox-github-token)
    (defvar paradox-automatically-star)
    ;; token "paradox emacs packages" (github.com)
    (setq paradox-github-token "203b6e30c0c11af83706cc718380ca09c7edb7ae")
    (setq paradox-automatically-star nil)))

(defun start-server-if-none ()
  "Start the server processes, unless another process already has."
  ;; test copied from server.el
  (require 'server)
  (let ((file (expand-file-name "server"
                                (if server-use-tcp
                                    server-auth-dir
                                  server-socket-dir))))
    (if (not (file-exists-p file))
        (server-start))))

(defun set-up-rst-auto-complete ()
  "Set up auto-complete for `rst-mode` using package `auto-complete-rst`."
  (when (functionp 'auto-complete-rst-init)
      (auto-complete-rst-init)))

(defun set-up-elpy ()
  "Enable `elpy` package and set up options."
  (when (functionp 'elpy-enable)
    (declare-function elpy-enable "elpy"  (&optional skip-initialize-variables))
    (elpy-enable))
    ;;(elpy-use-ipython)
    ;; default of 1 often times out of (elpy-refactor)
    ;; (setq elpy-rpc--timeout 5)

  ;;https://github.com/jorgenschaefer/elpy/issues/137
  (when (and (boundp 'elpy-default-minor-modes) (functionp 'flycheck-mode))
    (setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))
    (add-to-list 'elpy-default-minor-modes 'flycheck-mode)))

(defun set-up-global-undo-tree ()
  "Enable `undo-tree` mode wherever it makes sense."
  (when (functionp 'global-undo-tree-mode)
    (global-undo-tree-mode)))

(defun set-up-easy-kill ()
  "Replace standard `kill-ring-save` and `mark-sexp` bindings."
  (when (functionp 'easy-kill)
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark)))

(defun set-up-eldoc ()
  "Keep `eldoc` from interfering with `paredit`.  We hope."
  (if (functionp 'eldoc-add-command)
      (eldoc-add-command
       'paredit-backward-delete
       'paredit-close-round)))

(require 'file-mode-exp)

(defun set-up-js2-mode ()
  "Tell Emacs to use `js2-mode' for files with interpreter \"node\".

If `js2-mode' is not found, falls back to `javascript-mode'."
  (if (functionp 'js2-mode)
      (progn 
        (file-mode-exp-set-interpreter-mode "node" 'js2-mode)
        (file-mode-exp-set-interpreter-mode "nodejs" 'js2-mode)
        ;; and while we're at it, set up file extension
        (while (rassoc 'javascript-mode auto-mode-alist)
          (setf (cdr (rassoc 'javascript-mode auto-mode-alist)) 'js2-mode))
        ;;(add-hook 'js2-mode-hook ')
        )
    ;; at least set up node interpreter anyway
    (file-mode-exp-set-interpreter-mode "node" 'javascript-mode)
    (file-mode-exp-set-interpreter-mode "nodejs" 'javascript-mode)))

(defun set-up-web-beautify ()
  "Set up keys to invoke web-beautify in appropriate modes."
  (eval-after-load 'js2-mode
    (lambda ()
      (if (boundp 'js2-mode-map)
          (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))))
  (eval-after-load 'json-mode
    (lambda ()
      (if (boundp 'json-mode-map)
          (define-key json-mode-map (kbd "C-c b") 'web-beautify-js))))
  (eval-after-load 'sgml-mode
    (lambda ()
      (if (boundp 'html-mode-map)
          (define-key html-mode-map (kbd "C-c b") 'web-beautify-html))))
  (eval-after-load 'css-mode
    (lambda ()
      (if (boundp 'css-mode-map)
          (define-key css-mode-map (kbd "C-c b") 'web-beautify-css)))))

(defmacro error-into-message (body)
  "Catch error in BODY, write message."
  `(condition-case err-data
       ,body
     ((error debug) (message "[init] failure! type %s, msg %s in %s."
                             (car err-data)
                             (cdr err-data)
                             (car ',body)))))

(defun set-up-packages ()
  "Does various setup and initialization operations."
  (error-into-message (start-server-if-none))
  (error-into-message (set-up-rst-auto-complete))
  (error-into-message (set-up-paradox-variables))
  (error-into-message (set-up-elpy))
  (error-into-message (set-up-global-undo-tree))
  (error-into-message (set-up-easy-kill))
  (error-into-message (set-up-eldoc))
  (error-into-message (set-up-js2-mode))
  (error-into-message (set-up-web-beautify)))

(add-hook 'after-init-hook 'set-up-packages)

;;; init.el ends here
