;;; init.el --- dgellow's configuration entry point.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping/
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun dg-match-first (REGEXP STRING)
  "Return the first group matched by applying REGEXP on STRING."
  (string-match REGEXP STRING)
  (match-string 1 STRING))

(defun dg-message (STRING)
  "Print a message with `load-file-name' as context and STRING as value.
If `load-file-name' is empty, use the value of `current-buffer'."
  (let* ((filename (or load-file-name (current-buffer)))
         (regexp-filename "emacs.d/\\(.*\\.el$\\)")
         (context (dg-match-first regexp-filename filename)))
    (if context
      (message "[%s]: %s" context STRING))))

(dg-message "Welcome in dgellow's Emacs flavour.")

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-screen t)

;; Set paths
(defvar dg-utils-dir
  (expand-file-name "utils" user-emacs-directory)
  "Directory containing utility functions.
Located under ~/.emacs.d/utils/ .")

(defvar dg-modules-dir
  (expand-file-name "modules" user-emacs-directory)
  "Directory containing modules configuration files.
Located under  ~/.emacs.d/modules/ .")

(defvar dg-config-dirs
  (list dg-utils-dir dg-modules-dir)
  "List of directories containing config files.")

;; Load modules and utils
(defun dg-load-modules (DIRECTORY)
  "Load every *.el files in the given DIRECTORY."
  (when (file-exists-p DIRECTORY)
    (mapc 'load (directory-files DIRECTORY 't "^[^#].*\\.el$"))))

(dg-message "Load the modularized configuration.")
(dolist (dir dg-config-dirs)
        (dg-message (format"- %s" dir))
        (dg-load-modules dir))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-cljs-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(flycheck-clang-language-standard "c++14")
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (magit multi-term systemd jira-markup-mode dockerfile-mode yasnippet yaml-mode web-mode scss-mode rainbow-mode use-package rainbow-delimiters racer python-mode projectile powerline nim-mode markdown-mode js2-mode jedi irony go-mode flx-ido expand-region exec-path-from-shell diminish company-anaconda cider bind-key)))
 '(web-mode-code-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Input Mono" :background "#2B303B" :foreground "#C0C5CE" :slant normal :weight thin :height 130 :width normal)))))
