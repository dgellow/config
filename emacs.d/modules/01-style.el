;;; style.el --- dgellow's style configuration.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping/
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not port of GNU Emacs.

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
;; Do not highlight current line
(global-hl-line-mode -1)

;; Set custom theme path
(defvar dg-themes-dir
  (expand-file-name "themes" user-emacs-directory)
  "Directory containing dgellow's themes.")

;; Default theme
(defun use-default-theme ()
  "Set dgellow's default theme."
  (interactive)
  (disable-theme 'faceless)
  (load-theme 'octopussy)
  (when (boundp 'dgellow/default-font)
    (set-face-attribute 'default nil :font dgellow/default-font)))

;; Terminal theme
(defun use-terminal-theme ()
  "Set dgellow's theme when running Emacs in a terminal."
  (interactive)
  (disable-theme 'octopussy)
  (load-theme 'faceless)
  (when (boundp 'dgellow/terminal-font)
    (set-face-attribute 'default nil :font dgellow/terminal-font)))

;; Startup theme
(defun set-startup-theme ()
  "Select a theme depending on the way Emacs has been run (from a terminal or a graphic env)."
  (if window-system (use-default-theme)
    (use-terminal-theme)))

(set-startup-theme)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Do not use a blinking cursor
(blink-cursor-mode -1)

(provide 'style)
;;; style.el ends here
