;;; style.el --- dgellow's style configuration.
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
;; Do not highlight current line
(global-hl-line-mode -1)

;; Do not use a blinking cursor
(blink-cursor-mode -1)

;; Parentheses colored according to depth
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Show lambda as the greek character
;; (add-hook 'prog-mode-hook 'pretty-mode)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Minimize margins
(when (boundp 'fringe-mode)
  (fringe-mode '(1 . 0)))

;; Prettier mode-line
(use-package powerline
  :config
  (progn
    (defun powerline-nano-theme ()
      "Setup a nano-like mode-line."
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (lhs (list (powerline-vc nil 'l)))
                              (rhs (list (powerline-raw "")
                                         (powerline-raw "%4l" nil 'l)
                                         (powerline-raw ",")
                                         (powerline-raw "%3c" nil 'r)
                                         (if (buffer-modified-p) (powerline-raw " ⠾" nil 'r)
                                           (powerline-raw "  " nil 'r))))
                              (center (list (powerline-major-mode nil 'l)
                                            (powerline-raw " - ")
                                            (powerline-raw "%b" nil 'r))))
                         (concat (powerline-render lhs)
                                 (powerline-fill-center nil (/ (powerline-width center) 2.0))
                                 (powerline-render center)
                                 (powerline-fill nil (powerline-width rhs))
                                 (powerline-render rhs)))))))
    (powerline-nano-theme)))

;; Set fonts
(defvar dg-default-font nil "Default font when using a graphic environment.")
(setq dg-default-font
      '(:family "Input Mono"
                :slant normal
                :weight thin
                :height 130
                :width normal))

(defvar dg-terminal-font nil "Terminal font.")
(setq dg-terminal-font
      '(:family "Input Mono"
                :slant normal
                :weight thin
                :height 130
                :width normal))

;; Set custom theme path
(setq custom-theme-directory
  (expand-file-name "themes" user-emacs-directory))

;; Default theme
(defun use-default-theme ()
  "Set dgellow's default theme."
  (interactive)
  ;; (dg-message "Load default theme.")
  (disable-theme 'faceless)
  (load-theme 'octopussy t)
  (when (boundp 'dg-default-font)
    (custom-set-faces `(default ((t ,dg-default-font))))))

;; Terminal theme
(defun use-terminal-theme ()
  "Set dgellow's theme when running Emacs in a terminal."
  (interactive)
  ;; (dg-message "Load terminal theme.")
  (disable-theme 'octopussy)
  (load-theme 'faceless t)
  (when (boundp 'dg-terminal-font)
    (custom-set-faces `(default ((t ,dg-terminal-font))))))

;; Startup theme
(defun set-startup-theme ()
  "Select a theme depending on the display mode Emacs has been run on (from a terminal or a graphic env)."
  (interactive)
  (if (display-graphic-p) (use-default-theme)
    (use-terminal-theme)))

(set-startup-theme)

(provide 'style)
;;; style.el ends here
