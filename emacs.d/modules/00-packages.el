;;; 00-packages.el --- Install dependencies.
;;
;; Copyright (c) 2017 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping
;; Version: 1.0.0
;; Keywords: extensions

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

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-install 'use-package)

(defvar dg--default-pin-target 'melpa-stable)

(defvar dg--packages
  '(anaconda-mode
    cider
    clojure-mode
    company
    company-anaconda
    company-go
    (:name company-glsl :pin melpa)
    dash
    dockerfile-mode
    expand-region
    exec-path-from-shell
    flx
    flx-ido
    flycheck
    (:name glsl-mode :pin melpa)
    go-mode
    go-guru
    go-rename
    irony
    jedi
    jira-markup-mode
    js2-mode
    magit
    markdown-mode
    multi-term
    (:name nim-mode :pin melpa)
    powerline
    projectile
    (:name python-mode :pin melpa)
    racer
    rainbow-delimiters
    (:name rainbow-mode :pin gnu)
    rust-mode
    s
    scss-mode
    slime
    web-mode
    yaml-mode
    yasnippet))

(dolist (i dg--packages)
  (let ((name (if (listp i) (plist-get i :name) i))
	(pin (if (listp i) (plist-get i :pin) dg--default-pin-target)))
    (eval `(use-package ,name
             :ensure t
             :pin ,pin))))

(use-package rust-mode
  :ensure t
  :pin melpa-stable)

(provide 'dg-packages)
;;; 00-packages.el ends here
