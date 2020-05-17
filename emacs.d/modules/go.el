;;; go.el --- golang configuration.
;;
;; Copyright (c) 2017 Samuel El-Borai
;;
;; Author: Samuel El-Borai <sam@elborai.me>
;; URL: https://github.com/dgellow/config/
;; Version: 1.0.0
;; Keywords: convenience, language

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

(defun dg-load-gopath ()
  "Load GOPATH from my shell config."
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")))

(defun dg-use-company-go ()
  "Only use company-go."
    (progn
      (set (make-local-variable 'company-backends) '(company-go))
      (company-mode)))

(use-package go-guru)
(use-package go-rename)
(use-package company-go
  :init
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0))
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook #'dg-load-gopath)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'dg-use-company-go))

(provide 'dg-go)
;;; go.el ends here
