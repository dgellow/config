;;; flycheck.el --- flycheck configuration.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <sam@elborai.me>
;; URL: https://github.com/dgellow/config/
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
(require 'flycheck)

(defun dg-add-jsxhint-flycheck-checker ()
  (progn
    (flycheck-define-checker javascript-jsxhint
      "A wrapper around JSHint supporting JSX syntax."

      :command ("/home/sam/jsxhint-ignore-stderr" source)
      :error-parser flycheck-parse-checkstyle
      :modes (js-mode js2-mode js3-mode))
    (add-to-list 'flycheck-checkers 'javascript-jsxhint)))

(defun dg-add-typeflow-flycheck-checker ()
  (progn
    (flycheck-define-checker javascript-typeflow
      "A javascript syntax and style checker using Facebook's Flow."
      :command ("flow" source-original)
      :error-patterns
      ((error line-start
              (file-name)
              ":"
              line
              ":"
              (minimal-match (one-or-more not-newline))
              ":"
              (message (minimal-match (and (one-or-more anything) "\n")))
              line-end))
      :modes (js-mode js2-mode js3-mode))
    (flycheck-add-next-checker 'javascript-jsxhint 'javascript-typeflow)))

(use-package flycheck
  :init (progn
          (add-hook 'after-init-hook 'global-flycheck-mode)
          (dg-add-jsxhint-flycheck-checker)
          (dg-add-typeflow-flycheck-checker)))

(provide 'dg-flycheck)
;;; flycheck.el ends here
