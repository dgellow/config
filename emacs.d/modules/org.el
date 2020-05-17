;;; org.el --- org-mode configuration.
;;
;; Copyright (c) 2014 Samuel El-Borai
;;
;; Author: Samuel El-Borai <sam@elborai.me>
;; URL: https://github.com/dgellow/config/
;; Version: 1.0.0
;; Keywords:convenience

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
(setq org-default-notes-file "~/Org/tasks.org")
(define-key global-map "\C-ctn" 'org-capture)
(define-key global-map "\C-cta" 'org-agenda)

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/Org/tasks.org" "Tasks")
         "* TODO %?
  %u")
        ("p" "Project idea" entry (file "~/Org/ideas.org")
         "* %?
  %u")
        ("r" "Cooking recipe" entry (file "~/Org/recipes.org")
         "* TODO
  %u
  :PROPERTIES:
  :Result:
  :Quantity:
  :Link:
  :END:

  | Quantity | Ingredient      | How            | Recipe            |
  |----------+-----------------+----------------+-------------------|")))


(use-package org
  :config (progn
            (setq org-src-fontify-natively t)
            (setq org-agenda-ndays 14)
            (setq org-agenda-start-on-weekday nil)
            (setq org-agenda-skip-scheduled-if-done t)
            (setq org-log-done 'note)))

(provide 'dg-org)
;;; org.el ends here
