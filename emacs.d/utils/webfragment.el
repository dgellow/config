;;; webfragment.el --- A simple way to modify embedded HTML code.
;;
;; Copyright (c) 2015 Samuel El-Borai
;;
;; Author: Samuel El-Borai <samuel.elborai@gmail.com>
;; URL: https://github.com/dgellow/home-bootstrapping
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
(defvar webfragment/buffers-offset '())

(defun webfragment/reload-font-lock ()
  (font-lock-mode -1)
  (font-lock-mode 1))

(defun webfragment/open-narrow-indirect-buffer ()
  (interactive)
  (let ((offset (current-indentation))
        (end (if (region-active-p)
                 (region-end)
               (line-beginning-position)))
        (buffername (format "WebFrag - %s"
                          (file-name-nondirectory
                           (or (buffer-file-name) (buffer-name))))))
    (progn
      (clone-indirect-buffer-other-window buffername t)
      (setq webfragment/buffers-offset
            (cons `(,buffername . ,offset) webfragment/buffers-offset))
      (narrow-to-region (line-beginning-position) end)
      (web-mode)
      (webfragment/reload-font-lock))))

(defun webfragment/kill-narrow-indirect-buffer ()
  (interactive)
  (let* ((buffername (buffer-name))
         (offset (cdr (assoc buffername webfragment/buffers-offset))))
    (progn
      (indent-code-rigidly (buffer-end -1) (buffer-end 1) (or offset 0))
      (kill-this-buffer)
      (setq webfragment/buffers-offset
            (delete (assoc buffername webfragment/buffers-offset)
                    webfragment/buffers-offset))
      (webfragment/reload-font-lock))))

(provide 'webfragment)
;;; webfragment.el ends here
