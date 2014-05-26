(defun emacs-config ()
  "Open .emacs file."
  (interactive)
  (find-file (concatenate 'string (getenv "HOME") "/.emacs")))

;; (defun toggle-night-color-theme ()
;;   "Switch to/from night color scheme.

;; Also switch colors for the `rainbow-delimiter-mode'."
;;   (interactive)
;;   (let ((day-theme 'dichromacy)
;; 	(night-theme 'bubbleberry))
;;     (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
;; 	(progn (load-theme day-theme)
;; 	       (load-theme-rainbow-delimiter :grey-tone)) 
;;       (progn (load-theme night-theme)
;; 	     (load-theme-rainbow-delimiter :flashy)))))


(defun test (&rest yada)
  (progn (prin1 yada))
	 (type-of yada))

;; (apply 'test (cdr (assoc :flashy list-themes-rainbow-delimiter)))

;; (defun load-theme-rainbow-delimiter (theme)
;; 	(interactive "Enter a name (flashy, grey-tone) : ")
;;   (apply 'custom-set-faces (cdr (assoc theme list-themes-rainbow-delimiter))))

;; (setq list-themes-rainbow-delimiter '((:flashy  
;; 				       (rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
;; 				       '(rainbow-delimiters-depth-3-face ((t (:foreground "cyan"))))
;; 				       '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
;; 				       '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
;; 				       '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
;; 				       '(rainbow-delimiters-depth-8-face ((t (:foreground "cyan"))))
;; 				       '(rainbow-delimiters-depth-9-face ((t (:foreground "blue violet"))))
;; 				       '(rainbow-delimiters-unmatched-face ((t (:foreground "orange red")))))
				     
;; 				      (:grey-tone
;; 				       '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-3-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-4-face ((t (:foreground "bluee"))))
;; 				       '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-7-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-8-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-depth-9-face ((t (:foreground "blue"))))
;; 				       '(rainbow-delimiters-unmatched-face ((t (:foreground "blue")))))))
