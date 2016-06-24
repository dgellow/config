(deftheme octopussy
  "Author: Samuel El-Borai aka dgellow
   Date: January 2 2014
   Url: https://github.com/dgellow/home-bootstrapping/tree/master/emacs.d/themes

   Theme based on kkga's spacegray https://github.com/kkga/spacegray")

(custom-theme-set-variables
 'octopussy
 '(linum-format " %7i "))

(custom-theme-set-faces
 'octopussy

 '(default ((t (:foreground "#C0C5CE" :background "#2B303B"))))
 '(region ((t (:background "#705B44"))))
 '(cursor ((t (:background "#ffffff"))))
 '(fringe ((t (:background "#2f2f2f" :foreground "#ffffff"))))
 '(linum ((t (:background "#202020" :foreground "#2f2f2f" :box nil :height 100))))

 '(minibuffer-prompt ((t (:foreground "#9489C4" :weight bold))))
 '(minibuffer-message ((t (:foreground "#ffffff"))))

 '(font-lock-keyword-face ((t (:foreground "#B48BA5"))))
 '(font-lock-type-face ((t (:foreground "#ECC97C"))))
 '(font-lock-constant-face ((t (:foreground "#A2BF8A"))))
 '(font-lock-variable-name-face ((t (:foreground "#BFC5CE"))))
 '(font-lock-builtin-face ((t (:foreground "#A8D881"))))
 '(font-lock-string-face ((t (:foreground "#A2BD84"))))
 '(font-lock-comment-face ((t (:foreground "#65737F"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#65737F"))))
 '(font-lock-function-name-face ((t (:foreground "#8CA1B4"))))
 '(font-lock-doc-string-face ((t (:foreground "#496B83"))))


 ;; Ruby
 ;; Instance variables
 (font-lock-add-keywords
  'ruby-mode
  '(("@\\([A-z0-9_]+\\)" 1 '(:foreground "#B8575C") t)
    ("\\(\"\\)" 0 'font-lock-function-name-face t)
    ("\\(?:\\(#{\\)\\)+.*?\\(?:}\\)+" 1 '(:foreground "#AC7965") t)
    ("\\(?:#{\\)\\(+.*?\\)\\(?:}\\)+" 1 'font-lock-function-name-face t)
    ("\\(?:#{\\)+.*?\\(?:\\(}\\)\\)+" 1 '(:foreground "#AC7965") t)) 'append)


 '(tooltip ((default nil) (nil nil)))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(button ((t (:inherit (link)))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#FF6600"))))

 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#003453"))
              (((class color) (min-colors 88) (background dark)) (:background "#003450"))
              (((class color) (min-colors 16) (background light)) (:background "#003450"))
              (((class color) (min-colors 16) (background dark)) (:background "#004560"))
              (((class color) (min-colors 8)) (:foreground "#000000" :background "#00FF00"))
              (t (:inverse-video t))))

 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#999999"))
           (((class color grayscale) (min-colors 88) (background dark)) (:foreground "#999999"))
           (((class color) (min-colors 8) (background light)) (:foreground "#00ff00"))
           (((class color) (min-colors 8) (background dark)) (:foreground "#ffff00"))))

 '(trailing-whitespace ((((class color) (background light)) (:background "#ff0000"))
                        (((class color) (background dark)) (:background "#ff0000"))
                        (t (:inverse-video t))))

 '(link ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#00b7f0"))
         (((class color) (background light)) (:underline t :foreground "#0044FF"))
         (((class color) (min-colors 88) (background dark)) (:underline t :foreground "#0099aa"))
         (((class color) (background dark)) (:underline t :foreground "#0099aa"))
         (t (:inherit (underline)))))

 '(link-visited ((default (:inherit (link)))
                 (((class color) (background light)) (:inherit (link)))
                 (((class color) (background dark)) (:inherit (link)))))

 '(header-line ((default (:inherit (mode-line)))
                (((type tty)) (:underline t :inverse-video nil))
                (((class color grayscale) (background light)) (:box nil :foreground "#222222" :background "#bbbbbb"))
                (((class color grayscale) (background dark)) (:box nil :foreground "#bbbbbb" :background "#222222"))
                (((class mono) (background light)) (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff"))
                (((class mono) (background dark)) (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))

 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "#99ccee" :background "#444444"))
            (((class color) (min-colors 88) (background dark)) (:foreground "#bb3311" :background "#444444"))
            (((class color) (min-colors 16)) (:foreground "#0088cc" :background "#444444"))
            (((class color) (min-colors 8)) (:foreground "#0088cc" :background "#444444"))
            (t (:inverse-video t))))

 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "#ffaaaa"))
                 (((class color) (min-colors 88) (background dark)) (:background "#880000"))
                 (((class color) (min-colors 16)) (:background "#FF0000"))
                 (((class color) (min-colors 8)) (:background "#FF0000"))
                 (((class color grayscale)) (:foreground "#888888"))
                 (t (:inverse-video t))))

 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "#77bbdd"))
                   (((class color) (min-colors 88) (background dark)) (:background "#77bbdd"))
                   (((class color) (min-colors 16)) (:background "#4499ee"))
                   (((class color) (min-colors 8)) (:background "#4499ee"))
                   (t (:underline t))))

 '(match ((((class color) (min-colors 88) (background light)) (:background "#3388cc"))
          (((class color) (min-colors 88) (background dark)) (:background "#3388cc"))
          (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
          (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF"))
          (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))




 '(powerline-active1 ((t (:inherit mode-line :background "gray21" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray46"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray20" :foreground "white smoke"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "gray46"))))

 '(mode-line ((t (:box nil :background "yellow green" :foreground "black"))))
 '(mode-line-buffer-id ((t (:weight normal))))
 '(mode-line-emphasis ((t (:weight normal :underline t))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:box nil :foreground "#777777" :background "#303030"))))

 '(rainbow-delimiters-depth-2-face ((t (:foreground "violet red"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "lawn green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "salmon"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "orange red")))))

(provide-theme 'octopussy)
