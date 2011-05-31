;; languages.el
;; For programming language-specific customizations

(add-to-list 'load-path 
	     (concat emacs-config-home "/languages/"))

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; Yacc/Bison mode
(load "yacc.el")

;; Change default spacing from 4 -> 2 spaces in C
(setq c-basic-offset 2)

;; Set google's c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; php-mode
(add-to-list 'auto-mode-alist
	     '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
