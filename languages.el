;; languages.el
;; For programming language-specific customizations

(add-to-list 'load-path
             (concat emacs-config-home "/languages/"))

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

;; Yacc/Bison mode
(load "yacc.el")

;; Open README files in text-mode
(setq auto-mode-alist (cons '("README" . text-mode) auto-mode-alist))
;; Open .hn (horn) files with c++-mode
(setq auto-mode-alist (cons '("\\.hn$" . c++-mode) auto-mode-alist))

;; Change default spacing in C
(setq c-basic-offset 4)

;; Set google's c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; set up css mode
(autoload 'css-mode "css-mode" "Major mode for editing css" t)
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq css-indent-level 2)

;; Lisp mode for anything that ends in .emacs
(setq auto-mode-alist (append '(("\\.emacs$" . lisp-mode))
                              auto-mode-alist))

;; C++ mode for header files
(setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))

;; PHP mode for .phpt files
(autoload 'php-mode "php-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.phpt$" . php-mode))
                              auto-mode-alist))

(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

;; php-mode
(setq magic-mode-alist (append '(("<\\?php\\s " . xhp-mode))
                              magic-mode-alist))
(setq auto-mode-alist (append '(("\\.php$" . xhp-mode))
                              auto-mode-alist))

;; Javascript mode for .js files
(autoload 'javascript-mode "javascript-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.js$" . javascript-mode))
                              auto-mode-alist))

;; highlight-80+
(require 'highlight-80+)

(add-hook 'javascript-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ))

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ; php-mode.el disables this, but that conflicts with arc lint
            (set (make-local-variable 'require-final-newline) t)
            ))

(add-hook 'xhp-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))


;; Thrift mode for .thrift files
(autoload 'thrift-mode "thrift" nil t nil)
(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

(if (locate-library "python")
    (require 'python)) ;; python mode available in emacs >= 22

(add-hook 'python-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ))

;; python mode for SConscript and SConstruct files
(setq auto-mode-alist (cons '("\\SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\SConscript" . python-mode) auto-mode-alist))

;; python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; python mode for .cconf, .cinc, and .ctest files, i.e. configerator files
(setq auto-mode-alist (cons '("\\.cconf" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cinc" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ctest" . python-mode) auto-mode-alist))
;; Same for tupperware .tw files.
(setq auto-mode-alist (cons '("\\.tw" . python-mode) auto-mode-alist))

;; d mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; show trailing whitespace ...
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
;; ... and terminate with extreme prejudice
(if (fboundp 'delete-trailing-whitespace)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)
)

;;=========================================================
;;C++ Function documentation template
;;========================================================
(defun c++-doc ()
    "Inserts a C++ doc function template"
    (interactive)
    (insert-file-contents (concat emacs-config-home
                                  "/languages/c++-doc-template.txt")))

;;=========================================================
;;Java Method documentation template
;;========================================================
(defun java-doc ()
    "Inserts a C++ doc function template"
    (interactive)
    (insert-file-contents (concat emacs-config-home
                                  "/languages/c++-doc-template.txt")))

;;=========================================================
;;PHP Doc Function template
;;========================================================
(defun php-doc ()
    "Inserts a phpdoc function template"
    (interactive)
    (insert-file-contents (concat emacs-config-home
                                  "/languages/php-doc-template.txt")))

;;=========================================================
;;PHP Indentation Style
;;========================================================
(defconst fb-php-style
  '((c-basic-offset . 2)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Facebook's PHP Programming style"
)
(c-add-style "fb-php-style" fb-php-style)

;;=========================================================
;;C, C++, Objective-C Indentation Style
;;========================================================
(require 'fb-objc)

(defconst fb-c-style
  '((c-basic-offset . 2)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        (innamespace . 0)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (objc-method-args-cont . fb-c-lineup-ObjC-method-args)
                        (objc-method-call-cont
                         (fb-c-lineup-ObjC-method-call-colons
                          fb-c-lineup-ObjC-method-call +))
                        )))
  "Facebook's C, C++, and Objective-C programming style"
)
(c-add-style "fb-c-style" fb-c-style)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "fb-c-style")
            (highlight-80+-mode t)
            ))

;;=========================================================
;;Java Indentation Style
;;========================================================
(defconst fb-java-style
  '((c-basic-offset . 4)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Facebook's Java programming style"
)
(c-add-style "fb-java-style" fb-java-style)

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "fb-java-style")
            (highlight-80+-mode t)
            (local-set-key "C-c j" java-doc)
            ))

;; Automatically select the appropriate mode based on matching the
;; text at the beginning of the file.
(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist
          (append (list
                   '("\\(.\\|\n\\)*\n@implementation" . objc-mode)
                   '("\\(.\\|\n\\)*\n@interface" . objc-mode)
                   '("\\(.\\|\n\\)*\n@protocol" . objc-mode)
                   '("\\(.\\|\n\\)*\nnamespace.*{" . c++-mode)
                   '("<\\?php\\s " . php-mode))
                  magic-mode-alist))
)

;; GraphViz Dot Mode
(require 'graphviz-dot-mode)

;; Significant Whitespace (sws-mode) and jade-mode
(add-to-list 'load-path
             (concat emacs-config-home "languages/jade-mode/"))
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;; Ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-keys))

;; Rails > Cucumber > feature-mode
;; (add-to-list 'load-path
;;              (concat emacs-config-home "/languages/cucumber.el/"))
;; (setq feature-default-language "en")
;; (require 'feature-mode)
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; csv-mode
(require 'csv-mode)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

(load-file (concat emacs-config-home "languages/piglatin-mode/piglatin.el"))
