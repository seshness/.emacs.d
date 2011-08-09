;;; xhp-mode.el --- major mode for editing PHP code with XHP support

;; Copyright 2004-present Facebook.  All Rights Reserved.

(require 'php-mode)

(defcustom xhp-mode-hook nil
  "list of functions to be executed on entry to xhp-mode."
  :type 'hook
  :group 'php)

(defun xhp-mode-xhp-context-p (start end)
  "Does the range include some XHP?"
  (if (< start end)
      (save-excursion
        (goto-char start)
        (re-search-forward "<[:a-zA-Z][-:a-zA-Z0-9]*" end t))
    nil))

;; This probably doesn't work for cases or labels because using ':' as a
;; statement separator is a bit tricky.  In practice, this means XHP indenting
;; inside a switch statement is still somewhat broken.
;; This also doesn't play nice with XML entities which include a ';'
(defun xhp-mode-beginning-of-statement ()
  "Find the point of the first character of the current statement."
  (let ((end-regexp "\\(;\\)\\|)\\s *{\\|<\\?php"))
    (save-excursion
      (re-search-backward end-regexp nil t)
      (let ((semicolon (match-string 1)))
        (re-search-forward end-regexp nil t)
        (re-search-forward (if semicolon "[\n }]*" "[\n ]*") nil t))
      (point))))

(defun xhp-mode-current-indent ()
  "Indentation level of the current statement."
  (save-excursion
    (goto-char (xhp-mode-beginning-of-statement))
    (current-column)))

(defun xhp-mode-back-to-indentation ()
  (let ((first-non-indent
         (save-excursion
           (back-to-indentation)
           (point))))
    (if (< (point) first-non-indent)
        (back-to-indentation))))

(defun xhp-mode-statement-first-line-p ()
  "Is this the first line of the current statement?"
  (let ((current-point (point)))
    (save-excursion
      (goto-char (xhp-mode-beginning-of-statement))
      (and (>= current-point (line-beginning-position))
           (<= current-point (line-end-position))))))

(defun xhp-mode-indent-line-xhp ()
  "Indent a line containing XHP."
  (let ((nesting-regex
         "\\(<[:a-zA-Z][-:a-zA-Z0-9]*\\|{\\)\\|\\(</\\|/>\\|}\\)\\|\\(=\\|return[ \n]\\)")
        (indent-from (line-beginning-position))
        (depth 0))
    (save-excursion
      (goto-char (xhp-mode-beginning-of-statement))
      (while (and (< (point) indent-from)
                  (re-search-forward nesting-regex indent-from t))
        (if (match-string 1) (incf depth))
        (if (match-string 2) (decf depth))
        (if (and (match-string 3) (equal depth 0)) (incf depth)))
      (goto-char indent-from)
      (indent-line-to
       (+ (xhp-mode-current-indent)
          (* 2 depth)
          (if (looking-at "\s*\\(?:</\\|/>\\)") -2 0))))
    (xhp-mode-back-to-indentation)))

;; This function uses a heuristic regexp parser to determine the current syntax
;; context which then determines the indentation method.  It is not always
;; correct.
(defun xhp-mode-indent-line ()
  "Modify indent for a line of PHP including support for XHP."
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (cond
     ;; If a line contains XHP, use the custom indent function
     ((xhp-mode-xhp-context-p (xhp-mode-beginning-of-statement) (line-end-position))
      (xhp-mode-indent-line-xhp))

     ;; A line that does not include XHP will sometimes still be indented
     ;; incorrectly if it is the first line after an XHP statement because the
     ;; preceeding XHP confuses c-indent.
     ((save-excursion
        (and (xhp-mode-statement-first-line-p)
             (re-search-backward "\\([;:{}]\\)" nil t)
             (equal (match-string 1) ";") ;; other cases are handled correctly
             (xhp-mode-xhp-context-p (xhp-mode-beginning-of-statement) (point))))
      (indent-line-to
       (save-excursion
         (re-search-backward ";" nil t) ;; goto end of previous statement
         (xhp-mode-current-indent)))) ;; find indentation level

     ;; If none of these cases apply, hopefully the normal c-indent-line
     ;; function will work just fine
     (t (c-indent-line))))
  (xhp-mode-back-to-indentation))

(defun xhp-mode-indent-region (start end &optional quiet)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (search-forward "\n" nil t)
      (xhp-mode-indent-line))))

(defconst xhp-mode-font-lock-keywords-1
  php-font-lock-keywords-1 ;; nothing to add at this level
  "Level 1 Font Lock for XHP Mode.")

(defconst xhp-mode-font-lock-keywords-2
  (append
   (list
    '("\\<\\(class\\|interface\\)\\s-+\\([:a-zA-Z][-:a-zA-Z0-9]*\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
    '("\\<\\(new\\|extends\\|implements\\)\\s-+\\$?\\([:a-zA-Z][-:a-zA-Z0-9]*\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face))
    ;; TODO: XHP declaration classes - 'attribute', 'children', 'category' keywords
    )
   xhp-mode-font-lock-keywords-1
   php-font-lock-keywords-2)
  "Level 2 Font Lock for XHP Mode.")

(defconst xhp-mode-font-lock-keywords-3
  (append
   (list
    ;; XHP "<abcd ..." and "</abcd ..."
    '("\\(</?\\)\\([a-zA-Z:\-]+\\)" (1 php-default-face) (2 font-lock-type-face))

    ;; XML entities
    '("&\\w+;" . font-lock-constant-face)

    ;; TODO: XHP element attributes
    )
   xhp-mode-font-lock-keywords-2
   php-font-lock-keywords-3)
  "Level 3 Font Lock for XHP mode.")


;;;###autoload
(define-derived-mode xhp-mode php-mode "XHP"
  "Major mode for editing PHP code with XHP.\n\n\\{xhp-mode-map}"
  (c-add-language 'xhp-mode 'c-mode)

  ;; Adapted from php-mode.el
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((xhp-mode-font-lock-keywords-1
           xhp-mode-font-lock-keywords-2
           xhp-mode-font-lock-keywords-3)
          nil                               ; KEYWORDS_ONLY
          t                                 ; CASE-FOLD
          (("_" . "w") (?# . "< b"))        ; SYNTAX-ALIST
          nil))                             ; SYNTAX-BEGIN

  (setq indent-line-function 'xhp-mode-indent-line)
  (setq indent-region-function 'xhp-mode-indent-region)

  (run-hooks 'xhp-mode-hook))

(provide 'xhp-mode)
