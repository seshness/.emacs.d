;; http://www.rubyist.net/~matz/a/yacc.el

;;;
;;; Emacs mode for editing Yacc (and presumably Bison) files.
;;; I would *love* to say this code belons to someone else!  UGLY!
;;;

(defvar yacc-show-c-code nil
  "If non-nil value means to disable selective-display and show all c code lines.")

(defconst yacc-colon-column 16
  "Column in which the colon separating a rule from its definitions will go.")

(defconst yacc-semi-column 16
  "Column in which the semicolon terminating a rule will go.")

(defconst yacc-code-indent 4
  "Indentation from yacc-colon-column of a code block.")

(defconst yacc-percent-column 47
  "Column in which a % (not part of two-letter token) will go.")

(defconst yacc-auto-newline nil
  "*Non-nil means automatically newline before and after braces and semicolons
inserted in yacc code.")

(defvar yacc-mode-abbrev-table nil
  "Abbrev table in use in yacc-mode buffers.")
(define-abbrev-table 'yacc-mode-abbrev-table ())

(defvar yacc-mode-map ()
  "Keymap used in yacc mode.")
(if yacc-mode-map
    ()
  (setq yacc-mode-map (make-sparse-keymap))
  (define-key yacc-mode-map "{" 'yacc-code-block)
  (define-key yacc-mode-map "\C-c\C-b" 'enter-yacc-code-block)
  (define-key yacc-mode-map "\C-c{" 'enter-yacc-code-block)
  (define-key yacc-mode-map ";" 'electric-yacc-semi)
  (define-key yacc-mode-map ":" 'electric-yacc-colon)
  (define-key yacc-mode-map "|" 'electric-yacc-bar)
  (define-key yacc-mode-map "%" 'electric-yacc-percent)
  (define-key yacc-mode-map "\C-c\C-n" 'yacc-narrow-to-c-section)
  (define-key yacc-mode-map "\177" 'backward-delete-char-untabify)
  (define-key yacc-mode-map "\t" 'yacc-indent-command))

(defvar yacc-mode-syntax-table nil
  "Syntax table in use in yacc-mode buffers.")

(if yacc-mode-syntax-table
    ()
  (setq yacc-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" yacc-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" yacc-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" yacc-mode-syntax-table)
  (modify-syntax-entry ?% "." yacc-mode-syntax-table)
  (modify-syntax-entry ?| "." yacc-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" yacc-mode-syntax-table))

(defun yacc-mode ()
  "Major mode for editing yacc code.
Entry into a brace causes C mode to be invoked on a narrowed region; C-C C-C
is used to resume yacc mode beyond the closing brace.
Tab indents for yacc code.
Comments are delimited with /* ... */ and indented with tabs.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{yacc-mode-map}
Variables controlling indentation style:
 yacc-auto-newline
    Non-nil means automatically newline before and after braces and semicolons
    inserted in yacc code.  (A brace also enters C mode.)
 yacc-colon-column
    The column in which a colon or bar will be placed.
 yacc-semi-column
    The column in which a semicolon will be placed.
 yacc-code-indent
    Indentation of a code block from yacc-colon-column.

Turning on yacc mode calls the value of the variable yacc-mode-hook with no
args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'parse-sexp-ignore-comments)
  (yacc-mode-setup)
  (run-hooks 'yacc-mode-hook))

(defun yacc-mode-setup ()
  "Reinstate the context of a yacc-mode buffer.  Used by yacc-mode and
yacc-widen."
  (use-local-map yacc-mode-map)
  (setq major-mode 'yacc-mode)
  (setq mode-name "Yacc parser")
  (setq local-abbrev-table yacc-mode-abbrev-table)
; doesn't seem to be executed?!  Twice gets it set right.
  (set-syntax-table yacc-mode-syntax-table)
  (set-syntax-table yacc-mode-syntax-table)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (setq paragraph-separate paragraph-start)
  (setq indent-line-function 'yacc-indent-line)
  (setq require-final-newline t)
  (setq comment-start "/* ")
  (setq comment-end " */")
  (setq comment-column 9)
  (if yacc-show-c-code
      nil
    (setq selective-display (+ yacc-colon-column 3))
    (setq selective-display-ellipses t))
  (setq parse-sexp-ignore-comments t)
  (save-excursion
    (goto-char (point-min))
    (let ((section 1))
      (while (not (or (eobp)
		      (and (looking-at "%%")
			   (= (setq section (1+ section)) 3))))
	(forward-line 1))
    (narrow-to-region (point-min) (point)))))

(defun yacc-code-block (arg)
  "Insert character (left brace) and correct line's indentation, then insert
its matching right brace, narrow to the space between them and enter C mode."
  (interactive "P")
  (if yacc-auto-newline
      (progn
	(yacc-indent-line)
	(or (yacc-head-p) (newline))))
  (if (not (yacc-head-p))
      (yacc-indent-line (+ yacc-colon-column yacc-code-indent)))
  (insert "{")
  (newline)
  (if (not (yacc-head-p))
      (yacc-indent-line (+ yacc-colon-column yacc-code-indent)))
  (insert "}")
  (if yacc-auto-newline
      (progn
	(newline)
	(if (not (yacc-head-p))
	    (yacc-indent-line (+ 2 yacc-colon-column)))
	(forward-line -1)))
  (beginning-of-line)
  (backward-char 2)
  (enter-yacc-code-block))

(defun enter-yacc-code-block ()
  "Enter the yacc code block started by the left brace at point.  If there is
none, search forward until we find one; error if we hit end of buffer.  This
function enters C-mode on a narrowed region consisting of the code block; to
exit, call (yacc-widen) or C-C C-C."
  (interactive)
  (let ((here (point))
	endblok)
    (save-excursion
      (while (not (or (eobp) (= (following-char) ?{)))
	(forward-char 1))
      (if (eobp)
	  (error "I don't see a C code block here."))
      (beginning-of-line)
      (setq here (point))
      (let ((yacc-brace-depth 0))
	(while (not (or
		      (eobp)
		      (and
			(= (following-char) ?})
			(= (setq yacc-brace-depth (1- yacc-brace-depth)) 0))))
	  (if (= (following-char) ?{)
	      (setq yacc-brace-depth (1+ yacc-brace-depth)))
	  (forward-char 1)))
      (if (eobp)
	  (error "The C code block here is malformed.  Try C mode."))
      (forward-line 1)
      (setq endblok (point)))
    (narrow-to-region here endblok))
  (setq selective-display nil)
  (skip-chars-forward "^{")
  (forward-char 1)
  (c-submode 'yacc-widen "Yacc code"))

(defvar yacc-code-map nil)

(defun c-submode (resume-function &optional submode-name)
  "Enter C mode on the current (narrowed) region, after arranging for C-C C-C
to restore the previous context."
  (let ((curmode-name mode-name)
	c-mode-name)
    (and (consp curmode-name) (setq curmode-name (car curmode-name)))
    (c-mode)
    (if (null yacc-code-map)
	(setq yacc-code-map (copy-keymap c-mode-map)))
    (use-local-map yacc-code-map)
    (setq c-mode-name mode-name)
    (and (consp c-mode-name) (setq c-mode-name (car c-mode-name)))
    (local-set-key "\C-c\C-c" resume-function)
    (setq mode-name (or submode-name (concat curmode-name " "
					     c-mode-name " block")))
    (message "Enter C-C C-C to return to %s mode." curmode-name)))

(defun yacc-widen ()
  "Resume yacc mode upon exit from a C-mode yacc code block via C-C C-C."
  (interactive)
  (goto-char (point-max))
  (if (= (preceding-char) ?\n)
      (backward-char 1))
  (widen)
  (beginning-of-line)
  (skip-chars-forward "^\n}")
  (if (= (following-char) ?})
      ()
    (beginning-of-line)
    (if (looking-at "[ \t]*$")
	()
      (end-of-line)
      (newline))
    (yacc-indent-line (+ yacc-colon-column yacc-code-indent))
    (insert "}"))
  ; the following *should* be conditional on yacc-auto-newline, but selective
  ; display chopping out the part of the file the point's in causes havoc!
  (if (eobp)
      (newline)
    (forward-line 1))
  (local-unset-key "\C-c\C-c")
  (yacc-mode-setup)
  ; In the case of a %{ in the header, we must locate the matching } and make
  ; sure it has a % in front of it and is alone on its line.  This, of course,
  ; means ignoring a possibly-nil yacc-auto-newline, all in the cause of this
  ; gross hack.  (I should call it hack-mode!)
  (if (and (yacc-head-p)
	   (save-excursion
	     (skip-chars-backward "^}")
	     (let ((yacc-brace-depth 0))
	       (while (not (or
			     (bobp)
			     (and
			       (= (preceding-char) ?{)
			       (= (setq yacc-brace-depth (1- yacc-brace-depth))
				  0))))
		 (if (= (preceding-char) ?})
		     (setq yacc-brace-depth (1+ yacc-brace-depth)))
		 (backward-char 1)))
	     (and (bobp)
		  (error "The start of this C code block has been deleted!"))
	     (backward-char 1)
	     (= (preceding-char) ?%)))
      (save-excursion
	(skip-chars-backward "^}")
	(backward-char 1)
	(or (= (preceding-char) ?%) (insert "%"))
	(forward-char 1)
	(or (eolp) (newline))))
  (recenter))

(defun yacc-narrow-to-c-section ()
  "Switch from editing the yacc definitions to the C code after the second
%% code.  C-C C-C will return to the yacc definitions."
  (interactive)
  (goto-char (point-max))
  (widen)
  (if (not (looking-at "%%"))
      (insert "%%\n"))
  (forward-line 1)
  (narrow-to-region (point) (point-max))
  (c-submode 'c-narrow-to-yacc-section "Yacc C-section"))

(defun c-narrow-to-yacc-section ()
  "Resume editing the yacc definitions part of a buffer."
  (interactive)
  (widen)
  (local-unset-key "\C-c\C-c")
  (yacc-mode-setup))

(defun electric-yacc-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if yacc-auto-newline
      (progn
	(yacc-indent-line)
	(or (blank-line-p) (newline))))
  (yacc-indent-line yacc-semi-column)
  (self-insert-command (prefix-numeric-value arg))
  (if yacc-auto-newline (newline)))

(defun electric-yacc-colon (arg)
  "Indent to the colon column or as close to it as possible if already beyond
it, then self-insert."
  (interactive "P")
  (if (or
	(save-excursion
	  (skip-chars-backward "A-Za-z0-9_")
	  (not (bolp)))
	(yacc-head-p))
      (self-insert-command (prefix-numeric-value arg))
    (indent-to yacc-colon-column)
    (self-insert-command (prefix-numeric-value arg))
    (insert " ")))

(defun electric-yacc-bar (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (or (yacc-head-p)
      (progn
	(if yacc-auto-newline
	    (progn
	      (yacc-indent-line)
	      (newline)))
	(yacc-indent-line yacc-colon-column)))
  (self-insert-command (prefix-numeric-value arg))
  (or (yacc-head-p) (insert " ")))

(defun electric-yacc-percent (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let ((in-head (yacc-head-p))
	(doubled (= (preceding-char) ?%))
	(blank (blank-line-p)))
    (and in-head
	 yacc-auto-newline
	 (not blank)
	 (not doubled)
	 (not (bobp))
	 (newline))
    (if (not doubled)
	(or blank in-head (indent-to yacc-percent-column))
      ; Is there a cleaner way to check for narrowing?
      (let ((old-min (point-min))
	    (old-max (point-max)))
	(unwind-protect
	    (progn
	      (widen)
	      (if (not (and (= old-min (point-min))
			    (= old-max (point-max))))
		  (error "You already have a C section in this file.")))
	  (narrow-to-region old-min old-max)))
      (backward-delete-char 1)
      (delete-horizontal-space)
      ; must NOT use var "blank" here -- it is false due to the first %!
      (and yacc-auto-newline (not (blank-line-p)) (newline))
      (insert "%"))
    (insert "%")
    (and doubled yacc-auto-newline
	 (progn
	   (newline)
	   (or in-head (forward-line -1))))
    (if (or in-head (not doubled))
	()
      (beginning-of-line)
      (narrow-to-region (point-min) (point))
      (yacc-narrow-to-c-section))))

(defun yacc-indent-line (&optional column)
  "Indent the current line to the specified column or otherwise to where it
belongs."
  ; NOT interactive!
  (if (not column)
      (setq column (yacc-calculate-indentation)))
  (let ((here (point)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to column))
    (skip-chars-forward " \t")))

(defun yacc-calculate-indentation ()
  "Calculate the current indentation level.  If we are outside a rule, the
indentation is 0 unless we have a comment -- which will be indented to the
comment column.  Within a rule, a line beginning with a bar is indented to the
colon column; if a semicolon, to the semicolon column; if a brace, to the colon
column plus block indent; otherwise to the colon column plus 2."
  (save-excursion
    (beginning-of-line)
    (cond
      ((yacc-head-p) 0)
      ((save-excursion
	 (skip-chars-backward " \t\n")
	 (beginning-of-line)
	 (looking-at "[ \t]*;\\|%%")) 0)
      ((looking-at "^[ \t]*;") yacc-semi-column)
      ((looking-at "^[ \t]*|") yacc-colon-column)
      ((looking-at "^[ \t]*/\\*") yacc-comment-column)
      ((looking-at "^[A-Za-z_][A-Za-z_0-9]*:") 0)
      ((looking-at "^[ \t]*[{}]") (+ yacc-colon-column yacc-code-indent))
      (t (+ yacc-colon-column 2)))))

(defun yacc-indent-command ()
  "Insert a tab if applicable, else reindent."
  (interactive)
  (if (save-excursion
	(skip-chars-backward " \t")
	(bolp))
      (yacc-indent-line)
    (insert-tab)))

(defun yacc-head-p ()
  "Return non-nil if we are in the head of a yacc buffer (before the first %%)"
  (save-excursion
    (beginning-of-line)
    (while (not (or (bobp) (looking-at "%%")))
      (forward-line -1))
    (bobp)))

(defun blank-line-p ()
  "Return non-nil if the line point is in contains only spaces and/or tabs."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (eolp)))
