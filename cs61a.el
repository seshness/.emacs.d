;;  .emacs --- cs61a class master emacs startup file

;;  Copyright (C) 1998 Regents of California

;;  Author: Unknown
;;  Last modified: 10/02/01
;;  History:
;;    2001-10-02: auto-save-interval:100->1000 --David Schultz
;;


;; Commentary:

;; This is the class master cs61b emacs file and should only
;; be edited if you really need too. Please don't edit if you
;; don't know what to do. For problems email root@cory.

;;(defconst master (getenv "MASTERDIR"))
(defconst stkhome "/Applications/STk")

;;; Path

(setq load-path
      (append (mapcar 'expand-file-name
                      (list
                   (concat stkhome "/share/emacs/lisp")
                       (concat master "/lib/emacs/lisp")))
;        nil               ; MUST BE LAST IN LIST
              load-path))

;; Set Info directory back to the right thing.

(require 'info)
(setq Info-default-directory-list
      (append (mapcar 'expand-file-name
                 (list (concat stkhome "/lib/emacs/info")))
              Info-default-directory-list))


;;#############################################################################
;;***** Beginning of general customization

;;; completions package
;;(load "completion.el")
;;(initialize-completions)

;; Controlling backup files:
;;
;; Set the following variable to `nil' to disable the making of backup
;; files.

(setq make-backup-files t)


;; The following variables control how backup files are made, and are
;; only used if `make-backup-files' is non-nil.
;;
;; Backup files are created when a file is saved for the first time (and
;; the file already exists on disk).  Backup files can be created by
;; renaming the original file or by copying.
;;
;; Renaming means that Emacs renames the existing file so that it is a
;; backup file, then writes the buffer into a new file.  Any other names
;; that the old file had will now refer to the backup file.  The new
;; file is owned by you and its group is defaulted.  Note that this
;; method CAN CHANGE the ownerships of a file.  The variables
;; `backup-by-copying-when-linked' and `backup-by-copying-when-mismatch'
;; control whether or not the ownerships can change.
;;
; Copying means that Emacs copies the existing file into the backup
;; file, then writes the buffer on top of the existing file.  Any other
;; names that the old file had will now refer to the new (edited) file.
;; The file's owner and group are unchanged.  However, if you edit very
;; large file, backing up by copying can take a long time.
;;
;; The choice of renaming or copying is controlled by the variables
;; backup-by-copying, backup-by-copying-when-linked and
;; backup-by-copying-when-mismatch.  For most people, these variables
;; should have the following values:
;;
;; backup-by-copying                       nil
;;   backup-by-copying-when-linked           t
;;     backup-by-copying-when-mismatch         t
;;
;; If you want to backup by renaming, set the variable
;; `backup-by-copying' to nil; if you want to backup by copying, set the
;; variable `backup-by-copying' to non-nil.

(setq backup-by-copying nil)

;; If you want to use copying to create backups for files with multiple
;; names, set `backup-by-copying-when-linked' to non-nil.  This causes
;; the alternate names to refer to the latest version as edited.  This
;; variable is relevant only if backup-by-copying is nil.

(setq backup-by-copying-when-linked t)

;; If you want to create backups by copying if this preserves owner or
;; group, set `backup-by-copying-when-mismatch' to non-nil.  Renaming
;; may still be used (subject to control of other variables) when it
;; would not result in changing the owner or group of the file; that is,
;; for files which are owned by you and whose group matches the default
;; for a new file created there by you.  This variable is relevant only
;; if backup-by-copying is nil.

(setq backup-by-copying-when-mismatch t)


;; You can set `require-final-newline' to one of the following to
;; control newlines at the end of a file when the file is saved:
;;
;;     t               Silently place a newline at the end of the file
;;                       when the file is saved.  This is done only if
;;                 the file does not already end with a newline.
;; nil             Don't add newlines.
;;   (anything else) Ask the user what to do.

(setq require-final-newline t)


;; Set the following to `t' to create numbered backup files.  Set it to
;; `nil' to make numbered backup files only for those files that already
;; have them.  Set it to `never' to never make numbered backup files
;; (i.e., use "(setq version-control 'never)").

(setq version-control nil)


;; Set the following variable to `t' or `nil', depending on whether or
;; not you want Emacs to auto-save your files.  It is strongly suggested
;; that auto-save-default be set to `t' (enable auto-save) to prevent
;; much work from being lost in the event of a power failure or system
;; crash.

(setq auto-save-default t)


;; auto-save-interval is the number of keystrokes between auto-saves.
;; If it is set to zero, autosaving is disabled.

(setq auto-save-interval 1000)


;; If default-truncate-lines is non-nil, continuation lines are not
;; displayed; each line of text is given one and only one screen line.
;; In this case, lines longer than the screen/window width have to be
;; viewed using the scroll-left and scroll-right functions.
;; It is recommended that default-truncate-lines be set to `nil'.

(setq default-truncate-lines nil)
(setq truncate-partial-width-windows default-truncate-lines)


;; scroll-step is the number of lines to try scrolling a window when
;; point tries to move outside of a window.  If that fails to bring the
;; point back onto the screen the point is centered in the window
;; instead.  If scroll-step is zero, the point is always centered after
;; it moves outside of a window.

(setq scroll-step 0)


;; next-screen-context lines contains the number of lines of continuity
;; when scrolling a window.

(setq next-screen-context-lines 1)

;;**** Key-binding changes

;;(global-set-key "\eg" 'goto-line)
;;(global-set-key "\eW" 'copy-region-as-kill)
;; (global-set-key "\^w" 'backward-kill-word)
;; (global-set-key "\eq" 'query-replace)
;; (global-set-key "\eQ" 'query-replace-regexp)
;; (global-set-key "\^x\^e" 'compile)
;; (global-set-key [S-return] 'newline-and-indent)

;; Disable C-x n n, ^z, C-x C-l, C-x C-u

;;(global-unset-key "\^z")
;;(global-unset-key "\^xnn")
;; (global-unset-key "\^x\^l")
;; (global-unset-key "\^x\^u")


(if (and (eq window-system 'x) (eq emacs-major-version 20))
 (progn
  (load "faces")
  (setq hilit-mode-enable-list '())
  (require 'paren)
  (require 'faces)

;;  (set-background-color "moccasin")
;;  (set-foreground-color "black")
;;  (set-cursor-color "orange")
;;  (set-face-background 'region "SandyBrown")
;;  (make-face 'paren-match-face)
;;  (set-face-foreground 'paren-match-face "black")
;;  (set-face-background 'paren-match-face "chocolate")
;;  (setq show-paren-face 'paren-match-face)
))

;; Make .h and .H files default to C++ mode rather than C mode.
(setq auto-mode-alist
  (append
   '(("\\.H$" . c++-mode)
     ("\\.h$" . c++-mode))
   auto-mode-alist))


;; Set up C/C++ indentation
(setq c-tab-always-indent 'nil)
(setq c-indent-level 4)
;;(define-key c-mode-map "\r" 'newline-and-indent)
(setq c-continued-statement-offset 4)
(setq c-argdecl-indent 0)


;; tell emacs what program you would like it to use, for the instructional
;; machines you want "stk"

(setq scheme-program-name "stk-simply")

(load "cmuscheme")

(defun run-stk () "
   Remove the *scheme* buffer unless it is running Stk.  If there is no
   *scheme* buffer running Stk, create one.  Switch to the *scheme* buffer.
   FIXME: this won't kill EnvDraw"
   (interactive)
   (set-buffer (get-buffer-create "*scheme*"))
   (let ((proc (get-buffer-process "*scheme*")))
     (if (and proc (not (string-match
                       "stk$" (car (process-command proc)))))
  (progn
           (set-process-buffer proc nil)
           (kill-process proc))))
   (run-scheme "stk-simply"))

(defun run-envdraw () "
   Remove the *scheme* buffer unless it is running EnvDraw.  If there is no
   *scheme* buffer running EnvDraw, create one. Switch to the *scheme* buffer.
   FIXME: this is basically the same as run-stk above."
   (interactive)
   (set-buffer (get-buffer-create "*scheme*"))
   (let ((proc (get-buffer-process "*scheme*")))
     (if (and proc (not (string-match
                        "envdraw$" (car (process-command proc)))))
      (progn
           (set-process-buffer proc nil)
           (kill-process proc))))
   (run-scheme "envdraw"))

(defun run-half-scheme () "
   Run Scheme in half a window."
   (interactive)
   (split-window-vertically nil)
   (other-window 1)
   (call-interactively 'run-scheme))


;; Additional local key and menu definitions in Scheme mode.

(define-key scheme-mode-map [menu-bar scheme]
  (cons "Scheme" (make-sparse-keymap "Scheme")))

(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-load-file-and-go] '("Send Scheme File & Go" . scheme-load-file-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-load-file] '("Send Scheme File" . scheme-load-file))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-region-and-go] '("Send Region & Go". scheme-send-region-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-region] '("Send Region" . scheme-send-region))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-defn-and-go]
  '("Send Definition & Go" . scheme-send-definition-and-go))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-send-defn] '("Send Definition" . scheme-send-definition))
(define-key (lookup-key scheme-mode-map [menu-bar scheme])
  [scheme-indent-sexp] '("Indent S-expression" . scheme-indent-sexp))
(setq menu-bar-final-items (cons 'scheme menu-bar-final-items))


(define-key scheme-mode-map "\C-c\M-l" 'scheme-load-file-and-go)
(define-key scheme-mode-map "\r" 'newline-and-indent)

(defun scheme-load-file-and-go (file-name)
  "Load Scheme file FILE-NAME into the inferior Scheme process and then
go to Scheme buffer."
  (interactive (comint-get-source "Load Scheme file: " scheme-prev-l/c-dir/file
                                  scheme-source-modes t)) ; T because LOAD
                                                          ; needs an exact name
  (scheme-load-file file-name)
  (switch-to-scheme t))

(add-hook 'scheme-mode-hook
        (function
        (lambda ()
        (setq comment-start ";; "))))

(defun scheme-send-enclosing-definition () "
  Send the definition containing point to the *scheme* process."
  (interactive)
  (forward-char 7)
  (search-backward-regexp "^(define")
  (forward-sexp)
  (scheme-send-last-sexp)
  (if (not (null (search-forward "(define" nil t)))
    (backward-char 7)))

(global-set-key "\M-p"          'scheme-send-enclosing-definition)
(global-set-key "\M-s"          'run-half-scheme)
(define-key esc-map "\C-q"    'scheme-indent-sexp)

; stkdb debugger (PNH 8/20/2003)
;(require 'stkdb)

;; This lets students copy the .emacs file to their home computers
;; without errors, even if it might cause multiple loads.
;; -- Jordy Rose 2011-02-04
(load "stkdb" 'noerr)

;;; TEMPORARY FIX (PNH 9/18/2005).   REMOVE THESE LINES after TRANSFERING
;;; THIS MODIFIED DEFINITION TO stkdb.el!!!!

(defun force-stkdb-mode ()
  (if (not (scheme-active-p))
      (save-window-excursion
       (run-scheme scheme-program-name)
        (sleep-for 1)))
  (with-current-buffer scheme-buffer
    (or stkdb-mode (stkdb-mode 4))))


;; if you use special syntax, you can tell it what you want indented

(put 'sequence 'scheme-indent-function 0)
(put 'define-method 'scheme-indent-function 1)
(put 'slot-ref 'scheme-indent-function 0)

(put 'method 'scheme-indent-function 1)
(put 'default-method 'scheme-indent-function 0)
(put 'initialize 'scheme-indent-function 0)

;; Marking regions

;; (setq transient-mark-mode t)

;; Line numbers

(line-number-mode 1)

;; Printing

(load "lpr")

(defun print-buffer ()
  "Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr."
  (interactive)
  (print-region-1 (point-min) (point-max)
                (append lpr-switches (lpr-make-buffer-title)) t))

(defun lpr-make-buffer-title ()
  (list (concat "-b" (user-login-name) ":" (buffer-name) "<Emacs-buffer>")))

(setq lpr-command "enscript-stdin")

; Set lpr-headers-switches rather than lpr-switches to get
; around an odd bug in print-region-1.
(setq lpr-headers-switches "-2rG")

;; Set degree of noviceness

;; Don't complain about ESC ESC,  [commented out b/c emacs tutorial says so!]
;; (put 'eval-expression 'disabled nil)

(defvar abbreviated-buffer-file-name nil
   "A shortened version of the buffer's file name, if any (buffer local).")

(make-variable-buffer-local 'abbreviated-buffer-file-name)

(setq find-file-hooks
      (append find-file-hooks '(set-abbreviated-file-name)))
(setq write-file-hooks
      (append write-file-hooks '(set-abbreviated-file-name)))

(defun set-abbreviated-file-name ()
  (let* ((name (abbreviate-file-name buffer-file-name))
         (suffix-index (string-match "\\(/[^/]*/[^/]*/[^/]*\\)$" name)))
    (setq abbreviated-buffer-file-name
    (if (and suffix-index (> suffix-index 3))
           (concat "..." (substring name suffix-index))
          name))
    nil))

(setq-default mode-line-format
  '("EMACS {" host-name ": " mode-line-buffer-identification
    "%1*%1+" (line-number-mode " L%l") "}   "
    "%[(" mode-name minor-mode-alist "%n" mode-line-process
    ")%]   "
    (buffer-file-name ("{ " abbreviated-buffer-file-name " }   "))
    (-3 . "%p") "   " global-mode-string))

(set-default (quote mode-line-buffer-identification) (quote ("%b")))

;; Initial windows: Reminder file (if any) and Scheme.

; (shell)
; (call-interactively 'run-scheme)
; (split-window-vertically nil)
; (other-window 1)
; (switch-to-buffer "*shell*")
; (other-window 1)



(raise-frame (car (car (cdr (current-frame-configuration)))))

;; (if (eq window-system 'x)
;;     (let ((remfile (substitute-in-file-name "~$MASTER/adm/reminder"))
;;    frame-window)
;;       (if (file-readable-p remfile)
;;           (progn
;;          (save-window-excursion
;;          (find-file remfile)
;;           (let ((frame (make-frame '((minibuffer . nil)
;;                                   (height . 50)
;;                                         (width . 80)
;;                                          (name . "Class Notices. (Click here to put in the background.)")))))
;;                 (raise-frame frame)
;;           (setq frame-window (frame-selected-window frame))))
;;       (set-buffer "reminder")
;;       (goto-char (point-max))
;;       (beginning-of-line (- 3 (window-height frame-window)))
;;        (set-window-start frame-window (point))
;;       (set-buffer-modified-p nil)
;;           (toggle-read-only 1)))))


;; this stuff is to use (ding) Gnus as the newsreader.

;;(setq gnus-default-nntp-server "agate.berkeley.edu")
(setq gnus-default-nntp-server "news.csua.berkeley.edu")

(setq gnus-default-subscribed-newsgroups
      '("ucb.class.cs61a" "news.announce.newusers"
     "news.groups.questions" "gnu.emacs.gnus"))

;; (if (eq window-system 'x)
;;     (progn
;;       (set-face-font
;;        'italic "-adobe-courier-medium-o-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'modeline "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'default "-adobe-courier-medium-r-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'bold-italic "-adobe-courier-bold-o-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'bold "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'highlight "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1")
;;       (set-face-font
;;        'region "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1")
;; ))

; This function overrides the function of the same name in
; cmuscheme.el. -- brg, Aug 30 1998
;
(defun switch-to-scheme (eob-p)
  "Switch to the scheme process buffer.
   With argument, positions cursor at end of buffer."
  (interactive "P")
  (let ((buff (get-buffer scheme-buffer)))
    (if buff
    (if (not (eq buff (current-buffer)))
        (switch-to-buffer-other-window scheme-buffer))
;            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; Using this function to grab the scheme buffer instead of
; `pop-to-buffer' will prevent the currently selected window from
; ever being used to get the scheme buffer. -- brg, Aug 30 1998
;
      (error "No current process buffer. See variable scheme-buffer.")))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

;; Miscellaneous adjustments

; Turn off the confounded tool bar (PNH 8/20/2003)
(tool-bar-mode 0)

; Set readable menubar face [code copied from cus-face.el] (PNH 8/20/2003)
(require 'cus-face)
(let ((face 'menu)
      (spec '((((type x-toolkit)) (:width normal :family "Courier"))))
      (now nil)
      (comment nil))
  (put face 'saved-face spec)
  (put face 'saved-face-comment comment)
  (when now
    (put face 'force-face t))
  (when (or now (facep face))
    (put face 'face-comment comment)
    (make-empty-face face)
    (face-spec-set face spec)))

(defun set-program-coloring (arg)
  (interactive "P")
  (show-paren-mode arg)
  (global-font-lock-mode arg))

(set-face-foreground 'font-lock-string-face "sienna")

;;;######################################################################
;; Place any additional customization after the "load" command for
;; this file.
;;;######################################################################
