;;; fb-objc.el --- Facebook custom Objective-C indentation functions
;;; for CC Mode

;; Copied from cc-align.el and cc-vars.el from Emacs CVS, which is GPL
;; v3 or later.
;;
;; Copyright (C) 1985, 1987, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.

(require 'cc-defs)
(require 'cc-vars)
(require 'cc-engine)

(defcustom fb-c-objc-method-arg-min-delta-to-bracket 2
  "*Minimum number of chars to the opening bracket.

Consider this ObjC snippet:

        [foo blahBlah: fred
        |<-x->|barBaz: barney

If `x' is less than this number then `c-lineup-ObjC-method-call-colons'
will defer the indentation decision to the next function.  By default
this is `c-lineup-ObjC-method-call', which would align it like:

        [foo blahBlahBlah: fred
             thisIsTooDamnLong: barney

This behaviour can be overridden by customizing the indentation of
`objc-method-call-cont' in the \"objc\" style."
  :type 'integer
  :group 'c)

(defcustom fb-c-objc-method-arg-unfinished-offset 4
  "*Offset relative to bracket if first selector is on a new line.

    [aaaaaaaaa
    |<-x->|bbbbbbb:  cccccc
             ddddd: eeee];"
  :type 'integer
  :group 'c)

(defcustom fb-c-objc-method-parameter-offset 4
  "*Offset for selector parameter on a new line (relative to first selector.

    [aaaaaaa bbbbbbbbbb:
             |<-x->|cccccccc
                    ddd: eeee
                   ffff: ggg];"
  :type 'integer
  :group 'c)

(defun fb-c-lineup-ObjC-method-call (langelem)
  "Line up selector args as Emacs Lisp mode does with function args:
Go to the position right after the message receiver, and if you are at
the end of the line, indent the current line c-basic-offset columns
from the opening bracket; otherwise you are looking at the first
character of the first method call argument, so line up the current
line with it.

Works with: objc-method-call-cont."
  (save-excursion
    (let* ((extra (save-excursion
                    (back-to-indentation)
                    (c-backward-syntactic-ws (c-langelem-pos langelem))
                    (if (eq (char-before) ?:)
                        (- c-basic-offset)
                      0)))
           (open-bracket-pos (c-langelem-pos langelem))
           (open-bracket-col (progn
                               (goto-char open-bracket-pos)
                               (current-column)))
           (target-col (progn
                         (forward-char)
                         (c-forward-sexp)
                         (skip-chars-forward " \t")
                         (if (eolp)
                             (+ open-bracket-col c-basic-offset)
                           (current-column))))
           )
      (- target-col open-bracket-col extra))))

(defun fb-c-lineup-ObjC-method-call-colons (langelem)
  "Line up selector args as Project Builder / XCode: colons of first
   selector portions on successive lines are aligned.  If no decision can
   be made return NIL, so that other lineup methods can be tried.  This is
   typically chained with `fb-c-lineup-ObjC-method-call'.

Works with: objc-method-call-cont."
  (save-excursion
    (catch 'no-idea
      (let* ((method-arg-len (progn
                               (back-to-indentation)
                               (if (search-forward ":" (c-point 'eol) 'move)
                                   (- (point) (c-point 'boi))
                                 ; no complete argument to indent yet
                                 (throw 'no-idea nil))))

             (extra (save-excursion
                      ; indent parameter to argument if needed
                      (back-to-indentation)
                      (c-backward-syntactic-ws (c-langelem-pos langelem))
                      (if (eq ?: (char-before))
                          fb-c-objc-method-parameter-offset 0)))

             (open-bracket-col (c-langelem-col langelem))

             (arg-ralign-colon-ofs (progn
                        (forward-char) ; skip over '['
                        ; skip over object/class name
                        ; and first argument
                        (c-forward-sexp 2)
                        (if (search-forward ":" (c-point 'eol) 'move)
                            (- (current-column) open-bracket-col
                               method-arg-len extra)
                          ; previous arg has no param
                          fb-c-objc-method-arg-unfinished-offset))))

        (if (>= arg-ralign-colon-ofs fb-c-objc-method-arg-min-delta-to-bracket)
            (+ arg-ralign-colon-ofs extra)
          (throw 'no-idea nil))))))

(defun fb-c-lineup-ObjC-method-args (langelem)
  "Line up the colons that separate args in a method declaration.
The colon on the current line is aligned with the one on the first
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
           (curcol (progn (goto-char here) (current-column)))
           (eol (c-point 'eol))
           (relpos (c-langelem-pos langelem))
           (first-col-column (progn
                               (goto-char relpos)
                               (skip-chars-forward "^:" eol)
                               (and (eq (char-after) ?:)
                                    (current-column)))))
      (if (not first-col-column)
          c-basic-offset
        (goto-char here)
        (skip-chars-forward "^:" eol)
        (if (eq (char-after) ?:)
            (+ curcol (- first-col-column (current-column)))
          c-basic-offset)))))

(defun fb-c-lineup-ObjC-method-args-2 (langelem)
  "Line up the colons that separate args in a method declaration.
The colon on the current line is aligned with the one on the previous
line.

Works with: objc-method-args-cont."
  (save-excursion
    (let* ((here (c-point 'boi))
           (curcol (progn (goto-char here) (current-column)))
           (eol (c-point 'eol))
           (relpos (c-langelem-pos langelem))
           (prev-col-column (progn
                              (skip-chars-backward "^:" relpos)
                              (and (eq (char-before) ?:)
                                   (- (current-column) 1)))))
      (if (not prev-col-column)
          c-basic-offset
        (goto-char here)
        (skip-chars-forward "^:" eol)
        (if (eq (char-after) ?:)
            (+ curcol (- prev-col-column (current-column)))
          c-basic-offset)))))

(provide 'fb-objc)
