;;; texdrive.el --- minor mode for creating png images from TeX formulae 

;; Copyright (C) 2008-2009 Dirk-Jan C. Binnema.
;; Time-stamp: <2009-04-19 22:31:08 (djcb)>

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Created: 11 Jan 2009
;; Version: 0.3.1
;; Keywords: tex, hypermedia, multimedia, wp

;; This file is not part part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it undr the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Documentation:
;; texdrive creates png images from TeX formulas, and makes it really
;; easy to create html-pages with formulae. the real work is done by
;; latex and imagemagick, so you'll need to install those.
;; also see: http://emacs-fu.blogspot.com/2009/03/math-formulae-in-webpages.html

;; Usage:
;; suppose you are writing an html page, and you need to insert some formula.
;; you type:
;;   M-x texdrive-insert-formula[RET]a^2 + b^2 = c^2[RET]abc[RET]
;; now, at point, the following will be inserted:
;;
;; <img src="abc.png" title="abc" class="texdrive-formula" 
;;           name="a^2 + b^2 = c^2" border="0">
;;
;; the 'abc.png' is the image that will be created; it does not exist yet though.
;; after you have inserted one or more formulae, you type:
;;   M-x texdrive-generate-images-from-html[RET]
;; this will search the document for formulae as specified above, and generate
;; the images.

;; NOTE: while texdrive was specifically written to include the images in
;; webpages, it can be use for general formula-to-png conversion as well, 
;; using e.g.:
;; (texdrive-create-png "abc" "a^2 + b^2 = c^2")

;; Installation / customization:
;; copy texdrive.el to a place where emacs can find it, and in your .emacs
;; put something like
;;  (require 'texdrive)
;; then, to enable the texdrive minor mode:
;;  (texdrive-mode)
;; this will enable two keybindings:
;;
;; "C-c TAB f" for texdrive-insert-formula
;; "C-c TAB g" for texdrive-generate-images
;;
;; these keybindings fit in with the html-helper-mode keybindings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(defvar texdrive-img-path  ""
  "determines where to store the output image (relative to pwd)")

(defvar texdrive-tmp-path  "/tmp/" 
  "determines where to store temporary files")

(defvar texdrive-dvips  "dvips"    
  "name of the 'dvips' program on your system")

(defvar texdrive-convert "convert"   
  "name of the 'convert' program on your system")

(defvar texdrive-identify "identify"   
  "name of the 'identify' program on your system")

(defvar texdrive-latex "latex"
 "name of the 'latex' program on your system")

(defvar texdrive-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c TAB f") 'texdrive-insert-formula)
    (define-key map (kbd "C-c TAB g") 'texdrive-generate-images)
    map)
  "keymap for texdrive-minor-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst texdrive-output "*texdrive-output*"
  "name of texdrive output buffer")

(defconst texdrive-version "0.3.1")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun texdrive-basename-from-title-lst (lst)
  (when lst
    (concat
      (if (string< (car lst) "0") "-" (downcase (car lst)))
      (texdrive-basename-from-title-lst (cdr lst)))))

(defun texdrive-basename-from-title (title)
  "convert the formula title into a filename basename (without extension)"
  (texdrive-basename-from-title-lst (split-string title "" t)))

(defun texdrive-target-png (title)
  "target png file"
  (concat texdrive-img-path (texdrive-basename-from-title title) ".png"))

(defun texdrive-insert-formula (tex title)
  (interactive "sFormula (TeX):\nsTitle:")
  (insert
    (concat
      "<img " 
      "src=\"" (texdrive-target-png title) "\" "
      "title=\"" title "\" "
      "class=\"texdrive-formula\" "
      "alt=\"" tex   "\" "    
      "border=\"0\">"))) 

(defun texdrive-formulae-assoc ()
  "return an assoc of (<name> <formula>) starting from point"
  (when 
    (re-search-forward
      "<img src=\"\\(.+?\\)\.png\".*?class=\"texdrive-formula\".*?alt=\"\\(.+?\\)\"" 
      nil t)
    (cons (list 
	    (match-string-no-properties 1) 
	    (match-string-no-properties 2)) 
      (texdrive-formulae-assoc))))

(defun texdrive-buffer-formulae-assoc ()
  "return an assoc of (<name> <formula>) starting for the whole buffer"
  (save-excursion
    (goto-char 0)
    (texdrive-formulae-assoc)))

(defun texdrive-create-tmp-tex (name formula) 
  "create a tex file with the the latex for some formula;"
  "returns the filename created, or nil in case of error"
  (let ((texfile (concat texdrive-tmp-path name ".tex"))
	 (tex (concat
		"\\documentclass[10pt,notitlepage]{article}"
		"\\usepackage{amsmath}"
		"\\usepackage{amsmath}"
		"\\usepackage{amsfonts}"
		"\\usepackage{amssymb}"
  ;; to change the font used, one could uncommment, e.g.
  ;;   		"\\usepackage{mathpazo}" ;; or
  ;;		"\\usepackage{cmbright}" ;; or
  ;;		"\\usepackage[math]{iwona}"
  ;; see: http://ctan.tug.org/tex-archive/info/Free_Math_Font_Survey/survey.html
		"\\pagestyle{empty}"
		"\\begin{document}$" formula "$\\end{document}\n")))
    (with-temp-file texfile (insert tex))
    (message (concat "created: " texfile)) 
    texfile))

(defun texdrive-has-required-tools ()
  (let ((tools '(texdrive-latex texdrive-convert texdrive-dvips texdrive-identify))
	 (ok t))
    (mapc (lambda (tool)
	    (when (not (executable-find (symbol-value tool)))
	      (setq ok nil)
	      (message (concat "texdrive error: " (symbol-value tool)  
			 " not found; please install it."))))
      tools)
    ok))

(defun texdrive-create-tmp-dvi (name formula)
  "create a dvi file with some formula;"
  "returns the filename created, or nil in case of error"
  (when (texdrive-has-required-tools)
    (let ((texfile (texdrive-create-tmp-tex name formula))
	   (dvifile (concat texdrive-tmp-path name ".dvi")))
      (when texfile
	(if (and (= 0 (call-process texdrive-latex nil texdrive-output nil
			"-draftmode" "-file-line-error" "-halt-on-error"
			"-interaction=nonstopmode"
			(concat "-output-directory=" texdrive-tmp-path) texfile))
	      (file-exists-p dvifile))
	  dvifile
	  (progn (message 
		   "texdrive: LaTeX error; see the *texdrive-output* buffer") nil))))))
    
(defun texdrive-create-tmp-ps (name formula)
  "create a ps file with some formula;"
  "returns the filename created, or nil in case of error"
  (let ((dvifile (texdrive-create-tmp-dvi name formula))
	 (psfile (concat texdrive-tmp-path name ".ps")))
    (when dvifile
      (if  (= 0 (call-process 
		  texdrive-dvips nil texdrive-output nil "-o" psfile dvifile))
	psfile
	(progn (message (concat "texdrive: error while creating " psfile 
		   "; see *texdrive-output*")) nil)))))

(defun texdrive-png-is-uptodate (pngfile comment)
  "determine whether png file is up-to-date, by checking the comment field"
  (when (file-exists-p pngfile)
    (let ((png-comment (shell-command-to-string 
			 (concat texdrive-identify " -verbose " pngfile
			   "| grep '^ *Comment: ' "
			   "| sed 's/^ *Comment: //'"
			   "| tr -d '\n'"))))
      (when (string= png-comment comment) 
	(message (concat "\ntexdrive: up-to-date: " pngfile)) t))))

(defun texdrive-create-png (name formula)
  "create a ps file with some formula;"
  "returns the filename created, or nil in case of error"
  (let ((pngfile (texdrive-target-png name))
	 ;; we put the formula as a 'cookie' in a comment, so we can use
	 ;; it later to determine if the file needs updating
	 ;; we use base64 to avoid quoting issues
	 ;; TODO: check if there is some max length; a hash may be better.
	 (cookie (base64-encode-string formula t)))
    (if (texdrive-png-is-uptodate pngfile cookie)
      t
      (let ((psfile (texdrive-create-tmp-ps name formula)))
	(when psfile 
	  (message (concat "texdrive: creating: " pngfile))
	  (if (= 0 (call-process 
		     texdrive-convert nil texdrive-output nil
		     "-comment" cookie
		     "-density" "128"
		     psfile
		     "-trim" 
	;;	     "-resize" "50%"
		     "-transparent" "#ffffff" 
		     pngfile))
	    (progn (message (concat "texdrive: created: " pngfile)) pngfile)
	    (progn (message (concat "texdrive: error while creating " pngfile
			      "; see *texdrive-output*")) nil)))))))

(defun texdrive-generate-images-lst (lst)
  (when lst
    (let ((name (car (car lst))) (formula (cadr (car lst))))
      (when (texdrive-create-png name formula)
	(texdrive-generate-images-lst (cdr lst))))))

(defun texdrive-generate-images ()
  "generate png images for all the texdrive formulae in the buffer"
  (interactive)
  (let ((eqs (texdrive-buffer-formulae-assoc)))
    (texdrive-generate-images-lst eqs)))

(defun texdrive-version ()
  "show the texdrive version"
  (interactive)
  (message (concat "texdrive version: " texdrive-version)))

;;;###autoload
(define-minor-mode texdrive-mode
  "Toggle texdrive mode.
With no argument, this command toggles the mode.
A non-null prefix turns the mode on, while a null
argument turns the mode off.

When texdrive-mode is enabled,
  C-c TAB f 
inserts an <img...> element for a new formula (texdrive-insert-formula), while
  C-c TAB g
generates the images.

For more information, see: http://www.djcbsoftware.nl/code/texdrive/ and
 http://emacs-fu.blogspot.com/2009/03/math-formulae-in-webpages.html
"
  ;; the initial value
  :init-value nil
  ;; modeline indicator
  :lighter " TXD"
  ;; minor mode bindings
  :keymap texdrive-minor-mode-map)

(provide 'texdrive)

;;; texdrive.el ends here
