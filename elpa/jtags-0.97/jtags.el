;;; jtags.el --- enhanced tags functionality for Java development

;; Copyright (C) 2001-2011 Alexander Baltatzis, Johan Dykstrom

;; Author: Alexander Baltatzis <alexander@baltatzis.com>
;;	Johan Dykstrom <jody4711-sf@yahoo.se>
;; Maintainer: Johan Dykstrom <jody4711-sf@yahoo.se>
;; Created: May 2001
;; Version: 0.97
;; Keywords: languages, tools
;; URL: http://jtags.sourceforge.net

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main purpose of `jtags-mode' is to provide an improved tags lookup
;; function for Java source code, compared to the ordinary etags package.
;; While etags knows only the name of the identifier, jtags also knows the
;; context in which the identifier is used. This allows jtags to find the
;; correct declaration at once, instead of the declaration that happens to
;; appear first in the tags table file. The jtags package also contains a
;; function for completing partly typed class members, and functions for
;; managing tags table files.
;;
;; The following interactive functions are included in jtags mode:
;;
;; - jtags-member-completion:      find all completions of the partly typed
;;                                 method or variable name at point
;; - jtags-show-declaration:       look up and display the declaration of the
;;                                 indentifier at point
;; - jtags-show-documentation:     look up and display the Javadoc for the
;;                                 indentifier at point
;; - jtags-update-tags-files:      update all tags table files with the latest
;;                                 source code changes
;; - jtags-update-this-tags-file:  update the tags table file in which the
;;                                 class in the current buffer is tagged
;;
;; Throughout this file, the two terms DECLARATION and DEFINITION are used
;; repeatedly. The DECLARATION of an identifier is the place in the Java source
;; code where the identifier is declared, e.g. the class declaration. The
;; DEFINITION of an identifier is the data structure used by jtags to describe
;; the declaration, containing file name, line number etc.

;; Installation:

;; Place "jtags.el" in your `load-path' and place the following lines in your
;; init file:
;;
;; (autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
;; (add-hook 'java-mode-hook 'jtags-mode)

;; Configuration:

;; Add the Emacs "bin" directory to your path, and restart Emacs to make the
;; etags program available to jtags mode.
;;
;; Unzip the source code files that come with the JDK and other products you
;; use, e.g. JUnit. The etags program can only extract information from source
;; code files, and not from class files.
;;
;; Configure the tags table list in your init file. Include the directories
;; where you unzipped the external source code files, and the directories where
;; your project's source code is located.
;;
;; GNU Emacs example:
;;
;; (setq tags-table-list '("c:/java/jdk1.6.0/src"
;;                         "c:/projects/tetris/src"))
;; (setq tags-revert-without-query 't)
;;
;; XEmacs example:
;;
;; (setq tag-table-alist '(("\\.java$" . "c:/java/jdk1.6.0/src")
;;                         ("\\.java$" . "c:/projects/tetris/src")))
;; (setq tags-auto-read-changed-tag-files 't)
;;
;; Type `M-x jtags-update-tags-files' to update all of the files in the tags
;; table list. If you do not have write access to all of the tags table files,
;; e.g. in the JDK installation directory, you can copy the source code tree,
;; or ask the system administrator to create the tags table files for you. If
;; you are running Linux, you can start Emacs using the sudo command once, to
;; create the tags table files.
;;
;; The shell command that runs when you update tags table files is defined in
;; the variable `jtags-etags-command'. Change this variable to run a specific
;; version of etags, or to include other source code files in the tags table
;; files.
;;
;; To display Javadoc for third party libraries, you need to customize the
;; `jtags-javadoc-root-alist' and add Javadoc root URLs for these libraries.
;; Package jtags now supports both http URLs and file URLs.
;;
;; If you want to use the jtags submenu, set `jtags-display-menu-flag' to
;; non-nil. If this variable is non-nil, the jtags submenu will be displayed
;; when jtags mode is active.
;;
;; You can customize all the variables above, as well as the faces used in
;; member completion. Type `M-x customize-group' and enter group "jtags" for
;; the jtags mode variables, or "etags" for the tags table list.
;;
;; The jtags package defines four key bindings in the `jtags-mode-map':
;;
;; - C-,   is bound to `jtags-member-completion'
;; - M-,   is bound to `jtags-show-declaration'
;; - M-f1  is bound to `jtags-show-documentation'
;; - C-c , is bound to `jtags-update-this-tags-file'

;;; Code:

(eval-when-compile (require 'cl))
(require 'cc-mode)
(require 'easymenu)
(require 'easy-mmode)
(require 'etags)

;; ----------------------------------------------------------------------------
;; Customization:
;; ----------------------------------------------------------------------------

(defgroup jtags nil
  "Enhanced tags functionality for Java development."
  :link '(emacs-library-link :tag "Source File" "jtags.el")
  :group 'tools)

;; Faces:

(defface jtags-member-face
  '((t (:bold t)))
  "*Face used to display normal class members."
  :group 'jtags)
(defvar jtags-member-face
  (make-face 'jtags-member-face))

(defface jtags-final-member-face
  '((t (:foreground "seagreen" :bold t)))
  "*Face used to display final class members."
  :group 'jtags)
(defvar jtags-final-member-face
  (make-face 'jtags-final-member-face))

(defface jtags-static-member-face
  '((t (:italic t :bold t)))
  "*Face used to display static class members."
  :group 'jtags)
(defvar jtags-static-member-face
  (make-face 'jtags-static-member-face))

(defface jtags-final-static-member-face
  '((t (:foreground "seagreen" :italic t :bold t)))
  "*Face used to display final static class members."
  :group 'jtags)
(defvar jtags-final-static-member-face
  (make-face 'jtags-final-static-member-face))

(defface jtags-inherited-member-face
  '((t nil))
  "*Face used to display inherited class members."
  :group 'jtags)
(defvar jtags-inherited-member-face
  (make-face 'jtags-inherited-member-face))

(defface jtags-inherited-final-member-face
  '((t (:foreground "seagreen")))
  "*Face used to display inherited final class members."
  :group 'jtags)
(defvar jtags-inherited-final-member-face
  (make-face 'jtags-inherited-final-member-face))

(defface jtags-inherited-static-member-face
  '((t (:italic t)))
  "*Face used to display inherited static class members."
  :group 'jtags)
(defvar jtags-inherited-static-member-face
  (make-face 'jtags-inherited-static-member-face))

(defface jtags-inherited-final-static-member-face
  '((t (:foreground "seagreen" :italic t)))
  "*Face used to display inherited final static class members."
  :group 'jtags)
(defvar jtags-inherited-final-static-member-face
  (make-face 'jtags-inherited-final-static-member-face))

;; Other stuff:

(defcustom jtags-javadoc-root-list nil
  "This variable has been deprecated and will be removed."
  :type '(repeat file)
  :group 'jtags)

(defcustom jtags-javadoc-root-alist
  '(("^java\\." . "http://download.oracle.com/javase/6/docs/api")
    ("^javax\\." . "http://download.oracle.com/javase/6/docs/api")
    ("^org\\.w3c\\.dom" . "http://download.oracle.com/javase/6/docs/api")
    ("^org\\.xml\\.sax" . "http://download.oracle.com/javase/6/docs/api"))
  "*Alist of package patterns vs corresponding Javadoc root URLs.
Each element looks like (REGEXP . URL) where REGEXP is a regexp
that matches a group of imports, and URL is the Javadoc root URL
for that group of imports. The Javadoc root URL is where the
\"index.html\" file resides."
  :type '(alist :key-type regexp :value-type string)
  :group 'jtags)

(defcustom jtags-browse-url-function 'browse-url
  "*A function used by `jtags-browse-url' to display URLs.
This function will be called with one argument: the URL to display."
  :group 'jtags
  :type 'function)

(defcustom jtags-etags-command
  (if (eq system-type 'windows-nt)
      "dir /b /s *.java | etags --declarations --members -o %f -"
    "find `pwd` | grep -E java$ | etags --declarations --members -o %f -")
  "*The shell command to run when updating tags table files.
This variable allows you to customize how to update tags table files, e.g.
specify a path to the etags program. The sequence %f will be replaced by
the actual tags table file name when running the command."
  :type 'string
  :group 'jtags)

(defcustom jtags-display-menu-flag 't
  "*Non-nil means that the jtags submenu will be added to the menu bar.
Set this variable to nil if you do not want to use the jtags submenu. If
non-nil, the submenu will be displayed when jtags mode is active."
  :type 'boolean
  :group 'jtags)

(defcustom jtags-use-buffer-tag-table-list-flag nil
  "*Non-nil means use built-in function `buffer-tag-table-list' if available.
Set this variable to nil if you want to use `jtags-buffer-tag-table-list' to
parse `tag-table-alist' instead of the built-in function."
  :type 'boolean
  :group 'jtags)

(defcustom jtags-trace-flag nil
  "*Non-nil means that tracing is ON. A nil value means that tracing is OFF."
  :type 'boolean
  :group 'jtags)

;; ----------------------------------------------------------------------------
;; Generic functions:
;; ----------------------------------------------------------------------------

(defun jtags-uniqify-list (list)
  "Return a copy of LIST with all duplicates removed.
The original list is not modified. Example:

\(jtags-uniqify-list '\(1 3 1 5 3 1\)\) -> \(1 3 5\)"
  (let (unique)
    (while list
      (unless (member (car list) unique)
        (setq unique (cons (car list) unique)))
      (setq list (cdr list)))
    (nreverse unique)))

(defun jtags-filter-list (list predicate)
  "Return a list containing all elements in LIST that satisfy PREDICATE.
The original LIST is not modified. PREDICATE should be a function of one
argument that returns non-nil if the argument should be part of the result
list. Example:

\(jtags-filter-list '\(1 2 3 4 5\) \(lambda \(x\) \(> x 3\)\)\) -> \(4 5\)"
  (let (result)
    (while list
      (if (funcall predicate (car list))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))

(defsubst jtags-rotate-left (src)
  "Rotate the list SRC one position left, and return the result.
The original list is not modified."
  (if src
      (append (cdr src) (list (car src)))))

(defsubst jtags-get-line ()
  "Return the line number of the current buffer position."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

(defsubst jtags-line-to-point (line)
  "Convert LINE to a position in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (point)))

(defun jtags-file-name-directory (filename)
  "Return the directory component in file name FILENAME.
Unlike the built-in function `file-name-directory', this function also
normalizes the file name. File name components such as `..' and `~' are
expanded. Back slashes are replaced with forward slashes. The returned
string always ends with a slash."
  (setq filename (file-truename filename))
  (if (string-match "^[A-Za-z]:" filename)
      (setq filename (concat (downcase (substring filename 0 1))
                             (substring filename 1))))
  (if (not (file-directory-p filename))
      (setq filename (file-name-directory filename)))
  (setq filename (file-name-as-directory filename))
  (setq filename (replace-regexp-in-string "\\\\" "/" filename)))

;; ----------------------------------------------------------------------------
;; Private variables:
;; ----------------------------------------------------------------------------

(defconst jtags-version "0.97"
  "The current version of jtags mode.")

;;                              PACKAGE                  TYPE NAME          TYPE ARGS       ARRAY OR SPACE
(defconst jtags-type-regexp "\\([A-Za-z0-9_.]+\\\.\\)*\\([A-Za-z0-9_]+\\)\\(<[^>]+>\\)?\\(\\[\\]\\|[ \t\n]\\)+"
  "Defines a regular expression that matches a Java type.
The matched string is grouped into subexpressions as described below.

subexp   description
------   -----------

1        Package name, including the last dot
2        Type name
3        Generic type arguments, including the angle brackets
4        Array square brackets and/or space")

;; This regexp is copied from "etags.el" in GNU Emacs 22.1
(defconst jtags-tag-line-regexp
  "^\\(\\([^\177]+[^-a-zA-Z0-9_+*$:\177]+\\)?\
\\([-a-zA-Z0-9_+*$?:]+\\)[^-a-zA-Z0-9_+*$?:\177]*\\)\177\
\\(\\([^\n\001]+\\)\001\\)?\\([0-9]+\\)?,\\([0-9]+\\)?\n"
  "This monster regexp matches an etags tag line.
The matched string is grouped into subexpressions as described below.

subexp   description
------   -----------

1        The string to match
2        Not interesting
3        The guessed tag name
4        Not interesting
5        The explicitly-specified tag name
6        The line to start searching at
7        The char to start searching at")

(defvar jtags-last-possible-completions nil
  "Holds a list of the text to be replaced and possible completions.
This variable is used to circle completions.")

(defvar jtags-buffer-name "*jtags*"
  "The name of the jtags temporary buffer.")

;; ----------------------------------------------------------------------------
;; Utility functions:
;; ----------------------------------------------------------------------------

(defun jtags-message (string &rest args)
  "Display message STRING at the bottom of the screen if tracing is ON.
The message also goes into the `*Messages*' buffer.

The first argument is a format control string, and the rest are data
to be formatted under control of the string. See `format' for details.

See `jtags-trace-flag' on how to turn tracing ON and OFF."
  (when jtags-trace-flag
    (save-excursion
      (save-match-data

        ;; Get name of calling function
        (let* ((frame-number 0)
               (function-list (backtrace-frame frame-number))
               (function-name nil))
          (while function-list
            (if (symbolp (cadr function-list))
                (setq function-name (symbol-name (cadr function-list)))
              (setq function-name "<not a symbol>"))
            (if (and (string-match "^jtags-" function-name)
                     (not (string-match "^jtags-message$" function-name)))
                (setq function-list nil)
              (setq frame-number (1+ frame-number))
              (setq function-list (backtrace-frame frame-number))))

          ;; Update argument list
          (setq args (append (list (concat "%s:\t" string) function-name) args)))

        ;; Print message
        (apply 'message args)))))

;; The DEFINITION of an identifier is a struct type with the following elements:
;;
;; file       The name of the file containing the declaration
;; line       The line number where the declaration starts
;; package    The package in which the class (see below) is declared
;; class      The class in which the identifier is declared
;; name       The name of the identifier, i.e. the class name for classes
;; type       The type of the identifier, i.e.
;;                  - attributes = attribute type
;;                  - classes    = \"class\"
;;                  - enums      = \"enum\"
;;                  - interfaces = \"interface\"
;;                  - methods    = return type
(defstruct (jtags-definition) file line package class name type)

(defsubst jtags-type-p (definition)
  "Return non-nil if DEFINITION is a type (e.g. class), and not a class member."
  (string-match "^\\(class\\|interface\\|enum\\)$" (jtags-definition-type definition)))

;; ----------------------------------------------------------------------------
;; GNU Emacs/XEmacs compatibility:
;; ----------------------------------------------------------------------------

(unless (functionp 'replace-regexp-in-string)
  (defsubst replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))

(eval-when-compile
  (when (featurep 'xemacs)
    (defsubst jtags-visit-tags-table-buffer (name)
      (set-buffer (get-tag-table-buffer name))))
  (unless (featurep 'xemacs)
    (defsubst jtags-visit-tags-table-buffer (name)
      (visit-tags-table-buffer name))))

;; This macro is copied from "cc-defs.el" in GNU Emacs 22.1
(defmacro jtags-save-buffer-state (varlist &rest body)
  `(let* ((modified (buffer-modified-p)) (buffer-undo-list t)
          (inhibit-read-only t) (inhibit-point-motion-hooks t)
          before-change-functions after-change-functions
          deactivate-mark
          ,@varlist)
     (unwind-protect
         (progn ,@body)
       (and (not modified)
            (buffer-modified-p)
            (set-buffer-modified-p nil)))))

;; ----------------------------------------------------------------------------
;; Main functions:
;; ----------------------------------------------------------------------------

(defun jtags-show-declaration ()
  "Look up the identifier around or before point, and show its declaration.

Find the definition of the identifier in the tags table files. Load and
display the Java source file where the identifier is declared, and move
the point to the first line of the declaration.

A marker representing the point when this command is invoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark]."
  (interactive)
  (let ((definition (jtags-find-tag)))

    ;; If we found a definition in the tags table, load the file and go to
    ;; the first line of the declaration
    (if (null definition)
        (message "Tag not found!")

      ;; Record whence we came
      (if (featurep 'xemacs)
          (push-tag-mark)
        (ring-insert find-tag-marker-ring (point-marker)))

      (find-file (jtags-definition-file definition))
      (goto-char (point-min))
      (forward-line (1- (jtags-definition-line definition)))
      (message "Found %s in %s"
               (jtags-definition-name definition)
               (jtags-definition-file definition)))))

(defun jtags-show-documentation ()
  "Look up the identifier around or before point, and show its Javadoc.

Find the definition of the identifier in the tags table files.
Find the Javadoc for the identifier using the Javadoc root list,
and display it in the configured browser.

See also variable `jtags-javadoc-root-alist'."
  (interactive)
  (let ((definition (jtags-find-tag)))
    (if (null definition)
        (message "Tag not found!")
      (jtags-browse-url definition))))

(defun jtags-member-completion ()
  "Look up a partly typed identifier around or before point, and complete it.

Find all the classes which the identifier can belong to, find all matching
members in those classes, and display the list of members in the echo area.
Members with certain properties, e.g. 'static', can be displayed using a
different face.

The first member in the list is selected as the default completion. Typing
\\<jtags-mode-map>\\[jtags-member-completion] again will select the next member in the list, and so on.

When completing members in a class (and not an object), only static members
will be displayed."
  (interactive)
  (let ((last-completion nil))

    ;; If first call - find the text to be completed, and all completions
    (if (not (eq last-command 'jtags-member-completion))
        (setq jtags-last-possible-completions (jtags-find-member-completions))

      ;; Not first call, cycle through the possible completions
      (when jtags-last-possible-completions
          (setq last-completion (cadr jtags-last-possible-completions))
          (setq jtags-last-possible-completions
                (cons (car jtags-last-possible-completions)
                      (jtags-rotate-left (cdr jtags-last-possible-completions))))))
    ;; (jtags-message "Possible completions=%S" jtags-last-possible-completions)

    ;; If no completion found
    (if (not jtags-last-possible-completions)
        (message "No completion!")

      ;; If not first call, remove last completion
      (if (eq last-command 'jtags-member-completion)
          (let ((region-length (- (length last-completion)
                                  (length (car jtags-last-possible-completions)))))
            (delete-region (- (point) region-length) (point))))

      ;; Insert only the part that needs to be completed
      (insert (substring (cadr jtags-last-possible-completions)
                         (length (car jtags-last-possible-completions))))

      ;; Display completions
      (if (= (length jtags-last-possible-completions) 2)
          (message "Found one completion: %s"
                   (jtags-format-members (cdr jtags-last-possible-completions)))
        (message "Found %d completions: %s"
                 (1- (length jtags-last-possible-completions))
                 (jtags-format-members (cdr jtags-last-possible-completions)))))))

(defun jtags-format-members (members)
  "Format MEMBERS, a list of class members, for printing in the echo area.
The result is a string where each member is separated by a space, and where
members with certain text properties, e.g. 'static' are fontified using a
different face. See function `jtags-get-tagged-members'."
  (let ((result))
    (dolist (member members result)
      (let ((member-face "jtags-")
            (member-name (copy-sequence member)))

        (if (get-text-property 0 'inherited member)
            (setq member-face (concat member-face "inherited-")))
        (if (get-text-property 0 'final member)
            (setq member-face (concat member-face "final-")))
        (if (get-text-property 0 'static member)
            (setq member-face (concat member-face "static-")))

        (setq member-face (concat member-face "member-face"))

        ;; Convert face name to symbol, and set face on member
        (set-text-properties 0 (length member) (list 'face (intern member-face)) member-name)

        (setq result (concat result (if result " ") member-name))))))

;; ----------------------------------------------------------------------------
;; Functions for finding which class(es) the point is in:
;; ----------------------------------------------------------------------------

(defun jtags-get-surrounding-classes (filename)
  "Return a list of classes that the point is within.
The list starts with the innermost class, and ends with the outermost class.
FILENAME is the name of the current buffer."
  (jtags-filter-class-list (jtags-find-classes filename)))

(defun jtags-filter-class-list (class-list)
  "Return a list of classes that the point is within.
Look for the end of each class in CLASS-LIST and decide if point is
in that class or not. Return the list of classes that the point is
in, with the innermost class first and the outermost class last."
  (if class-list
      (let* ((class (caar class-list))
             (start (jtags-line-to-point (cdar class-list)))
             (end (jtags-find-end-of-class start))
             (pos (point)))
        (jtags-message "Class `%s' spans [%s - %s], pos=%s" class start end pos)
        (if (and (>= pos start) (<= pos end))
            (cons class (jtags-filter-class-list (cdr class-list)))
          (jtags-filter-class-list (cdr class-list))))))

(defun jtags-find-end-of-class (start)
  "Find end of class with start position START."
  (save-excursion
    (goto-char start)
    (re-search-forward "{")

    ;; Scan buffer to find closing brace, i.e. end-of-class. A scan error
    ;; ("Unbalanced parentheses") usually means that the user is in the
    ;; middle of editing a block or an expression -> the point is in the
    ;; class -> use `point-max' as end-pos.
    (condition-case nil
        (scan-sexps (1- (point)) 1)
      (error (point-max)))))

;; ----------------------------------------------------------------------------
;; Functions for finding base classes for a list of classes:
;; ----------------------------------------------------------------------------

(defun jtags-get-class-list (&optional class)
  "Return the list of classes that are available at point.

The list starts with the class that the point is in, continues with its base
classes, and ends with \"Object\". If the point is inside an inner class, the
list continues with the outer class, and its base classes.

If the optional argument CLASS is specified, the returned list will contain
CLASS and its base classes."
  (save-excursion
    (let ((src (if (null class)
                   (jtags-get-surrounding-classes (buffer-file-name))
                 (list class))))
      (jtags-message "Source classes=%S" src)
      (jtags-uniqify-list (jtags-get-class-list-iter src)))))

(defun jtags-get-class-list-iter (class-list)
  "Return the class hierarchies for all classes in CLASS-LIST as a flat list.
Subfunction used by `jtags-get-class-list'. Example:

\(jtags-get-class-list-iter '(\"String\" \"Integer\")) ->
               (\"String\" \"Object\" \"Integer\" \"Number\" \"Object\")"
  (if class-list
      (append (jtags-do-get-class-list (car class-list))
              (jtags-get-class-list-iter (cdr class-list)))))

(defun jtags-do-get-class-list (class &optional package-list)
  "Return the class hierarchy for CLASS, ending with class \"Object\".
If we cannot look up CLASS in the tags files, end the class list with CLASS.
The optional argument PACKAGE-LIST is a list of package names, or fully
qualified class names."
  ;; Lookup the class and if found, load its Java file
  ;; Otherwise, end the class list with this class
  (let ((definition (jtags-lookup-identifier class nil package-list))
        (regexp (concat "class\\W+"
                        class
                        "\\(\\W*<[^>]+>\\)*"
                        "\\W+extends\\W+"
                        "\\([A-Za-z0-9_.]+\\\.\\)*\\([A-Za-z0-9_]+\\)")))
    (jtags-message "Definition of `%s'=%S" class definition)
    (if (null definition)
        (list class)
      (set-buffer (find-file-noselect (jtags-definition-file definition) t))
      (goto-char (point-min))

      ;; Find out which class this class extends
      (if (re-search-forward regexp nil t)
          (let ((super-class (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
                package-list)

            ;; If class included a package name
            (if (match-beginning 2)
                (setq package-list (list (concat (buffer-substring-no-properties (match-beginning 2)
                                                                                 (match-end 2))
                                                 "*")))
              (setq package-list (jtags-find-imports (jtags-definition-package definition))))
            (jtags-message "Class `%s' extends class `%s'" class super-class)

            ;; Find out which class the super class extends
            (cons class (jtags-do-get-class-list super-class package-list)))

        ;; If this class does not extend any class, end the class list with
        ;; "Object" (if this class isn't "Object")
        (if (string-equal class "Object")
            (list class)
          (list class "Object"))))))

;; ----------------------------------------------------------------------------
;; Functions for looking up tags:
;; ----------------------------------------------------------------------------

(defun jtags-find-tag ()
  "Look up the identifier around or before point, and return its DEFINITION.
Parse the current buffer, other Java files, and the tags table files in
`tags-table-list' to find the declaration of the identifier. If found, the
DEFINITION of the identifier is returned."
  (let* ((identifiers (jtags-parse-java-line))
         (classes (jtags-get-class-list))
         (definition (jtags-local-tags-lookup identifiers classes)))
    (jtags-message "Local definition=%s" definition)

    ;; If no local definition, find class member definition
    (when (null definition)
      (setq definition (jtags-recursive-tags-lookup identifiers classes))
      (jtags-message "Recursive definition=%s" definition))

    definition))

(defun jtags-recursive-tags-lookup (identifiers classes)
  "Look up an identifier recursively, and return its DEFINITION.
IDENTIFIERS is a list of identifiers, starting with the first identifier
in the expression, and ending with the identifier to look up. CLASSES is
a list of classes to search for the identifier. If the identifier is found,
its DEFINITION is returned.

This function will not look up constructors, as it does not know if an
identifier refers to a constructor or its class. The DEFINITION of the
class will be returned instead."
  (jtags-message "Identifiers=%S, classes=%S" identifiers classes)
  (when identifiers
    (let ((definition nil)
          (looking-for-class (null classes)))

      ;; If no classes to look in, lookup first identifier as a class
      (if looking-for-class
          (setq definition (jtags-lookup-identifier (car identifiers)))

        ;; Special handling of string literals, and keywords class, super, and this
        (cond ((string-equal (car identifiers) "class")
               (setcar identifiers "Class"))
              ((string-equal (car identifiers) "#stringliteral#")
               (setcar identifiers "String"))
              ((string-equal (car identifiers) "super")
               (setcar identifiers (cadr classes)))
              ((string-equal (car identifiers) "this")
               (setcar identifiers (car classes))))

        ;; Look up the first identifier as a member in the first class, but not
        ;; if the identifier might be a constructor. We don't want to look up
        ;; and return a constructor, since we don't know here that the user
        ;; really wanted a constuctor, and not a class. The class will be found
        ;; in a later call to this function.
        (unless (equal (car classes) (car identifiers))
          (setq definition (jtags-lookup-identifier (car classes) (car identifiers)))))
      (jtags-message "Definition=%S" definition)

      ;; If we did not find the identifier, look for it in next class (if we
      ;; were not looking for a class)
      (if (null definition)
          (if (not looking-for-class)
              (jtags-recursive-tags-lookup identifiers (cdr classes)))

        ;; If this is the last identifier in the list, return the result
        (if (= (length identifiers) 1)
            definition

          ;; Otherwise, lookup the next identifier using the class (and its
          ;; base classes) returned from this lookup
          (let ((found-class (if looking-for-class
                                 (jtags-definition-class definition)
                               (jtags-definition-type definition))))
            (jtags-recursive-tags-lookup (cdr identifiers)
                                         (jtags-get-class-list found-class))))))))

(defun jtags-local-tags-lookup (identifiers classes)
  "Look up a local variable definition, and return its DEFINITION.
IDENTIFIERS is a list of identifiers, starting with the first identifier in the
expression, and ending with the identifier to look up. CLASSES is a list of
classes, but only the first one is used to search for the identifier. If the
identifier is found, its DEFINITION is returned."
  (jtags-message "Identifiers=%S, classes=%S" identifiers classes)
  (if (or (null identifiers) (null classes))
      nil
    ;; Use start of class as search boundary
    (let* ((class-def (jtags-lookup-identifier (car classes)))
           (start-line (jtags-definition-line class-def))
           (local-def (jtags-find-declaration (car identifiers) start-line)))
      (jtags-message "Local tag-def=%S" local-def)
      (if local-def
          ;; If there are more identifiers, look them up. Otherwise, return the
          ;; definition of the local variable.
          (if (cdr identifiers)
              (jtags-recursive-tags-lookup (cdr identifiers)
                                           (jtags-get-class-list (car local-def)))
            (make-jtags-definition :file (buffer-file-name)
                                   :line (cadr local-def)
                                   :package (jtags-find-package)
                                   :class (car classes)
                                   :name (car identifiers)
                                   :type (car local-def)))))))

;; ----------------------------------------------------------------------------
;; Functions for looking up completions:
;; ----------------------------------------------------------------------------

(defun jtags-find-member-completions ()
  "Look up the identifier around point, and return a list of completions.

The returned list will start with the text to be completed, and continue
with all matching members in the class, and in its super classes. Members
that are not visible in the current scope will be discarded, and members
that are defined in a super class will be marked with text property
'inherited'. When looking up members in a class (and not an object), only
static members will be included.

Return nil if there are no matching members."
  (save-excursion
    (let* ((case-fold-search nil)
           (identifiers (reverse (jtags-parse-java-line)))
		   (to-be-completed (car identifiers))
		   (surrounding-classes (jtags-get-class-list))
           lookup-classes
           class-members
           last-identifier
		   definition
		   members)

      (if (null identifiers)
          nil

        ;; Is there some text to be completed?
        (if (looking-at "\[A-Za-z0-9_\]")
            (setq identifiers (reverse (cdr identifiers)))
          (save-excursion
            (skip-chars-backward " \t\n")
            (backward-char)

            (if (not (eq (char-after) ?\.)) ;; are we at a .
                (setq identifiers (reverse (cdr identifiers)))
              ;; we will complete the empty string
              (setq identifiers (reverse identifiers))
              (setq to-be-completed ""))))

        ;; If no identifiers left, we try to complete a member without any
        ;; preceding class or object - lookup members in "this" class
        (if (null identifiers)
            (setq identifiers (list "this")))
        (jtags-message "Identifiers=%s, to-be-completed=`%s'" identifiers to-be-completed)
        (setq last-identifier (car (reverse identifiers)))

        ;; Find definition of identifier
        (setq definition (jtags-local-tags-lookup identifiers surrounding-classes))
        (unless definition
          (setq definition (jtags-recursive-tags-lookup identifiers surrounding-classes)))
        (jtags-message "Definition=%s" definition)

        ;; If we found a definition of the identifier
        (when definition
          ;; Get list of classes to look in
          (setq lookup-classes (jtags-get-class-list (if (jtags-type-p definition)
                                                         (jtags-definition-class definition)
                                                       (jtags-definition-type definition))))
          (jtags-message "Look in classes=%s, surrounding classes=%s"
                         lookup-classes surrounding-classes)

          ;; For each class in lookup-classes, lookup its members
          (dolist (class lookup-classes)
            (setq class-members (jtags-lookup-class-members class))
            ;; (jtags-message "All members in `%s' (%d)=%s" class (length class-members) class-members)

            ;; Keep only visible members
            (setq class-members (jtags-filter-list
                                 class-members
                                 (lambda (x)
                                   (cond ((get-text-property 0 'private x)
                                          (equal class (car surrounding-classes)))
                                         ((get-text-property 0 'protected x)
                                          (member class surrounding-classes))
                                         (t t)))))
            (jtags-message "Visible members in `%s' (%d)=%s" class (length class-members) class-members)

            ;; If looking up members in a class, and not an object, keep only static members
            ;; Special handling of string literals, and keywords class, super, and this
            (when (and (jtags-type-p definition)
                       (not (string-match "^\\(class\\|super\\|#stringliteral#\\|this\\)$" last-identifier)))
              (setq class-members (jtags-filter-list
                                   class-members
                                   (lambda (x) (get-text-property 0 'static x))))
              (jtags-message "Static members in `%s' (%d)=%s" class (length class-members) class-members))

            ;; Add text property for inherited class members
            (unless (equal class (car lookup-classes))
              (mapc (lambda (x) (add-text-properties 0 1 '(inherited t) x)) class-members))

            (setq members (append members class-members)))

          ;; If there are any visible members
          (when members
            ;; Find matching members
            (setq members (jtags-filter-list
                           members
                           (lambda (x) (string-match (concat "^" to-be-completed) x))))
            ;; (jtags-message "Matching members (%d)=%s" (length members) members)

            ;; Remove duplicates
            (setq members (jtags-uniqify-list members))
            (jtags-message "Unique members (%d)=%s" (length members) members)

            ;; If there are any matching members, return a list beginning with
            ;; the identifier to be completed, and ending with the matching
            ;; members sorted in alphabetical order. Otherwise, return nil.
            (if members
                (cons to-be-completed (sort members 'string<)))))))))

;; ----------------------------------------------------------------------------
;; Functions that operate on the tags table files:
;; ----------------------------------------------------------------------------

;; Parts of this function are copied from "etags.el" in XEmacs 21.4.21
(defun jtags-buffer-tag-table-list ()
  "Return a list of tags tables to be used for the current buffer.
Use `tag-table-alist' if defined, otherwise `tags-table-list'.
Setting variable `jtags-use-buffer-tag-table-list-flag' to non-nil
means use the built-in function `buffer-tag-table-list' instead.

The primary difference between this function, and `buffer-tag-table-list',
is that this function does not load the tags table files in the list. This
means that you can call this function before creating the tags table files."
  (cond ((not (boundp 'tag-table-alist))
         tags-table-list)

        ((and jtags-use-buffer-tag-table-list-flag (functionp 'buffer-tag-table-list))
         (buffer-tag-table-list))

        (t
         (let* (result
                expression
                (key (or buffer-file-name (concat default-directory (buffer-name))))
                (key (replace-regexp-in-string "\\\\" "/" key)))

           ;; Add `buffer-tag-table' if defined
           (when (and (boundp 'buffer-tag-table) buffer-tag-table)
             (when (file-directory-p buffer-tag-table)
               (setq buffer-tag-table (concat (jtags-file-name-directory buffer-tag-table) "TAGS")))
             (push buffer-tag-table result))

           ;; Add matching items from `tag-table-alist'
           (dolist (item tag-table-alist)
             (setq expression (car item))
             ;; If the car of the alist item is a string, apply it as a regexp
             ;; to the buffer-file-name. Otherwise, evaluate it. If the regexp
             ;; matches, or the expression evaluates non-nil, then this item
             ;; in tag-table-alist applies to this buffer.
             (when (if (stringp expression)
                       (string-match expression key)
                     (ignore-errors (eval expression)))
               ;; Now, evaluate the cdr of the alist item to get the name of
               ;; the tag table file.
               (setq expression (ignore-errors (eval (cdr item))))
               ;; If expression is a directory name, add file name TAGS.
               (when (file-directory-p expression)
                 (setq expression (concat (jtags-file-name-directory expression) "TAGS")))
               (push expression result)))
           (or result (error "Buffer has no associated tag tables"))
           (jtags-uniqify-list (nreverse result))))))

(defun jtags-lookup-identifier (class &optional member package-list)
  "Look up an identifier in the tags table files, and return its DEFINITION.

Look up the CLASS and its MEMBER in the tags table files. If no MEMBER is
provided, just look up the CLASS. The optional argument PACKAGE-LIST is a list
of package names, or fully qualified class names. If specified, a check is made
to verify that CLASS belongs to one of the packages in the list, see function
`jtags-right-package-p'.

Looking up a constructor will work if CLASS is equal to MEMBER, and CLASS has
a defined constructor. Default constructors will not be looked up."
  (save-excursion
    (jtags-message "Class=`%s', member=`%s', packages=%S" class member package-list)

    (let ((case-fold-search nil)
          (tags-list (jtags-buffer-tag-table-list)))
      (block while-tags-list
        (while tags-list
          (jtags-visit-tags-table-buffer (car tags-list))
          (goto-char (point-min))

          ;; (jtags-message "Looking in tags file `%s'" (buffer-file-name))
          (let (class-pos
                next-class-pos
                type-line-pos
                file-name)

            ;; Find class (or interface or enum)
            (while (re-search-forward (concat "\\(class\\|interface\\|enum\\)\\W+" class "\\W") nil t)
              (setq class-pos (point))

              ;; Find next class to limit searching
              (if (re-search-forward "\\(\\|\nclass\\|\ninterface\\|\nenum\\)" nil t)
                  (setq next-class-pos (1+ (match-beginning 0)))
                (setq next-class-pos (point-max)))

              ;; Find file name
              (setq file-name (jtags-get-tagged-file class-pos))
              (jtags-message "Found class in file `%s'" file-name)

              ;; If the package is right, find all methods and attributes
              (when (jtags-right-package-p file-name package-list)

                ;; Get member type
                (goto-char class-pos)
                (setq type-line-pos (jtags-get-tagged-type-line-pos class member next-class-pos))
                ;; (jtags-message "Type-line-pos=%S" type-line-pos)

                ;; If we found a match
                (when type-line-pos
                  (set-buffer (find-file-noselect file-name t))
                  (return-from while-tags-list
                    (make-jtags-definition :file file-name
                                           :line (nth 1 type-line-pos)
                                           :package (jtags-find-package)
                                           :class class
                                           :name (if member member class)
                                           :type (nth 0 type-line-pos)))))

              ;; Wrong package - find another class with the same name
              (goto-char next-class-pos))

            ;; Look in the next tags file
            (setq tags-list (cdr tags-list))))))))

(defun jtags-lookup-class-members (class &optional package-list)
  "Return a list of all members in CLASS, or nil if CLASS is not found.
The optional argument PACKAGE-LIST is a list of package names, or fully
qualified class names. If specified, a check is made to verify that CLASS
belongs to one of the packages in the list, see `jtags-right-package-p'."
  (save-excursion
    (jtags-message "Class=`%s', packages=%S" class package-list)
    (let ((case-fold-search nil)
          (tags-list (jtags-buffer-tag-table-list)))
      (block while-tags-list
        (while tags-list
          (jtags-visit-tags-table-buffer (car tags-list))
          (goto-char (point-min))

          ;; (jtags-message "Looking in tags file `%s'" (buffer-file-name))
          (let (class-pos
                next-class-pos
                file-name)

            ;; Find class (or interface or enum)
            (while (re-search-forward (concat "\\(class\\|interface\\|enum\\) " class "\\W") nil t)
              (setq class-pos (point))

              ;; Find next class to limit searching
              (if (re-search-forward "\\(\\|\nclass\\|\ninterface\\|\nenum\\)" nil t)
                  (setq next-class-pos (1+ (match-beginning 0)))
                (setq next-class-pos (point-max)))

              ;; Find file name
              (setq file-name (jtags-get-tagged-file class-pos))
              (jtags-message "Found class in file `%s'" file-name)

              ;; If the package is right, find all methods and attributes
              (when (jtags-right-package-p file-name package-list)
                (goto-char class-pos)
                (return-from while-tags-list (jtags-get-tagged-members class next-class-pos)))

              ;; Wrong package - find another class with the same name
              (goto-char next-class-pos))

              ;; Look in the next tags file
              (setq tags-list (cdr tags-list))))))))

(defun jtags-get-tagged-type-line-pos (class member bound)
  "Look in CLASS, and return type, line and position of MEMBER.

Parse CLASS (up to position BOUND) in the tags table file and
return the type, line and position of MEMBER. Return nil if
MEMBER is not found. Used by `jtags-lookup-identifier'."
  (if member
      (block while-tag-line
        (while (re-search-forward jtags-tag-line-regexp bound t)
          (let ((found-member (match-string 3))
                (line (match-string 6))
                (pos (match-string 7)))
            (when (string-equal member found-member)
              (jtags-message "Found member `%s' at pos %s" member pos)

              ;; If another member is declared at the same point this is an invalid tag
              (unless (jtags-next-member-same-pos-p member pos bound)

                ;; If we are looking for a constructor (member and class names are equal)
                (if (string-equal member class)
                    (return-from while-tag-line (list member
                                                      (string-to-number line)
                                                      (string-to-number pos)))

                  ;; Not constructor - find member type
                  (goto-char (match-beginning 3))

                  ;; If we can match a type name
                  (if (re-search-backward (concat "[ \t\n]+" jtags-type-regexp) nil t)
                      (return-from while-tag-line (list (match-string 2)
                                                        (string-to-number line)
                                                        (string-to-number pos))))))))))

    ;; No member supplied - we are looking for a class or interface or enum
    (end-of-line)
    (if (re-search-backward "\\(class\\|interface\\|enum\\).*[^0-9]\\([0-9]+\\),\\([0-9]+\\)" nil t)
        (list (match-string 1)
              (string-to-number (match-string 2))
              (string-to-number (match-string 3))))))

(defun jtags-next-member-same-pos-p (member pos bound)
  "Return non-nil if next member in class is declared at same POS as MEMBER.
The search is bounded by BOUND, which should be the end of the class.

This function exists because of a bug in the etags program in GNU Emacs 21."
  (when (and (not (featurep 'xemacs)) (< emacs-major-version 22))
    (save-excursion
      (save-match-data
        (if (re-search-forward jtags-tag-line-regexp bound t)
            (let ((next-member (match-string 3))
                  (next-pos (match-string 7)))
              (when (string-equal pos next-pos)
                ;; (jtags-message "Found members `%s' and `%s' at same pos, ignoring `%s'" member next-member member)
                next-member)))))))

(defun jtags-get-tagged-members (class bound)
  "Return a list of all unique members declared in CLASS.
The search is bounded by BOUND, which should be the end of the class.

This function also sets some text properties on the class members.
A member can have one or several of the following text properties:
final, private, protected, public, and static."
  (let ((member-list nil))

    (while (re-search-forward jtags-tag-line-regexp bound t)
      (let ((member (match-string 3))
            (pos (match-string 7)))
        ;; (jtags-message "Found member `%s' at pos %s" member pos)

        (unless (or (jtags-next-member-same-pos-p member pos bound)
                    (member member member-list)
                    (equal member "static")) ;; Ignore static class initializer

          ;; Set text properties to keep track of the member's Java properties
          (goto-char (match-beginning 3))
          (save-excursion
            (if (re-search-backward "[ \t\n]+final[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(final t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+static[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(static t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+private[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(private t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+protected[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(protected t) member)))
          (save-excursion
            (if (re-search-backward "[ \t\n]+public[ \t\n]+" (point-at-bol) t)
                (add-text-properties 0 1 '(public t) member)))

          (setq member-list (cons member member-list)))))
    member-list))

(defun jtags-get-tagged-file (class-pos)
  "Return the name of the file in which the class at CLASS-POS is declared.
Unfortunately, the function `file-of-tag' sometimes fails in XEmacs."
  (save-excursion
    (goto-char class-pos)
    (re-search-backward "" nil t)
    (forward-line 1)
    (let ((start-pos (point)))
      (re-search-forward "\\(\\\.java\\)," nil t)
      (let ((end-pos (match-end 1)))
	(buffer-substring start-pos end-pos)))))

(defun jtags-right-package-p (file-name package-list)
  "Return non-nil if the package in FILE-NAME matches one in PACKAGE-LIST.

FILE-NAME is the absolute file name of the class/package to check.
PACKAGE-LIST is a list of package names, e.g. \"java.io.*\", or fully qualified
class names, e.g. \"java.io.InputStream\". Package names must end with the wild
card character \"*\"."
  (if (null package-list)
      't
    (block while-package-list
      ;; (jtags-message "Comparing package in file `%s'" file-name)
      (while package-list
        (let ((package (car package-list)))

          ;; Replace * with [A-Za-z0-9_]+ in package
          (setq package (replace-regexp-in-string "\\*" "[A-Za-z0-9_]+" package))
          (setq package (concat package "\\\.java$"))
          ;; (jtags-message "Comparing to package `%s'" package)

          ;; If packages match, (return-from while-package-list 't)
          (if (string-match package file-name)
              (return-from while-package-list 't)))
        (setq package-list (cdr package-list))))))

(defun jtags-find-classes (file)
  "Find all classes defined in FILE and return a list of (class . line) pairs."
  (save-excursion
    (setq file (replace-regexp-in-string "\\\\" "/" file))
    (jtags-message "Finding classes in file `%s'" file)
    (let* ((case-fold-search nil)
           (tags-list (jtags-buffer-tag-table-list))
           bound)
      (block while-tags-list
        (while tags-list
          ;; (jtags-message "Looking in tags table file `%s'" (car tags-list))
          (jtags-visit-tags-table-buffer (car tags-list))
          (goto-char (point-min))

          ;; look for file name (case insensitive search)
          (when (let ((case-fold-search t)) (search-forward file nil t))
            (save-excursion
              (setq bound (search-forward "" nil t)))
            ;; (jtags-message "Search limits = [%s, %s]" (point) bound)
            (return-from while-tags-list (jtags-do-find-classes bound)))

          ;; Look in the next tags file
          (setq tags-list (cdr tags-list)))))))

(defun jtags-do-find-classes (&optional bound)
  "Find class definitions in tags table file, starting from point.
The optional argument BOUND bounds the search; it is a buffer position."
  (let (result)
    ;; Find class name
    (while (re-search-forward "\\(class\\|enum\\|interface\\)[ \t\n]+\\([A-Za-z0-9_]+\\)" bound t)

      ;; If we found a match, save class name and find line of declaration
      (let ((class (match-string 2))
            line)
        (jtags-message "Found class `%s' at pos %s in tags file" class (point))
        (re-search-forward "\\([0-9]+\\),[0-9]+" bound t)
        (setq line (string-to-number (match-string 1)))
        ;; (jtags-message "Class `%s' starts on line %d in source code" class line)
        (setq result (cons (cons class line) result))))
    result))

;; ----------------------------------------------------------------------------
;; Functions that update tags table files:
;; ----------------------------------------------------------------------------

(defun jtags-update-tags-files ()
  "Update all tags table files listed in `tags-table-list'.
An element that is a directory means the file \"TAGS\" in that directory.

Run the etags program and update all tags table files in `tags-table-list'
after first querying the user about each file."
  (interactive)
  (let ((tags-list (jtags-buffer-tag-table-list)))
    (set-buffer (get-buffer-create jtags-buffer-name))
    (map-y-or-n-p "Update %s? " 'jtags-update-tags-file tags-list)))

(defun jtags-update-this-tags-file ()
  "Update the tags table file in which this class is tagged.

Use `tags-table-list' to find the tags table file in which the class
in the current buffer is tagged. Update the tags table file using the
etags program."
  (interactive)
  (let ((norm-default-dir (jtags-file-name-directory default-directory)))
    (jtags-message "Normalized default directory=`%s'" norm-default-dir)
    (dolist (tags-file (jtags-buffer-tag-table-list))
      (let* ((norm-tags-dir (jtags-file-name-directory tags-file)))
        (jtags-message "Checking tags file `%s'" norm-tags-dir)

        ;; If we have found the right tags file, update it
        (when (string-match norm-tags-dir norm-default-dir)
          (jtags-update-tags-file tags-file)
          (return))))))

(defun jtags-update-tags-file (tags-file)
  "Update the tags table file specified by TAGS-FILE.
An argument that is a directory means the file \"TAGS\" in that directory."

  ;; Default file and directory name
  (let ((file-name "TAGS")
        (dir-name tags-file))

    ;; If tags-file is a file, get file and directory name
    (if (file-directory-p dir-name)
        (setq dir-name (file-name-as-directory dir-name))
      (setq file-name (file-name-nondirectory tags-file))
      (setq dir-name (file-name-directory tags-file)))

    (jtags-run-etags file-name dir-name)))

(defun jtags-run-etags (file-name dir-name)
  "Run shell command in `jtags-etags-command' to update a tags table file.
Replace the sequence %f with FILE-NAME in `jtags-etags-command', and run the
resulting shell command in directory DIR-NAME. This normally includes running
the etags program, so the Emacs \"bin\" directory must be in your path."
  (let ((command (replace-regexp-in-string "%f" file-name jtags-etags-command))
        (original-directory default-directory))
    (jtags-message "Shell command=%s" command)
    (cd dir-name)

    ;; Don't try to update if file is not writable
    (if (not (file-writable-p file-name))
        (error "Cannot update tags file: permission denied, %s" dir-name)
      (message "Updating tags file in %s..." dir-name)
      (shell-command command jtags-buffer-name)
      (message "Updating tags file in %s...done" dir-name))

    (cd original-directory)))

;; ----------------------------------------------------------------------------
;; Functions that parse Java files:
;; ----------------------------------------------------------------------------

(defun jtags-find-identifier-backward ()
  "Return identifier before point in an expression.
Search backward for an identifier in the expression around point, set point to
the first character in the identifier, and return the identifier. If a constant
string, e.g. \"foo\" is found, the string \"#stringliteral#\" is returned, and
the point is set to the end of the string. Return nil if no identifier can be
found."
  (let ((end (point)))
    (jtags-message "Before='%c', after='%c'" (char-before) (char-after))
    (cond ((looking-back "[A-Za-z0-9_]")                ; identifier?
           (skip-chars-backward "A-Za-z0-9_")
           (buffer-substring-no-properties (point) end))

          ((looking-back "[\"][ \t\n]*")                ; string?
           (skip-chars-backward "\" \t\n")
           (skip-chars-backward "^\"")
           (skip-chars-backward "\"")
           "#stringliteral#")

          ((looking-back "[])][ \t\n]*")                ; parenthesis?
           (skip-chars-backward " \t\n")
           (goto-char (scan-sexps (point) -1))
           (skip-chars-backward " \t\n")
           (jtags-find-identifier-backward))

          ((looking-back "[.][ \t\n]*")                 ; dot?
           (skip-chars-backward ". \t\n")
           (jtags-find-identifier-backward)))))

(defun jtags-parse-java-line ()
  "Parse expression around point, and return a list of identifiers.
The list starts with the first identifier in the expression, and
ends with the identifier we want to lookup. Example:

toString().substring(1).length() -> (\"toString\" \"substring\" \"length\")

The identifiers returned are names of attributes or methods, and the
first one can actually be a class name. Example:

Class.forName(\"Foo\") -> (\"Class\" \"forName\")"
  (save-excursion
    ;; If point is in identifier, goto end of identifier
    (skip-chars-forward "[A-Za-z0-9_]")
    (let ((result nil)
          (parse-in-literal (c-in-literal))
          (identifier (jtags-find-identifier-backward)))
      (while identifier
        ;; If in comment now, but not from the beginning, stop searching
        (if (and (c-in-literal) (not parse-in-literal))
            (setq identifier nil)
          (jtags-message "Adding identifier '%s'" identifier)
          (setq result (cons identifier result))
          (setq identifier (jtags-find-identifier-backward))))
      (jtags-message "Result=%S" result)
      result)))

(defun jtags-beginning-of-method (&optional bound)
  "Move backward to the beginning of the current method.
Return non-nil unless search stops due to beginning of buffer. The
search is bounded by BOUND, which should be the start of the class."
  (interactive)
  (let (start-pos     ;;                 RETURN TYPE           METHOD NAME               ARGS
        (start-regexp (concat "[ \t\n]+" jtags-type-regexp "\\([A-Za-z0-9_]+\\)[ \t\n]*\([^)]*\)")))
    (save-excursion
      (jtags-save-buffer-state ()

        ;; Make sure we are not in the middle of a method declaration
        (c-end-of-statement)

        ;; Search backward for method declaration
        (while (and (null start-pos) (re-search-backward start-regexp bound t))
          (jtags-message "Regexp match `%s'" (match-string 0))
          (let ((return-type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                (method-name (buffer-substring-no-properties (match-beginning 5) (match-end 5))))
            (jtags-message "Found method `%s' with return type `%s'" method-name return-type)

            ;; Ignore invalid method names, return types, and declarations in comments and strings
            (goto-char (match-beginning 2))
            (unless (or (string-match "^for$" method-name)
                        (string-match "^new$\\|^return$\\|^throw$" return-type)
                        (c-in-literal))

              ;; Go to the beginning of the method declaration
              (c-end-of-statement)
              (c-beginning-of-statement-1)
              (setq start-pos (point)))))))

    (if (and start-pos (< start-pos (point)))
        (goto-char start-pos))))

(defun jtags-find-declaration (var class-start-line)
  "Look backwards for a declaration of VAR, and return its declaration info.
The search is bounded by CLASS-START-LINE. If VAR is found, return a list
with the following elements:

'(type        ; The type of VAR
  line        ; The line number where the declaration starts
  pos)        ; The position where the declaration starts"
  (save-excursion
    (jtags-message "Finding declaration of identifier `%s', class starts at %s" var class-start-line)
    (let* ((class-start-pos (jtags-line-to-point class-start-line))
           (method-start-pos (save-excursion (jtags-beginning-of-method class-start-pos)))
           (decl-regexp (concat "[ \t\n\(]"
                                jtags-type-regexp
                                ;;  IDENT BEFORE      ARRAY OR SPACE
                                "\\([A-Za-z0-9_]+\\(\\[\\]\\|[ \t\n]\\)*,[ \t\n]*\\)*"
                                ;; SEARCH IDENT
                                var "[ \t\n]*[,=:;[\)]"))
           result)
      (jtags-message "Class starts at `%s', method starts at `%s'" class-start-pos method-start-pos)

      ;; If point is in identifier, goto end of identifier
      (skip-chars-forward "[A-Za-z0-9_]")

      ;; Find declaration (if point is in a method)
      (while (and method-start-pos
                  (null result)
                  (re-search-backward decl-regexp method-start-pos t))
        (let ((type (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
              (line (jtags-get-line))
              (pos (match-beginning 2)))
          (jtags-message "Found type `%s' for `%s'" type var)

          ;; Ignore invalid types, and declarations in comments and strings
          (unless (or (string-match "^\\(instanceof\\|new\\|return\\|static\\|throws?\\)$" type)
                      (c-in-literal))

            ;; We found a valid declaration
            (jtags-message "Found declaration on line %s" line)
            (setq result (list type line pos)))))

      ;; (jtags-message "Type-line-pos=%S" result)
      result)))

(defun jtags-find-package ()
  "Parse current buffer for package, and return package name."
  (save-excursion
    (goto-char (point-min))
    (let (package-name)
      (while (and (not package-name)
                  (re-search-forward "^package[ \t\n]+\\([^;]+\\);" nil t))
        (let ((beginning (match-beginning 1))
              (end (match-end 1)))
          (jtags-message "Regexp match=`%s'" (buffer-substring-no-properties beginning end))
          (goto-char beginning)
          ;; Ignore statement in comments and strings
          (unless (c-in-literal)
            (setq package-name (buffer-substring-no-properties beginning end))
            (setq package-name (replace-regexp-in-string "[ \t\n]" "" package-name)))))
      package-name)))

(defun jtags-find-imports (&optional current-package)
  "Find import statements, and return a list of package names.
The optional argument CURRENT-PACKAGE is the package of the file in the
current buffer. The package names may, or may not end with \".*\"."
  (save-excursion
    (let (list)

      ;; Start the list with the current package and package "java.lang.*",
      ;; since they are imported by default
      (if (not current-package)
          (setq current-package (jtags-find-package)))
      (if current-package
          (setq list (list (concat current-package ".*"))))
      (setq list (cons "java.lang.*" list))

      (goto-char (point-min))
      (while (re-search-forward "^import[ \t\n]+\\\([^;]+\\\);" nil t)
        (let ((beginning (match-beginning 1))
              (end (match-end 1)))
          (jtags-message "Regexp match=`%s'" (buffer-substring-no-properties beginning end))
          (goto-char beginning)
          ;; Ignore statement in comments and strings
          (unless (c-in-literal)
            (let* ((package-name (buffer-substring-no-properties beginning end))
                   (clean-name (replace-regexp-in-string "[ \t\n]" "" package-name)))
              (jtags-message "Package=`%s', cleaned=`%s'" package-name clean-name)
              (setq list (cons clean-name list))))))
      (jtags-uniqify-list (nreverse list)))))

;; ----------------------------------------------------------------------------
;; Functions for showing Javadoc:
;; ----------------------------------------------------------------------------

(defun jtags-find-javadoc (class package javadoc-root-list)
  "This method has been deprecated, and will be removed."
  (message "Variable jtags-javadoc-root-list has been deprecated, use jtags-javadoc-root-alist instead.")
  (dolist (javadoc-root javadoc-root-list)
    (setq javadoc-root (jtags-file-name-directory javadoc-root))
    (jtags-message "Normalized javadoc-root: `%s'" javadoc-root)

    (let ((javadoc-file (concat javadoc-root package "/" class ".html")))
      (jtags-message "Looking for Javadoc file `%s'" javadoc-file)
      (if (file-regular-p javadoc-file)
          (return javadoc-file)))))

(defun jtags-browse-url-old (package-name class-name)
  "This method has been deprecated, and will be removed."
  (setq package-name (replace-regexp-in-string "\\\." "/" package-name))
  (let ((javadoc-file (jtags-find-javadoc class-name package-name jtags-javadoc-root-list)))
    (if (not javadoc-file)
        (message "Documentation not found!")
      (message "Displaying URL %s" javadoc-file)
      (funcall jtags-browse-url-function javadoc-file))))

(defun jtags-browse-url (definition)
  "Show Javadoc for the class in DEFINITION in the configured browser.
Use variable `jtags-javadoc-root-alist' to find the Javadoc URL for the
class, and display it in the configured browser."
  (jtags-message "Definition=%S" definition)
  (let ((package-name (jtags-definition-package definition))
        (class-name (jtags-definition-class definition)))
    (if (null package-name)
        (setq package-name ""))
    (jtags-message "Looking for Javadoc for class `%s' in package `%s'" class-name package-name)
    (if jtags-javadoc-root-list
        (jtags-browse-url-old package-name class-name)

    ;; Find matching javadoc root
    (let ((javadoc-root (assoc-default package-name jtags-javadoc-root-alist 'string-match)))
      (if (not javadoc-root)
          (message "Documentation not found!")
        (jtags-message "Found javadoc-root `%s'" javadoc-root)
        (setq package-name (replace-regexp-in-string "\\\." "/" package-name))
        (if (not (string-match "/$" javadoc-root))
            (setq javadoc-root (concat javadoc-root "/")))
        (let ((javadoc-file (concat javadoc-root package-name "/" class-name ".html")))
          (message "Displaying URL %s" javadoc-file)
          (funcall jtags-browse-url-function javadoc-file)))))))

;; ----------------------------------------------------------------------------
;; Initialization:
;; ----------------------------------------------------------------------------

(defvar jtags-mode-map nil
  "Keymap used when jtags mode is enabled.")

(unless jtags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?\,)]   'jtags-member-completion)
    (define-key map [(meta ?\,)]      'jtags-show-declaration)
    (define-key map [(meta f1)]       'jtags-show-documentation)
    (define-key map [(control c) ?\,] 'jtags-update-this-tags-file)
    (setq jtags-mode-map map)))

(defvar jtags-menu-list
  (list "JTags"
        ["Member completion" jtags-member-completion t]
        ["Show declaration" jtags-show-declaration t]
        ["Show documentation" jtags-show-documentation t]
        "--"
        ["Update this tags file" jtags-update-this-tags-file t]
        ["Update all tags files" jtags-update-tags-files t])
  "JTags submenu definition.")

;; Define a menu, and put it in the `jtags-mode-map'.
(easy-menu-define jtags-menu jtags-mode-map
  "Provides menu items for accessing jtags functionality."
  jtags-menu-list)

;;;###autoload (add-hook 'java-mode-hook 'jtags-mode)

;;;###autoload
(define-minor-mode jtags-mode
  "Toggle jtags mode.
With arg, turn jtags mode on if arg is positive.

When jtags mode is enabled, a number of improved tags lookup commands are
available, as shown below. jtags mode provides commands for looking up the
identifier before or around point, completing partly typed identifiers, and
managing tags table files.

\\{jtags-mode-map}"
  nil
  nil
  jtags-mode-map
  (if jtags-mode
      (if jtags-display-menu-flag
          (easy-menu-add jtags-menu-list jtags-mode-map))
    (if jtags-display-menu-flag
        (easy-menu-remove jtags-menu-list))))

;; ----------------------------------------------------------------------------

(provide 'jtags)

;;; jtags.el ends here
