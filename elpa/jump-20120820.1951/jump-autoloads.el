;;; jump-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pluralize-string singularize-string) "inflections"
;;;;;;  "inflections.el" (20587 38466))
;;; Generated autoloads from inflections.el

(autoload 'singularize-string "inflections" "\


\(fn STR)" nil nil)

(autoload 'pluralize-string "inflections" "\


\(fn STR)" nil nil)

;;;***

;;;### (autoloads (defjump) "jump" "jump.el" (20587 38466))
;;; Generated autoloads from jump.el

(autoload 'defjump "jump" "\
Define NAME as a function with behavior determined by SPECS.
SPECS should be a list of cons cells of the form

   (jump-from-spec . jump-to-spec)

NAME will then try subsequent jump-from-specs until one succeeds,
at which point any resulting match information, along with the
related jump-to-spec will be used to jump to the intended buffer.
See `jump-to' and `jump-from' for information on spec
construction.

ROOT should specify the root of the project in which all jumps
take place, it can be either a string directory path, or a
function returning

Optional argument DOC specifies the documentation of the
resulting function.

Optional argument MAKE can be used to specify that missing files
should be created.  If MAKE is a function then it will be called
with the file path as it's only argument.  After possibly calling
MAKE `find-file' will be used to open the path.

Optional argument METHOD-COMMAND overrides the function used to
find the current method which defaults to `which-function'.

\(fn NAME SPECS ROOT &optional DOC MAKE METHOD-COMMAND)" nil (quote macro))

;;;***

;;;### (autoloads (which-function-mode) "which-func" "which-func.el"
;;;;;;  (20587 38466))
;;; Generated autoloads from which-func.el
 (put 'which-func-format 'risky-local-variable t)
 (put 'which-func-current 'risky-local-variable t)

(defalias 'which-func-mode 'which-function-mode)

(defvar which-function-mode nil "\
Non-nil if Which-Function mode is enabled.
See the command `which-function-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `which-function-mode'.")

(custom-autoload 'which-function-mode "which-func" nil)

(autoload 'which-function-mode "which-func" "\
Toggle Which Function mode, globally.
When Which Function mode is enabled, the current function name is
continuously displayed in the mode line, in certain major modes.

With prefix ARG, turn Which Function mode on if arg is positive,
and off otherwise.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("findr.el" "jump-pkg.el") (20587 38466
;;;;;;  725183))

;;;***

(provide 'jump-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jump-autoloads.el ends here
