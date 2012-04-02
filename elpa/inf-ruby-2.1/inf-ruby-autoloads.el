;;; inf-ruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (run-ruby inf-ruby inf-ruby-keys) "inf-ruby" "inf-ruby.el"
;;;;;;  (20296 37287))
;;; Generated autoloads from inf-ruby.el

(autoload (quote inf-ruby-keys) "inf-ruby" "\
Set local key defs to invoke inf-ruby from ruby-mode.

\(fn)" nil nil)

(autoload (quote inf-ruby) "inf-ruby" "\
Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use. Runs the
hooks `inf-ruby-mode-hook' (after the `comint-mode-hook' is
run).

\(fn &optional IMPL)" t nil)

(autoload (quote run-ruby) "inf-ruby" "\
Run an inferior Ruby process, input and output via buffer *ruby*.
If there is a process already running in `*ruby*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `ruby-program-name').  Runs the hooks `inferior-ruby-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn &optional COMMAND NAME)" t nil)

(eval-after-load (quote ruby-mode) (quote (add-hook (quote ruby-mode-hook) (quote inf-ruby-keys))))

;;;***

;;;### (autoloads nil nil ("inf-ruby-pkg.el") (20296 37287 843708))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; inf-ruby-autoloads.el ends here
