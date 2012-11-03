;;; jtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (jtags-mode) "jtags" "jtags.el" (20587 37071))
;;; Generated autoloads from jtags.el
 (add-hook 'java-mode-hook 'jtags-mode)

(autoload 'jtags-mode "jtags" "\
Toggle jtags mode.
With arg, turn jtags mode on if arg is positive.

When jtags mode is enabled, a number of improved tags lookup commands are
available, as shown below. jtags mode provides commands for looking up the
identifier before or around point, completing partly typed identifiers, and
managing tags table files.

\\{jtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("jtags-pkg.el") (20587 37072 11293))

;;;***

(provide 'jtags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jtags-autoloads.el ends here
