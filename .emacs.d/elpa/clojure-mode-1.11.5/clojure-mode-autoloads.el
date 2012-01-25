;;; clojure-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-enable-slime-on-existing-buffers clojure-jack-in
;;;;;;  clojure-mode) "clojure-mode" "../../../../../.emacs.d/elpa/clojure-mode-1.11.5/clojure-mode.el"
;;;;;;  "9c93dc780bfc3694a8fd265302dca49f")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/clojure-mode-1.11.5/clojure-mode.el

(autoload 'clojure-mode "clojure-mode" "\
Major mode for editing Clojure code - similar to Lisp mode.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{clojure-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `clojure-mode-hook'
if that value is non-nil.

\(fn)" t nil)

(autoload 'clojure-jack-in "clojure-mode" "\


\(fn)" t nil)

(autoload 'clojure-enable-slime-on-existing-buffers "clojure-mode" "\


\(fn)" t nil)

(add-hook 'slime-connected-hook 'clojure-enable-slime-on-existing-buffers)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/clojure-mode-1.11.5/clojure-mode-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/clojure-mode-1.11.5/clojure-mode.el")
;;;;;;  (20236 45850 938359))

;;;***

(provide 'clojure-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-mode-autoloads.el ends here
