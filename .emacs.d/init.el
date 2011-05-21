(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; ----------------------------------------------------------------------------
;;; Look & Feel            
;;; ----------------------------------------------------------------------------

;(require 'color-theme-zenburn)
;(color-theme-zenburn)

(setq-default cursor-type 'bar)
(set-cursor-color "#ff0000")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;; ----------------------------------------------------------------------------
;;; Hooks
;;; ----------------------------------------------------------------------------

(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(add-hook 'clojure-mode-hook 'whitespace-mode)

(defun clojure-slime-maybe-compile-and-load-file ()
  (when (and (eq major-mode 'clojure-mode)
             (slime-connected-p))
    (slime-compile-and-load-file)))

(add-hook 'after-save-hook 'clojure-slime-maybe-compile-and-load-file)
