(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; ----------------------------------------------------------------------------
;;; Look & Feel            
;;; ----------------------------------------------------------------------------

;(set-variable 'max-lisp-eval-depth 100000) ; needed this to install the theme
(require 'color-theme-zenburn)
(color-theme-zenburn)

(setq-default cursor-type 'bar)
(set-cursor-color "#ff0000")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;; ----------------------------------------------------------------------------
;;; Hooks
;;; ----------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)

(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'slime-repl-mode-hook 'highlight-symbol-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)

(defun clojure-slime-maybe-compile-and-load-file ()
  (when (and (eq major-mode 'clojure-mode)
             (slime-connected-p))
    (slime-compile-and-load-file)))

(add-hook 'after-save-hook 'clojure-slime-maybe-compile-and-load-file)

;;; ----------------------------------------------------------------------------
;;; Custom
;;; ----------------------------------------------------------------------------

(custom-set-variables
 '(global-hl-line-mode t)
 '(global-whitespace-mode t))

;;; ----------------------------------------------------------------------------
;;; Loads
;;; ----------------------------------------------------------------------------

;(add-to-list 'load-path "~/.emacs.d/lib")

;(load "slamhound")

