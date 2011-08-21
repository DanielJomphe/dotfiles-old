(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; -------------------------------------------------------------------------
;;; Loads
;;; -------------------------------------------------------------------------

 ; why isn't elpa's color-theme available by now!?
(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-6.6.1")
(add-to-list 'load-path "~/dev/solarized/emacs-colors-solarized")
(add-to-list 'load-path "~/.emacs.d/lib")

;;; -------------------------------------------------------------------------
;;; Look & Feel
;;; -------------------------------------------------------------------------

(set-face-attribute 'default nil :family "menlo")

(defun dj-init ()
  (load "color-theme-solarized")
  (color-theme-solarized-dark)

  (setq-default cursor-type 'bar)
  (set-cursor-color "#ff0000"))         ;(dj-init)

(defun dj-init-fullscreen ()
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (split-window-horizontally -80)
  (split-window-horizontally 80)
  (split-window-vertically))            ;(dj-init-fullscreen)

(setq initial-scratch-message nil)
(setq initial-buffer-choice "~/.emacs.d/manual.el")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;; -------------------------------------------------------------------------
;;; Hooks
;;; -------------------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
(add-hook 'clojure-mode-hook 'highlight-symbol-mode)

(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'highlight-parentheses-mode)
(add-hook 'slime-repl-mode-hook 'highlight-symbol-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(defun clojure-slime-maybe-compile-and-load-file ()
  (when (and (eq major-mode 'clojure-mode)
             (slime-connected-p))
    (slime-compile-and-load-file)))

(add-hook 'after-save-hook 'clojure-slime-maybe-compile-and-load-file)
