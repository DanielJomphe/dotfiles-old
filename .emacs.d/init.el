(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; -------------------------------------------------------------------------
;;; Loads
;;; -------------------------------------------------------------------------

 ; why isn't elpa's color-theme available by now!?
(add-to-list 'load-path "~/.emacs.d/elpa/color-theme-6.6.1")
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path              "~/dev/solarized/emacs-colors-solarized")
(add-to-list 'custom-theme-load-path "~/dev/solarized/emacs-colors-solarized/")
;(add-to-list 'custom-theme-load-path (concat prelude-dir "themes/"))

;;; -------------------------------------------------------------------------
;;; Look & Feel
;;; -------------------------------------------------------------------------

(set-face-attribute 'default nil :family "menlo")
(set-face-attribute 'default nil :height 180)

;(add-to-list 'initial-frame-alist `(fullscreen . fullheight))
;(add-to-list 'default-frame-alist `(fullscreen . fullheight))
;(add-to-list 'default-frame-alist '(left . 0))
;(add-to-list 'default-frame-alist '(top . 0))
;(add-to-list 'default-frame-alist '(height . 50))
;(add-to-list 'default-frame-alist '(width . 155))

;; custom Emacs 24 color themes support
;(load-theme 'solarized t)

(defun dj-init ()
  (load "color-theme-solarized")
  (color-theme-solarized-dark)

  (setq-default cursor-type 'bar)
  (set-cursor-color "#ff0000")
  (ns-toggle-fullscreen))

(defun dj-init-windows ()
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (split-window-horizontally -80)
  (split-window-horizontally 80)
  (split-window-vertically))

(setq initial-scratch-message nil)
(setq initial-buffer-choice "~/.emacs.d/manual.el")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key [f5] 'call-last-kbd-macro) ;or in fact keep hitting e
                                        ;in C-x e
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
;(global-set-key [(hyper h)] 'help-command) ;no need, F1 is already bound

(defalias 'qrr 'query-replace-regexp)

;;; -------------------------------------------------------------------------
;;; Global modes
;;; -------------------------------------------------------------------------

(global-whitespace-mode)
(global-hl-sexp-mode)

;;; -------------------------------------------------------------------------
;;; Hooks
;;; -------------------------------------------------------------------------

;;; prog-mode-hook is defined in Emacs Starter Kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(defun clojure-slime-maybe-compile-and-load-file ()
  (when (and (eq major-mode 'clojure-mode)
             (slime-connected-p))
    (slime-compile-and-load-file)))

(add-hook 'after-save-hook 'clojure-slime-maybe-compile-and-load-file)
