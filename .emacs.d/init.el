(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; -------------------------------------------------------------------------
;;; Auto-install my packages when needed!
;;; -------------------------------------------------------------------------

(if (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(starter-kit
    starter-kit-bindings
    starter-kit-eshell
    starter-kit-js
    starter-kit-lisp
    clojure-mode
   ;clojure-project-mode
    clojure-test-mode
    clojurescript-mode
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized
   ;elein
    hl-sexp
   ;magit-simple-keys
    markdown-mode
    org
   ;project-mode
    rainbow-delimiters
    undo-tree
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; The following packages are provided by some starter kit modules:
;;; * elisp-slime-nav
;;; * find-file-in-project
;;; * idle-highlight-mode
;;; * ido-ubiquitous
;;; * magit
;;; * paredit
;;; * smex

;;; -------------------------------------------------------------------------
;;; Loads
;;; -------------------------------------------------------------------------

;(add-to-list 'load-path "~/.emacs.d/lib") ; nothing in that folder for now.

;;; -------------------------------------------------------------------------
;;; Look & Feel
;;; -------------------------------------------------------------------------

;; TODO conditionalize the following
(set-face-attribute 'default nil :family "menlo")
(set-face-attribute 'default nil :height 150)

;; TODO conditionalize the following
;(add-to-list 'initial-frame-alist `(fullscreen . fullheight))
;(add-to-list 'default-frame-alist `(fullscreen . fullheight))
;(add-to-list 'default-frame-alist '(left . 0))
;(add-to-list 'default-frame-alist '(top . 0))
;(add-to-list 'default-frame-alist '(height . 47))
;(add-to-list 'default-frame-alist '(width . 155))

(color-theme-sanityinc-solarized-light)
;(color-theme-sanityinc-solarized-dark)
;(color-theme-sanityinc-tomorrow-day)
;(color-theme-sanityinc-tomorrow-eighties)

;; TODO conditionalize the following
(ns-toggle-fullscreen)

(split-window-horizontally)

(server-start)

(defun dj-init-manual ()
  (interactive)
  (setq-default cursor-type 'bar)
  (set-cursor-color "#ff0000"))

(defun dj-init-windows ()               ; no more used nowadays...
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (split-window-horizontally -80)
  (split-window-horizontally  80)
  (split-window-vertically))

(setq initial-scratch-message nil)
;(setq initial-buffer-choice "~/.emacs.d/manual.el")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(global-set-key "\C-h" "'backward-delete-char-untabify")
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

(global-set-key (kbd "C-c C-j") 'clojure-jack-in)

;;; -------------------------------------------------------------------------
;;; Global modes
;;; -------------------------------------------------------------------------

(global-whitespace-mode)

(global-hl-sexp-mode)

(define-globalized-minor-mode global-rainbow-delimiters-mode
  rainbow-delimiters-mode
  (lambda ()
    (rainbow-delimiters-mode t)))
(global-rainbow-delimiters-mode t)

;;; -------------------------------------------------------------------------
;;; Local modes & hooks
;;; -------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.dtm$"  . clojure-mode))

;;; prog-mode-hook is defined in Emacs Starter Kit
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode nil)
            (clojure-mode-font-lock-setup)
            (font-lock-mode t)))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; -------------------------------------------------------------------------
;;; Clojure Utilities - Durendal 0.2 didn't work; let's verbatim what works.
;;; In fact, not much here seems to work. :/
;;; -------------------------------------------------------------------------

;;; AUTO-COMPILE ON SAVE
(defvar durendal-auto-compile? nil      ; `t` if I want to put it back on
  "Automatically compile on save when applicable.")

(defun durendal-in-current-project? (file)
  (let ((root (expand-file-name
               (read (cadr (slime-eval
                            '(swank:eval-and-grab-output
                              "(System/getProperty \"user.dir\")")))))))
    (string= (substring file 0 (length root)) root)))

(defun durendal-auto-compile ()
  (when (and slime-mode durendal-auto-compile?
             (slime-connected-p) (slime-current-package)
             (durendal-in-current-project? buffer-file-name))
    (slime-compile-and-load-file)))

(defun durendal-enable-auto-compile ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'durendal-auto-compile))

;;; FONT FACES
(defun durendal-slime-repl-paredit ()
  (require 'paredit)
  (define-key slime-repl-mode-map
    (kbd "DEL") 'paredit-backward-delete)
  (define-key slime-repl-mode-map
    (kbd "{") 'paredit-open-curly)
  (define-key slime-repl-mode-map
    (kbd "}") 'paredit-close-curly)
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "'   ")
  (modify-syntax-entry ?, "    ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'")
  (paredit-mode t))

(defadvice slime-repl-emit (after durendal-slime-repl-emit-ad)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

(defadvice slime-repl-insert-prompt (after durendal-slime-repl-prompt-ad)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))

(defun durendal-enable-slime-repl-font-lock ()
  (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
  (ad-activate #'slime-repl-emit)
  (ad-activate #'slime-repl-insert-prompt))

(defun durendal-disable-slime-repl-font-lock ()
  (remove-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
  (ad-deactivate #'slime-repl-emit)
  (ad-deactivate #'slime-repl-insert-prompt))

(defun durendal-enable ()
  "Enable hooks for all durendal functionality."
  (add-hook 'slime-connected-hook
            (lambda ()
              (if (equal (slime-lisp-implementation-name) "clojure")
                  (progn
                    (add-hook 'clojure-mode-hook
                              'durendal-enable-auto-compile)
                    (add-hook 'slime-repl-mode-hook
                              'durendal-slime-repl-paredit)
                    (durendal-enable-slime-repl-font-lock))
                  (progn
                    (remove-hook 'clojure-mode-hook
                                 'durendal-enable-auto-compile)
                    (remove-hook 'slime-repl-mode-hook
                                 'durendal-slime-repl-paredit)
                    (durendal-disable-slime-repl-font-lock))))))

(durendal-enable)

;;; -------------------------------------------------------------------------
;;; Utilities
;;; -------------------------------------------------------------------------

;; thanks johnw: https://gist.github.com/1198329
(defun find-grep-in-project (command-args)
  (interactive
   (progn
     (list (read-shell-command "Run find (like this): "
                               '("git ls-files -z | xargs -0 egrep -nH -e " . 41)
                               'grep-find-history))))
  (when command-args
    (let ((null-device nil)) ; see grep
      (grep command-args))))
