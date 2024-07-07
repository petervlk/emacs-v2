;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/.config/emacs/crafted-emacs/modules/crafted-init-config")

(setq package-pinned-packages
      '((lsp-mode . "melpa")
        (projectile . "melpa-stable")))

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(add-to-list 'package-selected-packages 'no-littering)
(add-to-list 'package-selected-packages 'direnv)
(add-to-list 'package-selected-packages 'hydra)
(add-to-list 'package-selected-packages 'magit)
(add-to-list 'package-selected-packages 'git-timemachine)
(add-to-list 'package-selected-packages 'projectile)
(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-lisp-packages)
(require 'crafted-ide-packages)

(add-to-list 'package-selected-packages 'rainbow-delimiters)

(add-to-list 'package-selected-packages 'lsp-mode)
(add-to-list 'package-selected-packages 'consult-lsp)

(add-to-list 'package-selected-packages 'lsp-java)
(add-to-list 'package-selected-packages 'groovy-mode)

(require 'crafted-ui-packages)
(require 'crafted-writing-packages)
(add-to-list 'package-selected-packages 'minions)

;; structural editing - packages
(add-to-list 'package-selected-packages 'smartparens)
(add-to-list 'package-selected-packages 'evil-smartparens)

;; Install selected packages
(package-install-selected-packages :noconfirm)

;;use direnv
(direnv-mode)

;; Load configuration for the completion module
(require 'crafted-defaults-config)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(setq use-short-answers t)

;; Move autosaves to 'auto-save/' directory
(custom-set-variables
 '(auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
 '(backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
 )

;; show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(require 'crafted-completion-config)
(global-corfu-mode 1)
(custom-set-variables
 '(tab-always-indent 'complete)
 '(completion-cycle-threshold nil))

;; search in hidden files. using setq to keep original value intact
(setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))

;; evil navigate minibuffer
(with-eval-after-load 'evil
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (keymap-set vertico-map "M-h" #'vertico-directory-up))

(require 'crafted-evil-config)
(require 'crafted-lisp-config)

;; structural editing - config
(require 'smartparens-config)

(custom-set-variables
 '(show-smartparens-global-mode t)
 '(sp-navigate-interactive-always-progress-point t))

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

(defhydra smartparens-hydra (:hint nil)
  "
  _w_: next        _>_: barf     _(_: wrap round             _u_: splice     _d_: kill    _j_: down sexp    _q_: quit
  _W_: next lvl    _<_: slurp    _[_: wrap square            _r_: raise      _y_: copy    _k_: up sexp      ^ ^
  _b_: prev        ^ ^           _{_: wrap curly             ^ ^             _Y_: copy    ^ ^               ^ ^
  _B_: prev lvl    ^ ^           _\"_: wrap double quotes
  "
  (">" sp-forward-barf-sexp)
  ("<" sp-forward-slurp-sexp)

  ("j" sp-down-sexp)
  ("k" sp-backward-up-sexp)
  ("w" sp-next-sexp)
  ("W" sp-beginning-of-next-sexp)
  ("b" sp-previous-sexp)
  ("B" sp-beginning-of-previous-sexp)
  ;; ("e" sp-end-of-next-sexp "End of Next sexp")

  ("(" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")) :color blue)
  ("[" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")) :color blue)
  ("{" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")) :color blue)
  ("\"" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")) :color blue)

  ("u" sp-splice-sexp :color blue)
  ("r" sp-raise-sexp :color blue)

  ("y" sp-copy-sexp  :color blue)
  ("Y" sp-backwards-copy-sexp  :color blue)
  ("d" sp-kill-sexp :color blue)
  ("q" nil :color blue))

(define-key smartparens-mode-map (kbd "M-s") 'smartparens-hydra/body)

(require 'crafted-ide-config)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
(defun cw/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(flex))) ;; Configure orderless

(add-hook 'lsp-completion-mode-hook #'cw/lsp-mode-setup-completion)

(custom-set-variables
 '(lsp-completion-provider :none)
 '(lsp-eldoc-enable-hover nil) ;; use CIDER eldoc (this should be turned off only for clojure mode)
 '(lsp-enable-indentation nil) ; uncomment to use cider indentation instead of lsp
 '(lsp-enable-symbol-highlighting t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-lens-enable t) ;;Reference and test counts
 '(lsp-modeline-code-actions-enable nil) ;; Don't clutter modeline
 ;; '(lsp-modeline-diagnostics-enable nil) ;; Don't clutter modeline
 '(lsp-signature-auto-activate nil)
 '(lsp-signature-render-documentation nil))

(custom-set-variables
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-show-with-mouse nil)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-doc-childframe nil))

(with-eval-after-load 'lsp-mode
  (keymap-set evil-normal-state-map "SPC l" lsp-command-map))


;;;; UI
(require 'crafted-ui-config)

(minions-mode t)

(customize-set-variable 'display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Show column position in mode-line
(column-number-mode t)

;; highlight current line
(global-hl-line-mode t)

(defun pv-buffer-name ()
  "Sets the buffer name while taking current project and buffer visiting file into consideration."
  (let ((fname (buffer-file-name))
        (project (project-current)))
    (cond
     ((not fname) (buffer-name))
     ((not project) (abbreviate-file-name fname))
     (t (let ((project-name (file-name-nondirectory
                             (directory-file-name
                              (project-root project))))
              (project-file (file-relative-name fname (project-root project))))
          (file-name-concat project-name project-file))))))

(setq-default mode-line-buffer-identification
              '(:eval (propertize (pv-buffer-name) 'face 'mode-line-buffer-id)))

;; Include entire file path in title
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;;;; WRITING DOCS
(require 'crafted-writing-config)


;;;; Key bindings

;; Set preferred key bindings
(keymap-global-set "C-M-u"      #'universal-argument)

(keymap-global-set "C-M-;"      #'magit-status) ;; DEPRECATED
(keymap-set evil-normal-state-map "SPC g g" #'magit-status)
(keymap-set evil-visual-state-map "SPC g g" #'magit-status)

(keymap-global-set "C-<return>" #'embark-act)

(keymap-global-set "C-x C-b"    #'ibuffer) ;; DEPRECATED
(keymap-set evil-normal-state-map "SPC b i" #'ibuffer)
(keymap-set evil-visual-state-map "SPC b i" #'ibuffer)

(keymap-global-set "C-M-j"   #'consult-buffer) ;; DEPRECATED
(keymap-set evil-normal-state-map "SPC b b" #'consult-buffer)
(keymap-set evil-visual-state-map "SPC b b" #'consult-buffer)
(keymap-set evil-normal-state-map "SPC b n" #'evil-next-buffer)
(keymap-set evil-visual-state-map "SPC b n" #'evil-next-buffer)
(keymap-set evil-normal-state-map "SPC b p" #'evil-prev-buffer)
(keymap-set evil-visual-state-map "SPC b p" #'evil-prev-buffer)

(keymap-global-set "C-x C-r" #'consult-recent-file) ;; DEPRECATED
(keymap-set evil-normal-state-map "SPC f r" #'consult-recent-file)
(keymap-set evil-visual-state-map "SPC f r" #'consult-recent-file)

(keymap-global-set "C-x C-i" #'consult-imenu)
(keymap-global-set "C-x M-i" #'consult-imenu-multi)

(keymap-set evil-normal-state-map "C-p" #'consult-yank-from-kill-ring)
(keymap-set evil-insert-state-map "C-p" #'consult-yank-from-kill-ring)
(keymap-set evil-visual-state-map "C-p" #'consult-yank-from-kill-ring)

;; https://stackoverflow.com/a/11319885 - How do I bind a command to C-i without changing TAB?
;; unmap 'indent-for-tab-command
(keymap-set input-decode-map "C-i" "H-i")
(keymap-set evil-normal-state-map "H-i" #'evil-jump-forward)
(keymap-set evil-insert-state-map "H-i" #'evil-jump-forward)
(keymap-set evil-visual-state-map "H-i" #'evil-jump-forward)

(keymap-set evil-motion-state-map "[ j" #'evil-jump-backward) ;; DEPRECATED
(keymap-set evil-motion-state-map "] j" #'evil-jump-forward)  ;; DEPRECATED

(keymap-set evil-window-map "C-q"   #'evil-quit)
(keymap-set evil-window-map "C-b"   #'bookmark-jump-other-window)
(keymap-set evil-window-map "C-d"   #'dired-other-window)
(keymap-set evil-window-map "C-M-j" #'consult-buffer-other-window)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-up-directory
  "l" 'dired-find-file)

(keymap-set minibuffer-local-map "C-d" #'embark-act)

(global-set-key [remap bookmark-jump] 'consult-bookmark)


;;;; Source Control
(custom-set-variables
 '(magit-diff-refine-hunk t)
 '(magit-no-confirm '(stage-all-changes
                      unstage-all-changes))
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;;; Dired
(keymap-global-unset "C-x C-d")
(keymap-global-set "C-x C-d" 'dired-jump-other-window)

(custom-set-variables
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-kill-when-opening-new-dired-buffer t))


;;;; Project

(require 'projectile)
(projectile-mode +1)

(require 'significant-other)

(keymap-set project-prefix-map "g" #'consult-ripgrep)
(keymap-set project-prefix-map "t" #'significant-other-jump)

;; Make it possible to ignore risky local variables
(advice-add 'risky-local-variable-p :override #'ignore)


;;;; Java

(add-hook 'java-mode-hook 'lsp)


;;;; Clojure

(custom-set-variables
 '(clojure-toplevel-inside-comment-form t) ;; cider-defun-at-point treats contents of comment block as top level functions
 )

(setq clojure-align-forms-automatically t
      clojure-indent-style 'align-arguments)

(add-hook 'clojure-mode-hook #'evil-smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

;; accept kebab-case words
(add-hook 'clojure-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; https://github.com/dzer6/cljfmt-graalvm#integrate-with-emacs
(defun pv-cljfmt-format-buffer ()
  (when (or (eq major-mode 'clojure-mode)
            (eq major-mode 'clojurescript-mode))
    (shell-command-to-string (format "cljfmt fix %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

;; significant other

(require 's)

(defun setup-clojure-mode-significant-other ()
  (with-significant-others file-name
    ("/src/.+\.cljc" (list (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".cljc" "_test.clj"))))
    ("/src/.+\.clj"  (list (s-with file-name
                             (s-replace "/src/" "/test/")
                             (s-replace ".clj" "_test.clj"))))
    ("/test/.+\.clj" (list (s-with file-name
                             (s-replace "/test/" "/src/")
                             (s-replace "_test.clj" ".clj"))
                           (s-with file-name
                             (s-replace "/test/" "/src/")
                             (s-replace "_test.clj" ".cljc"))))))

(dolist (hook '(clojure-mode-hook
                clojurec-mode-hook
                clojurescript-mode-hook))
  (add-hook hook #'setup-clojure-mode-significant-other))

;; Cider
(custom-set-variables
 '(cider-save-file-on-load t) ;; save files when evaluating them
 '(cider-repl-pop-to-buffer-on-connect nil) ;; don't pop up repl when connecting
 )

;; Clj-refactor
(custom-set-variables
 '(cljr-favor-prefix-notation nil)
 '(cljr-favor-private-functions nil)
 '(cljr-insert-newline-after-require nil)
 '(cljr-assume-language-context "clj")
 '(cljr-warn-on-eval nil) ;; disable warning
 )

(setq cljr-clojure-test-declaration "[clojure.test :refer [deftest is testing]]")
(setq cljr-cljs-clojure-test-declaration cljr-clojure-test-declaration)
(setq cljr-cljc-clojure-test-declaration cljr-clojure-test-declaration)

;;;; Optional configuration

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
