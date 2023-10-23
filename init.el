;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/.config/emacs/crafted-emacs/modules/crafted-init-config")

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(add-to-list 'package-selected-packages 'no-littering)
(add-to-list 'package-selected-packages 'magit)
(require 'crafted-completion-packages)
(require 'crafted-evil-packages)
(require 'crafted-lisp-packages)
(require 'crafted-ide-packages)
(require 'crafted-ui-packages)
(require 'crafted-writing-packages)
(add-to-list 'package-selected-packages 'minions)

;; Install selected packages
(package-install-selected-packages :noconfirm)

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

(require 'crafted-completion-config)
(require 'crafted-evil-config)
(require 'crafted-lisp-config)
(require 'crafted-ide-config)

;;;; UI
(require 'crafted-ui-config)

(minions-mode t)

(customize-set-variable 'display-fill-column-indicator-column 100)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Show column position in mode-line
(column-number-mode t)

;; highlight current line
(global-hl-line-mode t)


;;;; WRITING DOCS
(require 'crafted-writing-config)


;;;; Source Control
(custom-set-variables '(magit-diff-refine-hunk t)
                      '(magit-no-confirm '(stage-all-changes
                                           unstage-all-changes))
                      '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;;; Optional configuration

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)
