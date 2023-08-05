;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/.config/crafted-emacs/modules/crafted-init-config")

;; Add package definitions for completion packages
;; to `package-selected-packages'.
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
