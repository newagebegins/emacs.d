(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(helm
                    markdown-mode))

;; Fetch the list of available packages.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Disable backups and auto-saves.
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq-default fill-column 80)
(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode)
(setq default-input-method "russian-computer")
;;(global-set-key (kbd "M-l") 'toggle-input-method)
(setq-default indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
;;(setq-default truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode)
(savehist-mode 1)

(recentf-mode 1)
(setq recentf-max-saved-items 500)

;; Smooth scrolling.
(setq scroll-step 3)

(setq-default c-basic-offset 4)
(setq c-default-style "stroustrup")
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

(require 'helm-config)
(helm-mode 1)
;; Better window splitting (prevents too small windows when, for example, re-builder is open)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
