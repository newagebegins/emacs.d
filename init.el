(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(markdown-mode
                    scss-mode
                    yaml-mode
                    jade-mode
                    coffee-mode
                    ag
                    wgrep
                    wgrep-ag
                    helm
                    projectile
                    helm-projectile
                    helm-descbinds
                    flycheck
                    sass-mode
                    js2-mode
                    magit))

;; Install missing packages.
(unless (every #'package-installed-p my-packages)
  (package-refresh-contents)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(load-theme 'tango-dark t)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

(setq make-backup-files nil)

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

(setq ring-bell-function 'ignore) ; Disable annoying sounds on Windows.
(show-paren-mode)
;; Need tool-bar-mode to be active for grep and ag to work correctly.
;; With disabled tool-bar-mode they often finish prematurely.
;; (tool-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(windmove-default-keybindings)

;; Better handling of CamelCase.
(setq dabbrev-case-fold-search nil)

;; Smooth scrolling.
(setq scroll-step 3)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(setq mouse-wheel-progressive-speed nil)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 'first-error)
(savehist-mode 1)
(global-subword-mode 1)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(require 'dired-x) ; Enables dired-jump with C-x C-j

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(setq-default fill-column 80)
(setq-default indicate-empty-lines t)
(delete-selection-mode)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

(defun my-c-mode-hook ()
  (setq truncate-lines t)
  (setq c-basic-offset 4)
  (setq c-default-style "awk")
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(winner-mode)
(setq set-mark-command-repeat-pop t)

(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

(setq coffee-tab-width 2)
(setq ruby-insert-encoding-magic-comment nil)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'server)
(unless (server-running-p) (server-start))

(add-to-list 'safe-local-variable-values '(encoding . utf-8))

(defun join-lines () (interactive) (let ((fill-column 999999)) (fill-paragraph nil)))

(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(helm-descbinds-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "<f5>") 'save-buffer)
(global-set-key (kbd "<f6>") 'helm-mini)
(global-set-key (kbd "<f7>") 'helm-find-files)
(global-set-key (kbd "<f8>") 'other-window)
(global-set-key (kbd "<f9>") 'helm-semantic-or-imenu)
(global-set-key (kbd "<f10>") 'recompile)
(global-set-key (kbd "<f11>") 'previous-error)
(global-set-key (kbd "<f12>") 'next-error)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-3") 'kill-whole-line)
(global-set-key (kbd "C-4") 'kill-ring-save)
(global-set-key (kbd "C-5") 'yank)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(auto-save-interval 20)
 '(auto-save-timeout 3)
 '(coffee-indent-like-python-mode t)
 '(helm-autoresize-mode t)
 '(helm-buffer-max-length 40)
 '(helm-inherit-input-method nil)
 '(js2-basic-offset 2)
 '(org-imenu-depth 9))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
