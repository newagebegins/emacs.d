(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(markdown-mode
                    php-mode
                    scss-mode
                    web-mode
                    yaml-mode
                    jade-mode
                    coffee-mode
                    helm
                    projectile
                    helm-projectile
                    helm-descbinds
                    solarized-theme))

;; Install missing packages.
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)

;; Tweak the font.
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

;; Disable backups and auto-saves.
(setq make-backup-files nil)
(setq auto-save-default nil)

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

(setq ring-bell-function 'ignore) ; Disable annoying sounds on Windows.
(show-paren-mode)
(menu-bar-mode -1)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Smooth scrolling.
(setq scroll-step 3)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(setq mouse-wheel-progressive-speed nil)

;; Use Github Flavored Markdown mode for markdown files.
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(defalias 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 'first-error)
(savehist-mode 1)

(require 'dired-x) ; Enables dired-jump with C-x C-j

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.\\(test\\|module\\|inc\\|install\\|profile\\)$" . php-mode))
(defun my-php-mode-hook ()
  (setq truncate-lines t)
  (php-enable-drupal-coding-style))
(add-hook 'php-mode-hook  'my-php-mode-hook)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  (setq truncate-lines t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(winner-mode)
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq coffee-tab-width 2)
(setq ruby-insert-encoding-magic-comment nil)

;; =============================================================================
;; Helm, projectile, etc.
;; =============================================================================

(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(helm-mode 1)
(helm-autoresize-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(helm-descbinds-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
