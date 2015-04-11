(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(markdown-mode
                    helm
                    helm-descbinds
                    helm-projectile
                    php-mode
                    projectile
                    scss-mode
                    web-mode
                    zenburn-theme))

;; Fetch the list of available packages.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Load Zenburn theme.
(setq custom-safe-themes '("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))
(load-theme 'zenburn t)

;; Tweak font size.
(set-face-attribute 'default nil :height 98)

;; Disable backups and auto-saves.
(setq make-backup-files nil)
(setq auto-save-default nil)

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")
(global-set-key (kbd "M-o") 'toggle-input-method)

;; Disable annoying sounds on Windows.
(setq ring-bell-function 'ignore)
;; Show matching parenthesis.
(show-paren-mode)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; Smooth scrolling.
(setq scroll-step 3)
;; Use Github Flavored Markdown mode for markdown files.
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-x C-b") 'ibuffer)
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
  (setq c-default-style "stroustrup")
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
(defun my-web-mode-hook ()
  (setq truncate-lines t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

(winner-mode)

(global-set-key [f10] 'recompile)
(global-set-key [f11] 'previous-error)
(global-set-key [f12] 'next-error)
;;(global-set-key (kbd "C-o") 'other-window)
;;(global-set-key (kbd "M-r") 'save-buffer)

;; helm
(require 'helm-config)
(helm-mode 1)
;; Better window splitting (prevents too small windows when, for
;; example, re-builder is open)
(setq helm-split-window-in-side-p t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)

;; helm-descbinds
(helm-descbinds-mode)
