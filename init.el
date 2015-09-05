(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq my-packages '(markdown-mode
                    php-mode
                    scss-mode
                    web-mode
                    zenburn-theme
                    yaml-mode
                    monokai-theme))

;; Install missing packages.
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Load Zenburn theme.
;;(setq custom-safe-themes '("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))
;;(load-theme 'zenburn t)

(setq custom-safe-themes '("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))
(load-theme 'monokai t)

;; Tweak font size.
;;(set-face-attribute 'default nil :height 98)
;;(set-face-attribute 'default nil :font "Consolas-11")
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

;; Disable backups and auto-saves.
(setq make-backup-files nil)
(setq auto-save-default nil)

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")
(global-set-key (kbd "M-l") 'toggle-input-method)
(define-key isearch-mode-map (kbd "M-l") 'isearch-toggle-input-method)

;; Disable annoying sounds on Windows.
(setq ring-bell-function 'ignore)
;; Show matching parenthesis.
(show-paren-mode)
(menu-bar-mode -1)
(tool-bar-mode 0)
(blink-cursor-mode 0)
;;(global-hl-line-mode 1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; Smooth scrolling.
;;(setq scroll-step 3)
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

(require 'ido)
(ido-mode t)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-key my-keys-minor-mode-map [f10] 'recompile)
(define-key my-keys-minor-mode-map [f11] 'previous-error)
(define-key my-keys-minor-mode-map [f12] 'next-error)
(define-key my-keys-minor-mode-map (kbd "M-r") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "M-v") 'switch-to-buffer)
(define-key my-keys-minor-mode-map (kbd "M-V") 'switch-to-buffer-other-window)
(define-key my-keys-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key my-keys-minor-mode-map (kbd "C-t") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-f") 'find-file)
(define-key my-keys-minor-mode-map (kbd "M-F") 'find-file-other-window)
(define-key my-keys-minor-mode-map (kbd "M-1") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-e") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "C-d") 'kill-line)
(define-key my-keys-minor-mode-map (kbd "M-o") (kbd "C-u C-SPC"))
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
