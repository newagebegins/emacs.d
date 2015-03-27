;; Packages that I use:
;; - markdown-mode
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default fill-column 80)
(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode)
(setq default-input-method "russian-computer")
(global-set-key (kbd "M-l") 'toggle-input-method)
(setq-default indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
;;(setq-default truncate-lines t)

; Smooth scroll
(setq scroll-step 3)

(setq-default c-basic-offset 4)
(setq c-default-style "stroustrup")
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))
