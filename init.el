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
(show-paren-mode)
(setq default-input-method "russian-computer")
(setq-default indicate-empty-lines t)
(setq ring-bell-function 'ignore)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
