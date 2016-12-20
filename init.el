(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq my-packages '(markdown-mode
                    yaml-mode
                    jade-mode
                    coffee-mode
                    wgrep
                    helm
                    projectile
                    helm-projectile
                    helm-descbinds
                    flycheck
                    sass-mode
                    magit
                    git-commit
                    zenburn-theme
                    js2-mode))

;; Install missing packages.
(unless (every #'package-installed-p my-packages)
  (package-refresh-contents)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(load-theme 'zenburn t)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

(setq make-backup-files nil)

(prefer-coding-system 'utf-8)
(setq default-input-method "russian-computer")

(setq ring-bell-function 'ignore) ; Disable annoying sounds on Windows.
(show-paren-mode)
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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output 'first-error)
(savehist-mode 1)

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
  (setq c-basic-offset 4)
  (setq c-default-style "awk"))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(add-hook 'js2-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'jade-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'coffee-mode-hook (lambda () (setq truncate-lines t)))

(winner-mode)
(setq set-mark-command-repeat-pop t)

(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

(setq coffee-tab-width 2)
(setq ruby-insert-encoding-magic-comment nil)

(require 'server)
(unless (server-running-p) (server-start))

(add-to-list 'safe-local-variable-values '(encoding . utf-8))

(defun join-lines () (interactive) (let ((fill-column 999999)) (fill-paragraph nil)))

(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(helm-mode 1)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(helm-descbinds-mode)

;;(add-hook 'after-init-hook #'global-flycheck-mode)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)))

;; https://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-src-fontify-natively t)

;; Disable version control. Fixes "index.lock exists" error during rebase.
(setq vc-handled-backends nil)

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map [f8] 'helm-resume)
(define-key my-keys-minor-mode-map [f10] 'recompile)
(define-key my-keys-minor-mode-map [f11] 'previous-error)
(define-key my-keys-minor-mode-map [f12] 'next-error)
(define-key my-keys-minor-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-keys-minor-mode-map (kbd "M-r") 'save-buffer)
(define-key my-keys-minor-mode-map (kbd "M-R") (kbd "C-u C-x s")) ; save all buffers silently
(define-key my-keys-minor-mode-map (kbd "M-v") 'helm-mini)
(define-key my-keys-minor-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key my-keys-minor-mode-map (kbd "C-t") 'other-window)
(define-key my-keys-minor-mode-map (kbd "M-f") 'helm-find-files)
(define-key my-keys-minor-mode-map (kbd "M-1") 'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-p") 'helm-projectile)
(define-key my-keys-minor-mode-map (kbd "C-e") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)
(define-key my-keys-minor-mode-map (kbd "C-d") 'kill-whole-line)
(define-key my-keys-minor-mode-map (kbd "M-o") (kbd "C-u C-SPC"))
(define-key my-keys-minor-mode-map (kbd "M-3") 'split-window-right)
(define-key my-keys-minor-mode-map (kbd "M-2") 'split-window-below)
(define-key my-keys-minor-mode-map (kbd "M-0") 'delete-window)
(define-key my-keys-minor-mode-map (kbd "M-t") 'helm-semantic-or-imenu)
(define-key my-keys-minor-mode-map (kbd "M-y") 'helm-show-kill-ring)
(define-key my-keys-minor-mode-map (kbd "M-7") 'jump-to-register)
(define-key my-keys-minor-mode-map (kbd "M-8") 'point-to-register)
(define-key my-keys-minor-mode-map (kbd "M-g") 'goto-line)
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "M-j") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "M-5") 'ibuffer)
(define-key helm-map (kbd "M-RET") 'helm-ff-run-switch-other-window)

(define-key my-keys-minor-mode-map (kbd "M-l") 'toggle-input-method)
(define-key isearch-mode-map (kbd "M-l") 'isearch-toggle-input-method)
(define-key helm-map (kbd "M-l") 'toggle-input-method)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.2)
 '(coffee-indent-like-python-mode t)
 '(column-number-mode t)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-buffer-max-length 30)
 '(helm-buffers-truncate-lines nil)
 '(helm-inherit-input-method nil)
 '(helm-split-window-in-side-p t)
 '(ibuffer-old-time 3)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-indent-switch-body t)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-diff-refine-hunk (quote all))
 '(menu-bar-mode nil)
 '(neo-window-width 40)
 '(org-imenu-depth 9)
 '(package-selected-packages
   (quote
    (git-timemachine js2-mode zenburn-theme magit sass-mode flycheck helm-descbinds helm-projectile projectile helm wgrep coffee-mode jade-mode yaml-mode markdown-mode)))
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(undo-limit 8000000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
