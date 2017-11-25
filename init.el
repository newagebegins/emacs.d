(require 'cl)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq my-packages '(markdown-mode
                    yaml-mode
                    jade-mode
                    coffee-mode
                    wgrep
                    ag
                    wgrep-ag
                    helm
                    projectile
                    helm-projectile
                    sass-mode
                    magit
                    git-commit
                    git-timemachine
                    zenburn-theme
                    js2-mode
                    rjsx-mode))

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
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.apib\\'" . gfm-mode))

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
  (c-set-offset 'case-label '+)
  (setq c-basic-offset 2)
  (setq c-default-style "awk")
  (setq truncate-lines t))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(defun my-js2-mode-hook ()
  (setq truncate-lines t)
  (setq fill-column 90))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(add-hook 'jade-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'coffee-mode-hook (lambda () (setq truncate-lines t)))

(winner-mode)
(setq set-mark-command-repeat-pop t)

(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

(setq coffee-tab-width 2)
(setq ruby-insert-encoding-magic-comment nil)

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

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;; https://www.emacswiki.org/emacs/TransposeWindows
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(require 'ob-sh)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-src-fontify-natively t)

;; Disable version control. Fixes "index.lock exists" error during rebase.
(setq vc-handled-backends nil)

(defun ansi-color-apply-on-region-int (beg end)
  "interactive version of func"
  (interactive "r")
  (ansi-color-apply-on-region beg end))

;; http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map [f10] 'recompile)
(define-key my-keys-minor-mode-map [f11] 'previous-error)
(define-key my-keys-minor-mode-map [f12] 'next-error)
(define-key my-keys-minor-mode-map [f9] 'magit-status)
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
(define-key my-keys-minor-mode-map (kbd "M-g") 'goto-line)
(define-key my-keys-minor-mode-map (kbd "C-a") 'mark-whole-buffer)
(define-key my-keys-minor-mode-map (kbd "M-j") 'dired-jump)
(define-key my-keys-minor-mode-map (kbd "C-5") 'query-replace)
(define-key my-keys-minor-mode-map (kbd "M-5") 'query-replace-regexp)
(define-key my-keys-minor-mode-map (kbd "C-\\") 'indent-region)
(define-key my-keys-minor-mode-map (kbd "M-6") 'delete-indentation)
(define-key my-keys-minor-mode-map (kbd "M-4") 'transpose-windows)
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
 '(ag-highlight-search t)
 '(auto-revert-interval 0.2)
 '(coffee-indent-like-python-mode t)
 '(column-number-mode t)
 '(git-commit-fill-column 100)
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(helm-buffer-max-length 30)
 '(helm-buffers-truncate-lines nil)
 '(helm-find-file-ignore-thing-at-point t)
 '(helm-inherit-input-method nil)
 '(helm-split-window-inside-p t)
 '(ibuffer-old-time 3)
 '(imenu-max-item-length nil)
 '(js-indent-level 2 t)
 '(js-switch-indent-offset 2)
 '(js2-indent-switch-body t)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-diff-refine-hunk (quote all))
 '(markdown-gfm-use-electric-backquote nil)
 '(menu-bar-mode nil)
 '(neo-window-width 40)
 '(org-imenu-depth 9)
 '(package-selected-packages
   (quote
    (rjsx-mode wgrep-ag ag git-timemachine js2-mode zenburn-theme magit sass-mode helm-projectile projectile helm wgrep coffee-mode jade-mode yaml-mode markdown-mode)))
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
