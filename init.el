;;; init.el --- Emacs configuration

;;; Commentary:
;;; See Readme.md for further details

;;; Code:

;;(package-initialize)
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("ess" . "http://kieranhealy.org/packages/") t)

(package-initialize)

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; quick alias for yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Skip startup message buffer
(setq inhibit-startup-message t)

;; ido mode for switching buffers
(ido-mode 1)
(setq ido-separator "\n")

;; popup switcher
(use-package popup-switcher
  :init (setq psw-in-window-center t)
  :bind ("<C-tab>" . psw-switch-buffer))

;; markdown mode
(use-package markdown-mode
  :mode
  (("\\.text\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)))

;; flycheck settings
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; spell checking
(use-package rw-language-and-country-codes)
(use-package rw-ispell)
(use-package rw-hunspell)

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")

(custom-set-variables
 '(rw-hunspell-default-dictionary "en_US")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "en_US") "ru_RU" "en_US")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)

;; duplicate current line
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
		      (buffer-substring (region-beginning) (region-end))
		    (prog1 (thing-at-point 'line)
		      (end-of-line)
		      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
			  (newline))))))
	(dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
	  (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
	(if (> 0 n)                             ;Comment out original with negative arg
	    (comment-region (line-beginning-position) (line-end-position)))
	(forward-line 1)
	(forward-char pos)))))

(global-set-key (kbd "C-S-d") 'duplicate-line-or-region)

;; moving lines up and down
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; backups in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ibuffer
(global-set-key (kbd "<f5>") 'ibuffer)

;; display column and line numbers
(line-number-mode 1)
(column-number-mode 1)

;; show trailing white spaces and empty lines
(setq-default indicate-empty-lines t
              show-trailing-whitespace t)

;; auto indent on ret
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Disable toolbar
(tool-bar-mode -1)

;; Highlight all parentheses
(use-package highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; project-explorer settings
(use-package project-explorer
  :init
  (setq pe/width 20)
  (setq pe/cache-enabled t))

;; comment or uncomment current line
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-?") 'toggle-comment-on-line)

;; load additional init files on demand
(defun load-user-file (name)
  (interactive "sFile to load: ")
  (load-file (expand-file-name (concat name ".el") "~/.emacs.d/")))

;; turn on buffer erase
(put 'erase-buffer 'disabled nil)

;; Company mode
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (use-package company-web)
)

;; Elixir and Phoenix
(use-package alchemist)

(provide 'init)
;;; init.el ends here
