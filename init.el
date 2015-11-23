;; Package management
(require 'package)

;;; Code:
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("ess" . "http://kieranhealy.org/packages/") t)

(package-initialize)

;; ido mode for switching buffers
(ido-mode 1)
(setq ido-separator "\n")

;; popup switcher
(require 'popup-switcher)
(setq psw-in-window-center t)
(global-set-key (kbd "<C-tab>") 'psw-switch-buffer)

;; markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; spell checking
(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(require 'rw-hunspell)

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US_hunspell")

(custom-set-variables
 '(rw-hunspell-default-dictionary "en_US_hunspell")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
)

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "en_US_hunspell") "ru_RU_hunspell" "en_US_hunspell")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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

;; quick alias for yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight all parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; project-explorer settings
(setq pe/cache-enabled t)
(setq pe/width 50)

;; flycheck settings
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-rust-check-tests t)

;; comment or uncomment current line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-?") 'toggle-comment-on-line)
