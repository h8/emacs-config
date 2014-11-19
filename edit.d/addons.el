;; Move lines, words and regions with Alt+Arrows
(drag-stuff-global-mode)

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-d") 'duplicate-line)

;; Highlight all parentheses
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
highlight-parentheses-mode
(lambda ()
(highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)
