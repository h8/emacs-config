;; Autoindent on Enter
(add-hook 'scala-mode-hook
	  (lambda () (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

