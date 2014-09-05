;; Disable toolbar
(tool-bar-mode -1)

;; Enable tab bar
(tabbar-mode t)
(global-set-key [C-tab] 
		(lambda () 
		  (interactive)
		  (if tabbar-mode 
		      (tabbar-forward) 
		    (tabbar-mode t))))

;; Auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

