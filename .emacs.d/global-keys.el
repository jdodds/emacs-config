(global-set-key [f5] 'flymake-goto-prev-error)
(global-set-key [f6] 'flymake-goto-next-error)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-cy" 'clipboard-yank)
(global-set-key "\C-ck" 'clipboard-kill-ring-save)
(global-set-key "\C-cc" 'comment-or-uncomment-region)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-xB" 'jump-to-window)
(global-set-key "\C-xQ" 'macro-query)
(global-set-key "\C-x\C-b" 'browse-url-at-point)

(provide 'global-keys)
;;; global-keys.el ends here
