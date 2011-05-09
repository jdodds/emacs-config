(menu-bar-mode -1)

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (boundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (boundp 'display-battery-mode)
    (display-battery-mode 1))

(global-auto-revert-mode)
(global-auto-complete-mode t)
(global-font-lock-mode t)

(show-paren-mode t)
(iswitchb-mode t)   
(winner-mode 1)

(setq transient-mark-mode nil)
(setq auto-save-default nil)
(setq column-number-mode t)

(setq midnight-mode t)
(auto-fill-mode t)

(provide 'global-modes)
;;; global-modes.el ends here
