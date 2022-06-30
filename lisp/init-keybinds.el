;; Write tilde as C-x \ (it layout)
(defun tilde () (interactive) (insert "~"))
(global-set-key (kbd "C-x \\" 'tilde)

(provide 'init-keybinds)
