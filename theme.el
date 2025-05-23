;;; theme.el -*- lexical-binding: t; -*-

;; Utility to set light & dark theme that follows system
;; This has only been tested on Gnome 48 and depends on gsettings

(use-package catppuccin-theme)

(defun set-color-scheme ()
  (set-theme
   (string-trim
    (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme"))))

(defun set-theme (scheme)
  (setq catppuccin-flavor (cond ((string-equal scheme "\'default\'")       'latte)
				((string-equal scheme "\'prefer-light\'")  'latte)
				((string-equal scheme "\'prefer-dark\'")   'mocha)))
  (load-theme 'catppuccin :no-confirm))

(defun monitor-theme-changes ()
  "Listen to gnome theme changes and run set the correct flavor"
  (let ((process (make-process
		  :name "gsettings-monitor"
		  :command '("gsettings" "monitor" "org.gnome.desktop.interface" "color-scheme")
		  :filter (lambda (_ output)
			    (when (string-match-p "color-scheme" output)
			      (set-color-scheme))))))
    ;; Kill on exit without user prompt
    (set-process-query-on-exit-flag process nil)))

;; Initialize settings
(set-color-scheme)
(monitor-theme-changes)

(provide 'theme)

;;; theme.el ends here
