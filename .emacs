;; Use ibuffer as default buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; add folders to load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/libraries"))
(load-theme 'dracula t)
