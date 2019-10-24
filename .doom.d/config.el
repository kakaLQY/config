;; Place your private configuration here

;; Env vars
(setenv "SHELL" "fish")

(setq display-line-numbers-type 'relative)
(setq show-trailing-whitespace t)

(setq doom-font (font-spec :family "Fira Code" :size 13))

(add-hook! 'js2-mode-hook 'eslintd-fix-mode)

(add-hook! 'before-save-hook
           'delete-trailing-whitespace
           'doom/delete-trailing-newlines)

(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en" "--run-together" "--camel-case"))

;; undo-tree
(global-undo-tree-mode 1)

;; String manipulation by using s package
(defun str-camel-case (beg end)
  "some words => someWords"
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (delete-region beg end)
    (insert (s-lower-camel-case s))))


;; Org Mode
(let ((org-folder-mac-os "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
      (org-folder-other-os "~/org"))
  (setq org-agenda-files (list org-folder-mac-os
                               (concat org-folder-mac-os "/private")       ;; For private device
                               org-folder-other-os
                               (concat org-folder-other-os "/private"))))
