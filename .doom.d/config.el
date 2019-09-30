;; Place your private configuration here

(setq display-line-numbers-type 'relative)
(setq show-trailing-whitespace t)

(setq doom-font (font-spec :family "Fira Code" :size 13))

(add-hook! 'js2-mode-hook 'eslintd-fix-mode)

(add-hook! 'before-save-hook
           'delete-trailing-whitespace
           'doom/delete-trailing-newlines)

(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en" "--run-together")) ;; --camel-case on 0.60.8

;; undo-tree
(global-undo-tree-mode 1)

;; String manipulation by using s package
(defun str-camel-case (beg end)
  "some words => someWords"
  (interactive "r")
  (let ((s (buffer-substring beg end)))
    (delete-region beg end)
    (insert (s-lower-camel-case s))))

(setenv "SHELL" "fish")
