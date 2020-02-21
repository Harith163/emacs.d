;; Add melpa and initialize
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;Force installation of use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c71d87d49a42b5c2220a648ffd9a45db115ab7d1b6406e83dae107fd4ccf53cb" default)))
 '(package-selected-packages
   (quote
    (dashboard smog seti-theme powerline-mode powerline cdlatex auctex TeX AUCTeX LaTeX-mode LaTeX latex expand-region jedi flycheck outline-mode use-package undo-tree rainbow-delimiters org-bullets org magit htmlize diminish auto-complete which-key try outline-magic darkokai-theme counsel ace-window))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight normal :height 167 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))


(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))

