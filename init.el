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
    ("37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "563404393f53f6a989199c09a5cb8f663267495ca32d969099456f7eb9d70493" "d71f6c718dab453b625c407adc50479867a557668d5c21599a1ebea204d9e4f3" "2eb1f5551310e99101f0f9426485ab73aa5386054da877aacd15d438382bb72e" default)))
 '(package-selected-packages
   (quote
    (powerline-mode powerline cdlatex auctex TeX AUCTeX LaTeX-mode LaTeX latex expand-region jedi flycheck outline-mode use-package undo-tree rainbow-delimiters org-bullets org magit htmlize diminish auto-complete which-key try outline-magic darkokai-theme counsel ace-window))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :weight normal :height 167 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 2.0)))))


(org-babel-load-file (expand-file-name "~/.emacs.d/initfile.org"))
