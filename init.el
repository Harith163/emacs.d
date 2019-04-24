;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs.

;; Sets up default look of emacs and basic stuff.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-visual-line-mode t)
(setq initial-major-mode 'org-mode)


;; Set new frame parameters. This includes starting fulscreen maximised and setting alpha. Only need to change the first number in (x . 50). No idea what the 50 does tho.
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#454545" "#d65946" "#6aaf50" "#baba36" "#5180b3" "#ab75c3" "#68a5e9" "#bdbdb3"])
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "ab564a7ce7f2b2ad9e2cfe9fe1019b5481809dd7a0e36240c9139e230cc2bc32" "6350f0cf3091e574a5de01d7309c0b456d814756a79867eac02c11b262d04a2e" default)))
 '(fci-rule-color "#424748")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("hw&notes" "\\documentclass{hw&notes}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(package-selected-packages
   (quote
    (auto-complete-auctex git-auto-commit-mode darkokai-theme monokai-theme ac-ispell org-ac pdf-tools org-bullets org rainbow-delimiters outline-magic cdlatex latex-preview-pane latex-pretty-symbols math-symbol-lists latex-extra auto-complete)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))

;; Auto Complete
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'org-mode) 
(add-to-list 'ac-modes 'TeX-mode)

;; Outline-mode magic keybind, currently set to <C-tab>. Folding sections in files.
(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

;; adding folding to latex
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

;; Switch between buffers.
(icomplete-mode t)

;; org-mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; org mode latex colors.
(setq org-highlight-latex-and-related '(latex))

;; Loading an org mode file as default.
(find-file "~/Documents/life/life.org")

;; Latex Stuff
(latex-preview-pane-enable)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq latex-run-command "pdflatex")

(add-hook 'TeX-mode-hook
          '(lambda () (local-set-key (kbd "RET") 'newline-and-indent))); Tex auto-indent
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Rainbow Delimitters
(add-hook 'Latex-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; In theory should turn on cdlatex mode by default. Doesn't work all that well though.
(require 'cdlatex)
(autoload 'cdlatex-mode "cdlatex" "CDLatex Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLatex Mode" nil)
(add-hook 'LaTex-mode-hook 'turn-on-cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; My version of window split.
(defun newWindow()
  "My new window."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (find-file "~/Documents" ))

;; Setting keybind for newWindow function.
(global-set-key (kbd "C-x 3") 'newWindow)
(lookup-key (current-global-map) (kbd "C-x 3"))

;; linum
(global-linum-mode t)

;; disable linum for PDFView
(defun inhibit-global-linum-mode()
  "Doesn't Let line numbers in certain modes."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))
(add-hook 'pdf-view-mode-hook 'inhibit-global-linum-mode)

;;emacs theme
(load-theme 'darkokai t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
