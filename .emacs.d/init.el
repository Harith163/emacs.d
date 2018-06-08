;; Sets up default look of emacs.
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Set new frame parameters. This includes starting fulscreen maximised and setting alpha. Only need to change the first number in (x . 50). No idea what the 50 does tho.
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

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
    ("6350f0cf3091e574a5de01d7309c0b456d814756a79867eac02c11b262d04a2e" default)))
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
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (alect-themes soothe-theme python-mode web-beautify web-mode js2-refactor js2-mode xref-js2 org-bullets org color-theme-sanityinc-tomorrow rainbow-delimiters jedi anaconda-mode flycheck-pycheckers outline-magic cdlatex yasnippet-snippets latex-preview-pane latex-pretty-symbols math-symbol-lists latex-extra auto-complete-auctex auto-complete)))
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

;; Outline-mode magic keybind, currently set to <C-tab>.
(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle))); folding sections in files

;; Switch between buffers.
(iswitchb-mode t)

;; Loading an org mode file as default.
(find-file "~/Desktop/Misc/life.org")

;;org-mode bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; adding folding to latex
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

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

;; Turns on pdf-tools mode by default.
(pdf-tools-install)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

;; My version of window split.
(defun newWindow()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (find-file "~/Desktop" ))

;; Setting keybind for newWindow function.
(global-set-key (kbd "C-x 3") 'newWindow)
(lookup-key (current-global-map) (kbd "C-x 3"))

;; javascript mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; js2_refractor and xref-js2.
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; web-mode for html files.
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; web beautify.
(eval-after-load 'js2-mode
      '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'json-mode
      '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
    (eval-after-load 'sgml-mode
      '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
    (eval-after-load 'css-mode
      '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
(lookup-key (current-global-map) (kbd "C-c b"))

;; linum
(global-linum-mode t)

;; disable linum for PDFView
(defun inhibit-global-linum-mode()
  "Doesn't Let line numbers in certain modes."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

(add-hook 'pdf-view-mode-hook 'inhibit-global-linum-mode)

;; In theory should turn on cdlatex mode by default. Doesn't work all that well though.
(autoload 'cdlatex-mode "cdlatex" "CDLatex Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLatex Mode" nil)
(add-hook 'Latex-mode-hook 'turn-on-cdlatex)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

;; Loads up emacs theme.
(load-theme 'soothe t)
(set-face-foreground 'font-lock-comment-face "#3c7780")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
