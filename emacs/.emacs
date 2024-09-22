;; Emacs config file, filename must be .emacs and must lie in your home folder
;; Much honour goes to Lars Tveito (Emacs Guru), he has taught me all I know.
;;
;; Fork of @mathiasciarlo/emacs

(require 'loadhist)
;(require 'cl)
(require 'cl-lib)
;
;(file-dependents (feature-file 'cl))
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	    ("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
	))

;; install some packages if missing
(let* ((packages '(ac-js2
                   auto-complete
                   cider ;; REPL for Clojure in Emacs
;                   clojure-mode
;                   coffee-mode
                   yaml-mode
                   editorconfig
                   exec-path-from-shell
                   flycheck
                   go-autocomplete
                   go-eldoc
                   go-mode
;                   gradle-mode
                   ido-vertical-mode
                   js2-mode
                   json-mode
;                   magit
                   markdown-mode
                   multiple-cursors
                   paredit
                   rjsx-mode
                   solarized-theme
                   undo-tree
                   web-mode
                   ))
       (packages (cl-remove-if 'package-installed-p packages)))
  (when packages
    (package-refresh-contents)
    (mapc 'package-install packages)))


;; Paredit
;(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
;  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))

(setq lisp-loop-forms-indentation   2
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 2)

;; Show files beneath
(ido-vertical-mode 1)

;; use undo-tree-mode globally
(global-undo-tree-mode 1)

;; get the default config for auto-complete (downloaded with
;; package-manager)
(require 'auto-complete-config)

;; load the default config of auto-complete
(ac-config-default)
(setq ac-ignore-case nil)



;; Your theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(clojure-indent-style :always-indent)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
; '(preferred-dark 'solarized-dark)
; '(preferred-dark 'solarized-light)
; '(custom-enabled-themes preferred-dark)
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default))
 '(fci-rule-color "#073642")
 '(global-auto-revert-mode nil)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#3b6b40f432d7" "#07b9463d4d37" "#47a3341f358a" "#1d873c4056d5" "#2d87441c3362" "#43b7362e3199" "#061e418059d7"))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
     ("#5b7300" . 20)
     ("#007d76" . 30)
     ("#0061a8" . 50)
     ("#866300" . 60)
     ("#992700" . 70)
     ("#a00559" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#866300" "#992700" "#a7020a" "#a00559" "#243e9b" "#0061a8" "#007d76" "#5b7300"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(lsp-ui-doc-border "#93a1a1")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(protobuf-mode magit go-eldoc rjsx-mode restclient go-errcheck go-complete cypher-mode graphql-mode go-autocomplete gradle-mode 2048-game cider yaml-mode web-mode undo-tree try terraform-mode solarized-theme pretty-lambdada paredit multiple-cursors monokai-theme markdown-mode json-mode ido-vertical-mode groovy-mode go-mode flycheck exec-path-from-shell editorconfig dockerfile-mode coffee-mode clojure-mode auto-complete ac-js2))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4466ec20b5")
     (60 . "#c11679431550")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed992380000")
     (140 . "#96bf94d00000")
     (160 . "#8e5497440000")
     (180 . "#859900")
     (200 . "#77689bfc4636")
     (220 . "#6d449d475bfe")
     (240 . "#5fc09ea47093")
     (260 . "#4c69a01784aa")
     (280 . "#2aa198")
     (300 . "#303598e7affc")
     (320 . "#2fa1947dbb9b")
     (340 . "#2c889009c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#839496" "#657b83"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

;(if (not window-system)
;    (custom-set-variables
;     '(custom-enabled-themes (quote nil))))


(defun set-theme (theme)
  "Set and preserve theme"
  (custom-set-variables '(custom-enabled-themes theme))
)

(defun toggle-theme ()
  "Toggles between solarized-dark and solarized-light theme"
  (interactive)
  (let ((preferred-dark '(solarized-dark))
        (preferred-light '(solarized-light)))
    (if (cl-equalp preferred-dark custom-enabled-themes)
        (set-theme preferred-light)
      (set-theme preferred-dark))))

;        (custom-set-variables '(custom-enabled-themes preferred-light))
;      (custom-set-variables '(custom-enabled-themes preferred-dark)))))

(defun tt ()
  "Simplifies toggling themes between dark and light"
  (interactive)
  (toggle-theme))

(set-theme '(solarized-dark))

(defun rev ()
  "Revert buffer alias"
  (interactive)
  (revert-buffer))

(setq
 auto-save-default                        t ; nil to disable auto-save
 c-default-style                    "linux" ; Nice c indention.
 c-basic-offset                           4 ; Indentation
 default-directory                     "~/" ; Default home directory
 inhibit-startup-message                  t ; Removes start-up screen
 initial-scratch-message                 "" ; Removes default scratch text
 ring-bell-function                 'ignore ; Stop annoying system ringing noice
 word-wrap                                t ; Stop breaking lines splitting words
 org-support-shift-select                 t ; Enable org-mode shift select
 window-resize-pixelwise                  t ; Enable pixelwise window resizing

 ;; Web mode style
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset    2
 web-mode-code-indent-offset   2

 ;; Js/Jsx-mode
 js2-basic-offset 2
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning nil
 sgml-basic-offset 2

 )

(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs


;; To avoid file system clutter we put all auto saved files in a single
;; directory.
(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
directory called autosaves located wherever your .emacs.d/ is
located.")

(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))


;; Basic looks
(blink-cursor-mode 0)  ; Self explainatory
(column-number-mode 1) ; Shows column number at the bottom
(global-display-line-numbers-mode 1)  ; Shows line number on the left hand side

(show-paren-mode 1)    ; Marks matching paranthesis

;; Less toolbars, more text. We have shortcuts
(menu-bar-mode 1)      ; Hide menu
(tool-bar-mode 0)      ; Hide toolbar
(scroll-bar-mode 0)    ; Hide scrollbar


;; Adds closing parents automatically
(electric-pair-mode 1)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))

;; Remove trailing whitespaces when saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Web development modes
;;
;; Enable globl flycheck mode
(require 'flycheck)

;; Prefer eslint to jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; Use eslint with js2-mode
;;(flycheck-add-mode 'javascript-eslint 'js2-mode)
;;(add-hook 'js2-mode-hook 'flycheck-mode)
;;(add-hook 'js2-mode-hook 'ac-js2-mode)

(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
(add-hook 'rjsx-mode-hook 'flycheck-mode)
(add-hook 'rjsx-mode-hook 'ac-js2-mode)

;; Customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disable-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(add-to-list 'auto-mode-alist '(".sh-" . sh-mode))
(add-to-list 'auto-mode-alist '(".sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '(".bash" . sh-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))


;;(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))

(add-to-list 'interpreter-mode-alist '("node" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))


;; EditorConfig setup
(require 'editorconfig)
;; Enable editorconfig by default
(editorconfig-mode 1)

;; Answer yes or no with y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Overwrite marked text
(delete-selection-mode 1)

;; enable ido-mode, changes the way files are selected in the minibuffer
(ido-mode 1)
;; use ido everywhere
(ido-everywhere 1)
;; show vertically
(ido-vertical-mode 1)

;; use undo-tree-mode globally
(global-undo-tree-mode 1)

;; Word-wrapping
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'help-mode-hook 'visual-line-mode)

;; Open config-file
(defun init()
  (interactive)
  (find-file "~/.emacs"))

(defun godev ()
  (interactive)
  (find-file "~/dev/go/src/"))

(defun docs ()
  (interactive)
  (find-file "~/Documents"))

;; Change focus between windows in emacs
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

;; To tidy up a buffer we define this function borrowed from simenheg
(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

;; Kill process and buffer
(defun kill-shell ()
  (interactive)
  (delete-process "*shell*")
  (kill-buffer "*shell*"))

;; Tidy all buffers that are not read-only
(defun tidy-all-buffers()
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (when (eq buffer-read-only nil)
        (tidy)))))


;; Full screen
(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; Scrolling
(defun pan-up()
  (interactive)
  (scroll-down 4))

(defun pan-down()
  (interactive)
  (scroll-up 4))

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(setq word-wrap t)

(global-set-key (kbd "<C-tab>") 'tidy)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-2") 'select-next-window)
(global-set-key (kbd "M-1")  'select-previous-window)
(global-set-key (kbd "M-n") 'pan-down)
(global-set-key (kbd "M-p") 'pan-up)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "RET") 'newline-and-indent)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Mac OSX tweaks
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
        mac-option-modifier nil
        mac-command-modifier 'meta
        mac-allow-anti-aliasing 't
        x-select-enable-clipboard t)
  (exec-path-from-shell-initialize))


;(file-dependents (feature-file 'cl))
