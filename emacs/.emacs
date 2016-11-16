;; Emacs config file, filename must be .emacs and must lie in your home folder
;; Much honour goes to Lars Tveito (Emacs Guru), he has taught me all I know.
;;
;; Fork of @mathiasciarlo/emacs

(require 'cl)
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; install some packages if missing
(let* ((packages '(auto-complete
                   exec-path-from-shell
                   ido-vertical-mode
                   markdown-mode
                  ;monokai-theme
                   atom-one-dark-theme
                   multiple-cursors
                   paredit
                   pretty-lambdada
                   undo-tree
                   try
                   flycheck
                   flyspell
                   haskell-mode
                   web-mode
                   js2-mode
                   ac-js2
                   json-mode
                   editorconfig
                   coffee-mode
                   jade-mode
                   dockerfile-mode
                   solarized-theme
                   yaml-mode
                   ))
       (packages (remove-if 'package-installed-p packages)))
  (when packages
    (package-refresh-contents)
    (mapc 'package-install packages)))

;; Paredit
(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))

(setq lisp-loop-forms-indentation   4
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 4)

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
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (groovy-mode w3 markdown-mode+ ox-gfm go-mode shared-buffer yaml-mode web-mode undo-tree tss try slime pretty-lambdada paredit multiple-cursors monokai-theme markdown-mode magit jsx-mode jade-mode ido-vertical-mode haskell-mode geiser focus flycheck exec-path-from-shell editorconfig dockerfile-mode coffee-mode atom-one-dark-theme ac-js2))))

(setq
 auto-save-default                        t ; nil to disable auto-save
 c-default-style                    "linux" ; Nice c indention.
 c-basic-offset                           4 ; Indentation
 default-directory                     "~/" ; Default home directory
 inhibit-startup-message                  t ; Removes start-up screen
 initial-scratch-message                 "" ; Removes default scratch text
 ring-bell-function                 'ignore ; Stop annoying system ringing noice
 word-wrap                                t ; Stop breaking lines splitting words

 ;; Web mode style
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset    2
 web-mode-code-indent-offset   2

 ;; Js/Jsx-mode
 js2-basic-offset 2
 js2-strict-missing-semi-warning nil
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


(require 'markdown-mode)

(setq my-markdown-gfm-command "/usr/local/bin/marked")
(setq my-markdown-command "/usr/local/bin/markdown")

(cond
 ((file-exists-p my-markdown-gfm-command)
  (setq markdown-command my-markdown-gfm-command)
  (setq markdown-command-needs-filename t))
 ((file-exists-p my-markdown-command)
  (setq markdown-command my-markdown-command)))

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(add-hook 'visual-line-mode-hook
	  '(lambda()
	     (setq word-wrap nil)))

(add-hook 'gfm-mode-hook
 	  '(lambda()
	     (setq indent-tabs-mode nil)
	     (setq tab-width 4)))

;; Basic looks
(blink-cursor-mode 0)  ; Self explainatory
(column-number-mode 1) ; Shows column number at the bottom
(global-linum-mode 1)  ; Shows line number on the left hand side
(show-paren-mode 1)    ; Marks matching paranthesis

;; Less toolbars, more text. We have shortcuts
(menu-bar-mode 0)      ; Hide menu
(tool-bar-mode 0)      ; Hide toolbar
(scroll-bar-mode 0)    ; Hide scrollbar


;; Adds closing parents automatically
(electric-pair-mode 1)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))

;; Remove trailing whitespaces when saving files.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Correctly handle hanging indentation
(defun hanging-indentation-mode ()
      (c-set-offset 'arglist-intro '+))

;; Web development modes
;;
;; Enable globl flycheck mode
(require 'flycheck)

;; Prefer eslint to jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; Use eslint with js2-mode
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'hanging-indentation-mode)

;; Customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disable-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . web-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; Docker
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

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

;; Change focus between windows in emacs with Alt-left and Alt-right
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


;; Opens a shell in the next window
(defun open-shell ()
  (interactive)
  (select-window (next-window))
  (shell)
  (select-window (previous-window)))


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
(defun scroll-opp()
  (interactive)
  (scroll-down 4))

(defun scroll-ned()
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
(global-set-key (kbd "M-n") 'scroll-ned)
(global-set-key (kbd "M-p") 'scroll-opp)

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
        x-select-enable-clipboard t)
  (exec-path-from-shell-initialize))
