(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(js-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'iso-transl)

(global-set-key (kbd "<dead-tilde>") "~")



(defun init-hook ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    (org-babel-tangle)
    (byte-compile-file (concat user-emacs-directory "init.el"))))

(add-hook 'after-save-hook 'init-hook)

(global-linum-mode)
(global-visual-line-mode)



;; Managing packages
(require 'package)
(package-initialize)

(add-to-list
 'package-archives
 '("MELPA" . "http://melpa.milkbox.net/packages/") t)

(defun newest-package-installed-p (package)
;  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (or (assq package package-alist)
                               (assq package package--builtins)))
           (newest-pkg-desc (assq package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-vers (cdr local-pkg-desc))
                           (package-desc-vers (cdr newest-pkg-desc)))))))

(defun upgrade-or-install-package (package)
;  "Unless the newest available version of PACKAGE is installed
;PACKAGE is installed and the current version is deleted."
  (unless (newest-package-installed-p package)
    (let ((pkg-desc (assq package package-alist)))
      (when pkg-desc
        (package-delete (symbol-name package)
                        (package-version-join
                         (package-desc-vers (cdr pkg-desc)))))
      (package-install package))))


(defvar days-between-updates 14)
(defvar do-package-update-on-init t)
(defvar package-last-update-file
  (expand-file-name (concat user-emacs-directory ".package-last-update")))

(require 'time-stamp)
;; Open the package-last-update-file
(with-temp-file package-last-update-file
  (if (file-exists-p package-last-update-file)
      (progn
        ;; Insert it's original content's.
        (insert-file-contents package-last-update-file)
        (let ((start (re-search-forward time-stamp-start nil t))
              (end (re-search-forward time-stamp-end nil t)))
          (when (and start end)
            ;; Assuming we have found a time-stamp, we check determine if it's
            ;; time to update.
            (setq do-package-update-on-init
                  (<= days-between-updates
                      (days-between
                       (current-time-string)
                       (buffer-substring-no-properties start end))))
            ;; Remember to update the time-stamp.
            (when do-package-update-on-init
              (time-stamp)))))
    ;; If no such file exists it is created with a time-stamp.
    (insert "Time-stamp: <>")
    (time-stamp)))


(when (and do-package-update-on-init
           (y-or-n-p "Update all packages?"))
  (package-refresh-contents)

  (dolist (package
           '(ac-geiser         ; Auto-complete backend for geiser
             ac-slime          ; An auto-complete source using slime completions
             ace-jump-mode     ; quick cursor location minor mode
             auto-compile      ; automatically compile Emacs Lisp libraries
             auto-complete     ; auto completion
             elscreen          ; window session manager
             expand-region     ; Increase selected region by semantic units
             flx-ido           ; flx integration for ido
             ido-vertical-mode ; Makes ido-mode display vertically.
             geiser            ; GNU Emacs and Scheme talk to each other
             haskell-mode      ; A Haskell editing mode
             jedi              ; Python auto-completion for Emacs
             magit             ; control Git from Emacs
             markdown-mode     ; Emacs Major mode for Markdown-formatted files.
             matlab-mode       ; MATLAB integration with Emacs.
             monokai-theme     ; A fruity color theme for Emacs.
             move-text         ; Move current line or region with M-up or M-down
             multiple-cursors  ; Multiple cursors for Emacs.
             org               ; Outline-based notes management and organizer
             paredit           ; minor mode for editing parentheses
             powerline         ; Rewrite of Powerline
             pretty-lambdada   ; the word `lambda' as the Greek letter.
             smex))            ; M-x interface with Ido-style fuzzy matching.
    (upgrade-or-install-package package))
  ;; This package is only relevant for Mac OS X.
  (when (memq window-system '(mac ns))
    (upgrade-or-install-package 'exec-path-from-shell)))


(dolist (feature
         '(auto-compile             ; auto-compile .el files
           auto-complete-config     ; a configuration for auto-complete-mode
           jedi                     ; auto-completion for python
           matlab                   ; matlab-mode
           ob-matlab                ; org-babel matlab
           ox-latex                 ; the latex-exporter (from org)
           ox-md                    ; Markdown exporter (from org)
           pretty-lambdada          ; show 'lambda' as the greek letter.
           recentf                  ; recently opened files
           tex-mode))               ; TeX, LaTeX, and SliTeX mode commands
  (require feature))

(setq initial-scratch-message nil     ; Clean scratch buffer.
      inhibit-startup-message t       ; No splash screen please.
      default-input-method "TeX"      ; Use TeX when toggeling input method.
      ring-bell-function 'ignore      ; Quite as a mouse.
      doc-view-continuous t           ; At page edge goto next/previous.
      echo-keystrokes 0.1)            ; Show keystrokes asap.



(let ((default-directory (concat user-emacs-directory "site-lisp/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(fset 'yes-or-no-p 'y-or-n-p)


(set-language-environment "UTF-8")

(put 'narrow-to-region 'disabled nil)

(ac-config-default)

(add-hook 'doc-view-mode-hook 'auto-revert-mode)


(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           scroll-bar-mode              ; No scroll bars either.
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

(load-theme 'monokai t)

(when (member "Inconsolata-g" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-g-11"))


(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active (powerline-selected-window-active))
           ;; left hand side displays Read only or Modified.
           (lhs (list (powerline-raw
                       (cond (buffer-read-only "Read only")
                             ((buffer-modified-p) "Modified")
                             (t "")) nil 'l)))
           ;; right side hand displays (line,column).
           (rhs (list
                 (powerline-raw
                  (concat
                   "(" (number-to-string (line-number-at-pos))
                   "," (number-to-string (current-column)) ")") nil 'r)))
           ;; center displays buffer name.
           (center (list (powerline-raw "%b" nil))))
      (concat (powerline-render lhs)
              (powerline-fill-center nil (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill nil (powerline-width rhs))
              (powerline-render rhs))))))

(global-set-key (kbd "<M-S-up>")    'move-text-up)
(global-set-key (kbd "<M-S-down>")  'move-text-down)


;; LISP

(dolist (mode '(slime-repl-mode geiser-repl-mode))
  (add-to-list 'pretty-lambda-auto-modes mode))

(pretty-lambda-for-modes)

(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; Scheme
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))
(setq geiser-active-implementations '(racket))
(show-paren-mode 1)



;; Java and C
(defun c-setup ()
  (local-set-key (kbd "C-c C-c") 'compile))

(define-abbrev-table 'java-mode-abbrev-table
  '(("psv" "public static void main(String[] args) {" nil 0)
    ("sopl" "System.out.println" nil 0)
    ("sop" "System.out.printf" nil 0)))

(defun java-setup ()
  (abbrev-mode t)
  (setq-local compile-command (concat "javac " (buffer-name))))

(add-hook 'java-mode-hook 'java-setup)

;; TEX

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      (mapcar
       (lambda (str)
         (concat "pdflatex -shell-escape "
                 (substring str (string-match "-" str))))
       org-latex-pdf-process))

(setcar (cdr (cddaar tex-compile-commands)) " -shell-escape ")

;; Python

;; (setq jedi:server-command
;;       (cons "python3" (cdr jedi:server-command))
;;       python-shell-interpreter "python3")
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:ac-setup)


;; MATLAB

(add-to-list 'matlab-shell-command-switches "-nosplash")


;; Arduino support

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

