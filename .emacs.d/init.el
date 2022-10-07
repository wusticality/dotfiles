;; Kevin Depue (2017)
;; My personal emacs configuration. I used to use the
;; package setup from Magnars, but now I'm going with
;; use-package. Always a work in progress.

;; TODO
;; * Navigate to the *scratch* buffer and close
;;   all other windows once we are done loading.
;; * Figure out a way to copy the thing under point.

;; PACKAGES TO EXPLORE
;; * slime
;; * highlight-escape-sequences
;; * jump-char
;; * gist
;; * move-text
;; * string-edit
;; * rainbow-delimiters
;; * rainbow-identifiers
;; * sexp-fu
;; * swiper-helm
;; * rjsx-mode
;; * skewer-mode

;;
;; platform
;;

;; Define some platform-specific variables.
(setq is-terminal (equal window-system nil))
(setq is-gui (memq window-system '(mac ns x)))
(setq is-osx (equal system-type 'darwin))
(setq is-wsl
      (and
       (equal system-type 'gnu/linux)
       (string-match "[Mm]icrosoft" operating-system-release)))
(setq is-gnu (memq system-type '(gnu gnu/linux gnu/kfreebsd)))

;;
;; private
;;

;; Variables for erc.
(defvar my-erc-nick nil)
(defvar my-erc-password nil)

;; Put all private variables here.
(load "~/.private.el" t)

;;
;; custom
;;

;; Put the special emacs "customize" settings in their own
;; file. We load it first because we may want to override
;; these settings in our config below.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;
;; basic
;;

;; I don't ever want to see this.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 0))

;; Move as far as possible when scrolling.
(setq  scroll-error-top-bottom t)

;; No splash screen / startup message.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Clear the minibuffer please.
(defun display-startup-echo-area-message ()
  (message ""))

;; Make the scratch buffer empty.
(setq initial-scratch-message "")

;; Auto refresh buffers.
(global-auto-revert-mode 1)

;; Auto refresh dired.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Highlight the line that point is on.
(global-hl-line-mode t)

;; Answering 'y' or 'n' will do.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use utf8 encoding please.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always show line and column numbers.
(setq line-number-mode t)
(setq column-number-mode t)

;; Subword mode is the cat's meow.
(global-subword-mode 1)

;; We're not in the 80's anymore.
(setq gc-cons-threshold 100000000)

;; Shoot for an 80 character width.
(setq fill-column 80)
(setq-default fill-column 80)

;; Make a vertical split more likely.
(setq split-height-threshold 100)

;; Save minibuffer history.
(savehist-mode 1)
(setq history-length 1000)

;; Decrease minibuffer echo time.
(setq echo-keystrokes 0.1)

;; If inserting text, replace active region.
(delete-selection-mode 1)

;; If loading code from a byte-compiled elc
;; file, prefer raw code from an el file if
;; it is newer.
(setq load-prefer-newer t)

;; If we open a help buffer in
;; any way, navigate to it.
(setq help-window-select t)

;; Show matching delimiters instantly.
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Don't automatically debug on error.
;; We enable this by hand if desired.
(setq debug-on-error nil)

;; Don't blink the cursor.
(setq blink-cursor-mode nil)

;; Don't use a visual bell.
(setq ring-bell-function 'ignore)

;; Cursor types.
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows nil)

;; Start gui emacs fullscreen.
(when (and is-osx is-gui)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;; Bind C-M-h to M-<backspace>.
(define-key key-translation-map [?\C-\M-h] [?\C-\M-?])

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Bind other-frame to a useful hotkey.
(global-set-key (kbd "C-c o") 'other-frame)

;; This keybinding is annoying as hell.
(global-unset-key (kbd "C-x C-b"))

;;
;; completion
;;

;; Set this to helm or ivy.
(setq completion-system 'ivy)

;; Helper variables.
(setq use-helm (eq completion-system 'helm))
(setq use-ivy (eq completion-system 'ivy))

;;
;; backups
;;

;; TODO
;; Find a better way to deal with backups.

;; Put backup files in their own directory.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Backup files even if we're using source control.
(setq vc-make-backup-files t)

;;
;; font
;;

(when (and is-osx is-gui)
  (custom-set-faces
   '(default ((t (:height 160 :width normal :family "Menlo"))))))

(when (and is-gnu is-gui)
  (custom-set-faces
   '(default ((t (:height 140 :width normal :family "Consolas"))))))

;;
;; c / c++
;;

;; TODO
;; * See if we can initialize this stuff
;;   using the use-package macro.

;; Open header files in c++ mode.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Style defaults.
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; The style.
(add-hook
 'c-mode-common-hook
 (lambda ()
   (c-set-style "k&r")
   (setq tab-width 4)
   (setq indent-tabs-mode nil)
   (setq c-basic-offset 4)
   (c-set-offset 'inline-open '0)
   (c-set-offset 'case-label '4)
   (electric-pair-mode 1)))

;;
;; latex
;;

;; Only use one '%' in comments, please.
(add-hook 'latex-mode-hook (lambda () (setq-local comment-add 0)))

;;
;; packages
;;

;; Setup our load path.
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Setup packages.
(require 'package)
(setq package-enable-at-startup nil)

;; Setup package repositories.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize packages.
(package-initialize)

;; Install / setup use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;
;; libraries
;;

;; The string library.
(use-package s
  :ensure t)

;; The file library.
(use-package f
  :ensure t)

;; The list library.
(use-package dash
  :ensure t)

;;
;; copy / paste
;;

;; Copy / paste on osx / gnu.
(when (and (or is-osx is-gnu) (not is-wsl) is-terminal)
  (use-package xclip
    :ensure t
    :config
    (progn (xclip-mode 1))))

;; Copy / paste on wsl.
(when (and is-wsl is-terminal)
  ;; Copy from the clipboard.
  (defun wsl-copy ()
    (s-replace "" "" (shell-command-to-string "win32yank -o")))

  ;; Paste from the clipboard.
  (defun wsl-paste (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "win32yank" nil "win32yank" "-i")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; Install the commands.
  (setq interprogram-paste-function 'wsl-copy)
  (setq interprogram-cut-function 'wsl-paste))

;;
;; paths
;;

;; For gui emacs, loads PATH from ~/.profile.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when is-gui
    (exec-path-from-shell-initialize)))

;;
;; theme
;;

;; Setup our theme path.
;; (setq custom-theme-directory (concat user-emacs-directory "themes"))

;; Use the it3ration theme for now.
;; (condition-case nil
;;     (load-theme 'it3ration t)
;;   (wrong-number-of-arguments (load-theme 'it3ration)))

;; This package is just awesome, details can be found here:
;; * http://chriskempson.com/projects/base16/?
;; * https://github.com/belak/base16-emasc
;; You must use a terminal that supports 24bit color:
;; * https://github.com/syl20bnr/spacemacs/wiki/Terminal
(use-package base16-theme
  :ensure t
  :init
  (when is-terminal
    (setq base16-theme-256-color-source 'colors))
  :config
  ;; My favorite themes, in order. :)
  (load-theme 'base16-hopscotch t)
  ;; (load-theme 'base16-darktooth t)
  ;; (load-theme 'base16-gruvbox-dark-soft t)
  ;; (load-theme 'base16-spacemacs t)
  ;; (load-theme 'base16-monokai t)
  ;; (load-theme 'base16-woodland t)
  ;; (load-theme 'base16-paraiso t)
  )

;;
;; text-mode
;;

;; Wrap words in text mode please.
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;
;; clang-format
;;

(defun my-clang-format-buffer ()
  "Reformat buffer if .clang-format exists in the projectile root."
  (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
    (clang-format-buffer)))

(let ((path "~/bin/clang-format.el"))
  (when (f-exists? path)
    ;; Load the package.
    (load path)

    ;; Format on save for c / c++.
    (-map
     (lambda (x)
       (add-hook x (lambda () (add-hook 'before-save-hook #'my-clang-format-buffer nil 'local))))
     '(c-mode-hook c++-mode-hook))))

;;
;; hydra
;;

(use-package hydra
  :ensure t)

;;
;; uniquify
;;

(require 'uniquify)

;; Customize the look of duplicate values.
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;;
;; wgrep
;;

;; Makes grep buffers editable.
(use-package wgrep
  :ensure t
  :init
  (progn
    ;; Save modified buffers when you exit.
    (setq wgrep-auto-save-buffer t)))

;;
;; rg
;;

(use-package rg
  :ensure t)

;;
;; expand-region
;;

(use-package expand-region
  :ensure t
  :config
  (progn
    ;; The hydra.
    (defhydra hydra-marking
      (:columns 3)
      "marking"

      ;; Words.
      ("w" er/mark-word "mark-word")

      ;; Symbols.
      ("m" er/mark-symbol "mark-symbol")
      ("M" er/mark-symbol-with-prefix "mark-symbol-with-prefix")

      ;; Accessors.
      ("a" er/mark-next-accessor "mark-next-accessor")

      ;; Invocations (method calls).
      ("i" er/mark-method-call "mark-method-call")

      ;; Comments.
      ("c" er/mark-comment "mark-comment")

      ;; Strings.
      ("<" er/mark-inside-quotes "mark-inside-quotes")
      (">" er/mark-outside-quotes "mark-outside-quotes")

      ;; Delimiters.
      ("(" er/mark-outside-pairs "mark-outside-pairs")
      (")" er/mark-inside-pairs "mark-inside-pairs")

      ;; Special.
      ("u" er/mark-url "mark-url")
      ("e" er/mark-email "mark-email")

      ;; Functions.
      ("d" er/mark-defun "mark-defun")

      ;; Expand / contract / reset region.
      ("k" er/expand-region "expand-region")
      ("j" er/contract-region "contract-region")
      ("0" er/reset-region "reset-region")

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; This should always be bound.
    (global-set-key (kbd "C-c m") 'hydra-marking/body)))

(defun er/reset-region ()
  "Resets any marked region."
  (interactive)
  (er/contract-region 0))

;;
;; rainbow-delimiters
;;

(use-package rainbow-delimiters
  :ensure t)

;; Turn this on for Emacs lisp.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;
;; paredit
;;

(use-package paredit
  :ensure t
  :init
  (progn
    ;; Turn it on for all lisp modes.
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))
  :config
  (progn
    ;; Paredit hijacks C-j in lisp-interaction-mode, so fix that.
    (define-key lisp-interaction-mode-map [remap paredit-newline] #'eval-print-last-sexp)

    ;; The hydra.
    (defhydra hydra-paredit
      (:columns 3)
      "paredit"

      ;; Forward / backward.
      ("f" paredit-forward "forward")
      ("b" paredit-backward "backward")

      ;; Down / up.
      ("d" paredit-forward-down "forward-down")
      ("u" paredit-backward-up "backward-up")

      ;; Down / up (backwards).
      ("D" paredit-backward-down "backward-down")
      ("U" paredit-forward-up "forward-up")

      ;; Next / previous.
      ("n" paredit-next-sexp "next-sexp")
      ("p" paredit-previous-sexp "previous-sexp")

      ;; Beginning / end.
      ("a" paredit-beginning-sexp "beginning-sexp")
      ("e" paredit-end-sexp "end-sexp")

      ;; Slurping / barfing.
      ("(" paredit-backward-slurp-sexp "backward-slurp-sexp")
      (")" paredit-forward-slurp-sexp "forward-slurp-sexp")
      (">" paredit-backward-barf-sexp "backward-barf-sexp")
      ("<" paredit-forward-barf-sexp "forward-barf-sexp")

      ;; Wrapping.
      ("w(" paredit-wrap-round "wrap-round")
      ("w[" paredit-wrap-square "wrap-square")
      ("w{" paredit-wrap-curly "wrap-curly")
      ("w<" paredit-wrap-angled "wrap-angled")
      ("w\"" paredit-meta-doublequote "meta-doublequote")

      ;; Splicing.
      ("ss" paredit-splice-sexp "splice-sexp")
      ("sf" paredit-splice-sexp-killing-forward "splice-sexp-killing-forward")
      ("sb" paredit-splice-sexp-killing-backward "splice-sexp-killing-backward")

      ;; Split / join.
      ("S" paredit-split-sexp "split-sexp")
      ("J" paredit-join-sexps "join-sexps")

      ;; Raise.
      ("r" paredit-raise-sexp "raise-sexp")

      ;; Convolute.
      ("c" paredit-convolute-sexp "convolute-sexp")

      ;; Reindent.
      ("i" paredit-reindent-defun "reindent-defun")

      ;; Comment.
      (";" paredit-comment-dwim "comment-dwim")

      ;; Transpose forward / backward.
      ("M-n" paxedit-transpose-forward "transpose-forward")
      ("M-p" paxedit-transpose-backward "transpose-backward")

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; Don't pollute the C-c keymap.
    (define-key paredit-mode-map (kbd "C-c C-M-l") nil)

    ;; Bind when paredit is active.
    (define-key paredit-mode-map (kbd "C-c j") 'hydra-paredit/body)))

(defun inside-sexp-p ()
  "Returns true if point is on a sexp."
  (let ((b (bounds-of-thing-at-point 'sexp)))
    (and b (>= (point) (car b)) (< (point) (cdr b)))))

(defun paredit-next-sexp ()
  "Navigate to the beginning of the next sexp."
  (interactive)
  (when (inside-sexp-p)
    (paredit-forward))
  (paredit-forward)
  (paredit-backward))

(defun paredit-previous-sexp ()
  "Navigate to the beginning of the previous sexp."
  (interactive)
  (paredit-backward))

(defun paredit-beginning-sexp ()
  "Navigate to the beginning of the current sexp."
  (interactive)
  (paredit-backward-up)
  (paredit-forward-down))

(defun paredit-end-sexp ()
  "Navigate to the end of the current sexp."
  (interactive)
  (paredit-forward-up)
  (paredit-backward-down))

;;
;; paxedit
;;

(use-package paxedit
  :ensure t
  :init
  (progn
    ;; Turn it on for all lisp modes.
    (add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
    (add-hook 'clojure-mode-hook 'paxedit-mode)
    (add-hook 'clojurescript-mode-hook 'paxedit-mode)
    (add-hook 'clojurec-mode-hook 'paxedit-mode)
    (add-hook 'cider-repl-mode-hook 'paxedit-mode)))

;;
;; company
;;

;; This mode enables make-
;; believe intellisense.
(use-package company
  :ensure t
  :init
  (progn
    ;; No delay please.
    (setq company-idle-delay 0)

	;; Start completing after a single character.
	(setq company-minimum-prefix-length 1)

	;; Align fields in completions.
	(setq company-tooltip-align-annotations t)

    ;; Turn company on globally.
    (add-hook 'after-init-hook 'global-company-mode)))

;;
;; which-key
;;

;; The replacement for guide-key. Given a key
;; sequence, shows what commands are available.
(use-package which-key
  :ensure t
  :config
  (progn
    ;; No delay please.
    (setq which-key-idle-delay 0.0)

    ;; Show count / total on the modeline.
    (setq which-key-show-remaining-keys t)

    ;; Allow 50% of the frame to display keys.
    (setq which-key-side-window-max-height 0.5)

    ;; Open it at the bottom.
    (which-key-setup-side-window-bottom)

    ;; Turn it on.
    (which-key-mode)))

;;
;; projectile
;;

(use-package projectile
  :ensure t
  :demand t
  :init
  (progn
    ;; Add a global prefix.
    (global-set-key (kbd "C-c p") 'projectile-command-map)

    ;; Setup the completion system.
    (setq projectile-completion-system completion-system)

    ;; Set our indexing mode.
    (setq projectile-indexing-method 'alien))
  :config
  (progn
    ;; Turn projectile on globally.
    (projectile-mode)))

;;
;; helm
;;

;; TODO
;; * Customize this package.
;; * You should probably map helm-occur
;;   to something that's easier to type.
;; * Bind the various ag commands to
;;   useful keys, you use them a lot.

;; The best completion package ever in my
;; humble opinion. The initialization order
;; is important here due to some global key
;; bindings - we start with the config.
(when use-helm
  (use-package helm
    :ensure t
    :demand t
    :bind
    (("C-x C-f" . helm-find-files)
     ("M-x" . helm-M-x)
     ("C-c n" . helm-M-x)
     ("C-x b" . helm-mini)
     ("C-x C-b" . helm-buffers-list)
     ("M-y" . helm-show-kill-ring)
     ("C-h a" . helm-apropos)
     ("C-x p" . helm-browse-project))
    :init
    (progn
      ;; We need this now.
      (use-package helm-config)

      ;; Open helm in the current window.
      (setq helm-split-window-in-side-p t)

      ;; Stop annoying keybindings.
      (global-unset-key (kbd "C-x c"))

      ;; Add a global prefix.
      (global-set-key (kbd "C-c h") 'helm-command-prefix)

      ;; Up this limit a bit.
      (setq helm-candidate-number-limit 400)

      ;; Add a few extensions to helm's command map.
      ;; (define-key helm-command-map (kbd "d") 'helm-dash)
      ;; (define-key helm-command-map (kbd "o") 'helm-occur)
      )
    :config
    (progn
      ;; Turn it on.
      (helm-mode t)

      ;; Turn on helm-follow for certain sources.
      (add-hook
       'helm-before-initialize-hook
       (lambda ()
         (let ((sources '(helm-source-occur)))
           (mapc (lambda (source)
                   (when (memq source sources)
                     (helm-attrset 'follow 1 (symbol-value source))))
                 helm-sources))))

      ;; Makes helm-grep buffers editable.
      (use-package wgrep-helm
        :ensure t)

      ;; For spell checking.
      (use-package helm-flyspell
        :ensure t)

      ;; Swoop mode ftw.
      (use-package helm-swoop
        :ensure t
        :bind (("M-i" . helm-swoop))
        :init
        (progn
          ;; Start with no search string.
          (setq helm-swoop-pre-input-function (lambda () ""))

          ;; Split vertically please.
          (setq helm-swoop-split-direction 'split-window-horizontally))
        :config
        (progn
          ;; Move up and down using isearch keys.
          (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
          (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
          (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
          (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)))

      ;; For helm git integration.
      (use-package helm-ls-git
        :ensure t)

      ;; Helm integration? Yes please!
      (use-package helm-projectile
        :ensure t
        :config
        (progn
          ;; Turn it on.
          (helm-projectile-toggle 1)))

      ;; For helm rg integration.
      (use-package helm-rg
        :ensure t)

      ;; TODO: Set this up some more using this links:
      ;; https://github.com/tuhdo/emacs-c-ide-demo/blob/master/custom/setup-helm-gtags.el
      ;; https://www.emacswiki.org/emacs/GnuGlobal
      ;; https://gist.github.com/dkruchinin/925042
      ;; For navigating tags.
      ;; (use-package helm-gtags
      ;;   :ensure t
      ;;   :config
      ;;   (progn
      ;;
      ;;     ;; Use this for modes that gnu global supports.
      ;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
      ;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)
      ;;     (add-hook 'csharp-mode-hook 'helm-gtags-mode)
      ;;
      ;;     ;; Setup key bindings.
      ;;     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
      ;;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

      ;; For inspecting bindings.
      (use-package helm-descbinds
        :ensure t
        :bind (("C-h b" . helm-descbinds))
        :config
        (progn
          ;; Open in the other window please.
          (setq helm-descbinds-window-style 'split-window)))))

  (defun helm-grep-do-git-grep-all ()
    "Like helm-grep-do-git-grep but searches the entire repository."
    (interactive)
    (helm-grep-git-1 default-directory 'all))

  (global-set-key (kbd "C-c M-i") 'helm-grep-do-git-grep-all))

;;
;; ivy
;;

(when use-ivy
;;   (defun ivy-display-function-other-window (text)
;;     "Show ivy results in other window."
;;     (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
;;           (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
;;                  (let ((point (point))
;;                        (string (concat (buffer-string) "  " text)))
;;                    (add-face-text-property
;;                     (- point 1) point 'ivy-cursor t string)
;;                    string))))
;;       (with-current-buffer buffer
;;         (let ((inhibit-read-only t))
;;           (erase-buffer)
;;           (insert str)))
;;       (with-ivy-window
;;         (display-buffer
;;          buffer
;;          `((display-buffer-reuse-window
;;             (lambda (buffer alist)
;;               (other-window 1)
;;               (switch-to-buffer buffer))))))))

;;   (defun my-swiper (&optional initial-input)
;;     "Opens swiper in a vertical buffer."
;;     (interactive)
;;     (let ((position (point)))
;;       (save-window-excursion
;;         (let* ((buffer (current-buffer))
;;                (window (get-buffer-window))
;;                (should-swap
;;                 (and
;;                  (not (window-at-side-p window 'left))
;;                  (window-at-side-p window 'right)))
;;                (ivy-height (1- (window-height window))))
;;           (delete-other-windows)
;;           (split-window-horizontally)
;;           (when should-swap
;;             (other-window 1)
;;             (switch-to-buffer buffer))
;;           (setq position (swiper initial-input))))
;;       (goto-char position)))

  (use-package counsel
    :ensure t
    :demand t
    :bind
    (("C-s" . swiper-isearch)
     ("M-x" . counsel-M-x)
     ("C-c n" . counsel-M-x)
     ("C-x C-f" . counsel-find-file)
     ("M-y" . counsel-yank-pop)
     ("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)
     ("C-x b" . counsel-switch-buffer)
     ("C-h a" . counsel-apropos)
     ("C-h b" . counsel-descbinds)
     ;; ("M-i" . my-swiper)
     ("M-i" . swiper)
     ("C-c M-i" . counsel-git-grep)
     ("C-x p" . counsel-git))
    :init
    ;; (setq ivy-display-functions-alist
    ;;       '((swiper . ivy-display-function-other-window)))

    ;; Show recent files / bookmarks.
    (setq ivy-use-virtual-buffers t)

    ;; Show the count format.
    (setq ivy-count-format "(%d/%d) ")

    ;; Make highlight extend all the way to the right.
    (setq ivy-format-function 'ivy-format-function-line)

    ;; Increase the height.
    (setq ivy-height 18)

    ;; Always stay the same height.
    (setq ivy-fixed-height-minibuffer t)

    ;; Visually separate things in the kill ring.
    (setq counsel-yank-pop-separator
          (concat "\n" (make-string 24 ?-) "\n"))

    ;; Ignore order in completing read.
    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order)))
    :config
    ;; Customize the default input for various commands.
    ;; (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

    ;; Enable everywhere.
    (ivy-mode 1))

  (use-package ivy-hydra
    :ensure t
    :after (ivy hydra))

  (use-package ivy-rich
    :ensure t
    :after ivy
    :config
    (ivy-rich-mode 1)

    ;; Highlight the entire line in the minibuffer.
    (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line))

  (use-package lsp-ivy
    :ensure t
    :after ivy)

  ;; (use-package counsel-projectile
  ;;   :after (ivy counsel projectile))
  )

;;
;; magit
;;

;; TODO
;; * Open the status buffer in the current window.
;; * Add a keymap for magit so we get which-key
;;   support.
;; * Customize this package.

;; The best git interface ever.
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (progn
    ;; Turn these off, they're ugly.
    (setq magit-section-visibility-indicator nil))
  :config
  (progn
    ;; Open the status buffer in the current window and select it.
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)))

;;
;; multiple-cursors
;;

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    ;; The hydra.
    (defhydra hydra-multiple-cursors
      (:columns 3)
      "multiple-cursors"

      ;; mc/mark-pop
      ;; mc/mmlte--up
      ("l" mc/edit-lines "edit-lines" :exit t)
      ;; mc/mmlte--left
      ;; mc/mmlte--down
      ;; mc/mmlte--right
      ;; mc/sort-regions
      ;; mc/mark-all-dwim
      ;; mc/cycle-forward
      ;; mc/keyboard-quit
      ;; mc/cycle-backward
      ;; mc/vertical-align
      ;; mc/insert-letters
      ;; mc/repeat-command
      ;; mc/insert-numbers
      ;; mc/reverse-regions
      ;; mc/mark-next-lines
      ;; mc/edit-ends-of-lines
      ("a" mc/mark-all-like-this "mark-all-like-this" :exit t)
      ("r" mc/mark-all-in-region "mark-all-in-region" :exit t)
      ;; mc/mark-sgml-tag-pair
      ;; mc/mark-previous-lines
      ("n" mc/mark-next-like-this "mark-next-like-this")
      ;; mc/add-cursor-on-click
      ("M-n" mc/unmark-next-like-this "unmark-next-like-this")
      ;; mc/toggle-cursor-on-click
      ("N" mc/skip-to-next-like-this "skip-to-next-like-this")
      ;; mc/mark-all-like-this-dwim
      ("p" mc/mark-previous-like-this "mark-previous-like-this")
      ;; mc/mark-all-words-like-this
      ;; mc/mark-next-word-like-this
      ;; mc/mark-next-like-this-word
      ;; mc/edit-beginnings-of-lines
      ;; mc/vertical-align-with-space
      ("M-p" mc/unmark-previous-like-this "unmark-previous-like-this")
      ("R" mc/mark-all-in-region-regexp "mark-all-in-region-regexp" :exit t)
      ("P" mc/skip-to-previous-like-this "skip-to-previous-like-this")
      ;; mc/mark-all-symbols-like-this
      ;; mc/mark-next-symbol-like-this
      ;; mc/mark-next-like-this-symbol
      ;; mc/mark-all-like-this-in-defun
      ;; mc/mark-previous-word-like-this
      ;; mc/mark-previous-like-this-word
      ;; mc/mark-more-like-this-extended
      ;; mc/mark-previous-symbol-like-this
      ;; mc/mark-previous-like-this-symbol
      ;; mc/mark-all-words-like-this-in-defun
      ;; mc/mark-all-symbols-like-this-in-defun

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; This should always be bound.
    (global-set-key (kbd "C-c l") 'hydra-multiple-cursors/body)))

;;
;; rainbow-mode
;;

(use-package rainbow-mode
  :ensure t
  :config
  (progn
    ;; Only style hex colors please.
    (setq rainbow-ansi-colors nil)
    (setq rainbow-latex-colors nil)
    (setq rainbow-html-colors nil)
    (setq rainbow-x-colors nil)))

;;
;; undo-tree
;;

(use-package undo-tree
  :ensure t
  :bind (("C-x u" . undo-tree-visualize))
  :config
  (progn
    ;; Let's use this everywhere.
    (global-undo-tree-mode)))

;;
;; try
;;

;; Allows you to try packages
;; without installing them.
(use-package try
  :ensure t
  :commands (try try-and-refresh))

;;
;; csharp-mode
;;

;; The c# language.
(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :init
  (progn
    ;; Use electric pair mode.
    (electric-pair-mode 1)))

;;
;; protobuf-mode
;;

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto")

;;
;; yasnippet
;;

(use-package yasnippet
  :ensure t)

;;
;; flycheck
;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;
;; lsp-mode
;;

(use-package lsp-mode
  :ensure t
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :bind-keymap ("C-c k" . lsp-command-map)
  :bind (("C-c M-u" . lsp-find-references)
         ("C-c M-y" . lsp-find-implementation)
         ("C-c M-." . xref-find-definitions-other-window)
         ("S-<f6>" . lsp-rename)
         )
  :commands (lsp lsp-deferred)
  :init
  (progn
    ;; Set the which-key prefix.
    (setq lsp-keymap-prefix "C-c k")

    ;; Use the correct completion provider.
    (setq lsp-completion-provider :capf)

    ;; Reduce the idle delay.
    (setq lsp-idle-delay 0.0)

    ;; Increase the amount of data emacs reads from the process.
    (setq read-process-output-max (* 1024 1024))

    ;; Turn off file watching for now.
    (setq lsp-enable-file-watchers nil)

    ;; Disable lsp logging.
    (setq lsp-log-io nil)

    ;; Don't use breadcrumbs please.
    ;; (setq lsp-headerline-breadcrumb-enable nil)

    ;; Turn on highlighting please.
    (setq lsp-semantic-tokens-enable t)

    ;; Don't enable code lens.
    (setq lsp-lens-enable nil))
  :config
  (progn
    ;; Let's add helm support please.
    (use-package helm-lsp
      :disabled
      :ensure t
	  :commands helm-lsp-workspace-symbol)))

;;
;; lua-mode
;;

(use-package lua-mode
  :ensure t
  :mode "\\.lua$")

;;
;; rust
;;

(use-package rustic
  :ensure t
  :hook (rustic-mode . yas-minor-mode)
  :bind
  (:map
   rustic-mode-map
   ("M-j" . lsp-ui-imenu)
   ("M-?" . lsp-find-references)
   ("C-c C-c l" . flycheck-list-errors)
   ("C-c C-c a" . lsp-execute-code-action)
   ("C-c C-c r" . lsp-rename)
   ("C-c C-c q" . lsp-workspace-restart)
   ("C-c C-c Q" . lsp-workspace-shutdown)
   ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;
;; shader-mode
;;

;; For editing shaders.
(use-package shader-mode
  :ensure t
  :mode ("\\.shader$"
         "\\.compute$"
         "\\.hlsl$"
         "\\.glsl$"
         "\\.cg$"
         "\\.cginc$"))

;;
;; slime
;;

(use-package slime
  :ensure t
  :init
  (progn
    ;; We're using SBCL as our CL runtime.
    (setq inferior-lisp-program "/usr/local/bin/sbcl")

    ;; Load the most popular contribs.
    (setq slime-contribs '(slime-fancy))))

;;
;; clojure-mode
;;

;; The clojure language.
(use-package clojure-mode
  :disabled
  :ensure t
  :mode "\\.clj$"
  :hook (clojure-mode . rainbow-delimiters-mode)

  :init
  (progn
    ;; Align let bindings / maps / etc.
    (setq clojure-align-forms-automatically t))
  :config
  (progn
    ;; The hydra.
    ;; (defhydra hydra-clojure
    ;;   (:columns 3)
    ;;   "clojure"

    ;;   ;; Cancel.
    ;;   ("q" nil "quit" :exit t))

    ;; clojure-mode
    ;; clojure-align
    ;; clojurec-mode
    ;; clojure-unwind
    ;; clojure-thread
    ;; clojure-sort-ns
    ;; clojure-cycle-if
    ;; clojure-update-ns
    ;; clojure-mode-menu
    ;; clojure-cycle-not
    ;; clojure-view-guide
    ;; clojure-unwind-all
    ;; clojurescript-mode
    ;; clojure-cycle-when
    ;; clojure-move-to-let
    ;; clojure-cycle-privacy
    ;; clojure-introduce-let
    ;; clojure-view-grimoire
    ;; clojure-insert-ns-form
    ;; clojure-view-cheatsheet
    ;; clojure-mode-report-bug
    ;; clojure-thread-last-all
    ;; clojure-thread-first-all
    ;; clojure-view-style-guide
    ;; clojure-mode-display-version
    ;; clojure-forward-logical-sexp
    ;; clojure-backward-logical-sexp
    ;; clojure-toggle-keyword-string
    ;; clojure-view-reference-section
    ;; clojure-let-forward-slurp-sexp
    ;; clojure-insert-ns-form-at-point
    ;; clojure-let-backward-slurp-sexp
    ;; clojure-convert-collection-to-map
    ;; clojure-convert-collection-to-set
    ;; clojure-convert-collection-to-list
    ;; clojure-convert-collection-to-vector
    ;; clojure-convert-collection-to-quoted-list

    ;; Clear the C-c keymaps.
    ;; (define-key clojure-mode-map (kbd "C-c") nil)

    ;; Bind when cider is active.
    ;; (define-key clojure-mode-map (kbd "C-c y") 'hydra-clojure/body)
    ))

;;
;; cider
;;

;; This is -the- clojure ide.
(use-package cider
  :disabled
  :ensure t
  :init
  (progn
    ;; Make the scratch buffer empty.
    (setq cider-scratch-initial-message "")

    ;; Colorize usages of functions and variables from all namespaces.
    (setq cider-font-lock-dynamically '(macro core function var)))
  :config
  (progn
    ;; Helm integration? Yes please!
    (use-package helm-cider
      :disabled
      :ensure t
      :init
      (progn
        ;; Don't snap to the bottom.
        (setq cider-repl-scroll-on-output nil))
      :config
      (helm-cider-mode 1))

    ;; Refactoring support.
    (use-package clj-refactor
      :ensure t)

    ;; The docs hydra.
    (defhydra hydra-cider-docs
      (:columns 3)
      "cider docs"

      ("a" cider-apropos "apropos" :exit t)
      ("d" cider-apropos-documentation "apropos-documentation" :exit t)
      ("j" cider-javadoc "javadoc" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; The eval hydra.
    (defhydra hydra-cider-eval
      (:columns 3)
      "cider eval"

      ("e" cider-eval-last-sexp "eval-last-sexp" :exit t)
      ("E" cider-eval-last-sexp-and-replace "eval-last-sexp-and-replace" :exit t)
      ("r" cider-eval-last-sexp-to-repl "eval-last-sexp-to-repl" :exit t)
      ("k" cider-eval-defun-at-point "eval-defun-at-point" :exit t)
      ("d" cider-eval-defun-to-point "eval-defun-to-point")
      ("K" cider-debug-defun-at-point "debug-defun-at-point" :exit t)
      ("s" cider-eval-sexp-at-point "eval-sexp-at-point" :exit t)
      ("R" cider-eval-region "eval-region" :exit t)
      ("i" cider-eval-ns-form "eval-ns-form" :exit t)
      ("n" cider-load-buffer "load-buffer" :exit t)
      ("N" cider-load-buffer-and-switch-to-repl-buffer "load-buffer-and-switch-to-repl-buffer" :exit t)
      ("y" cider-repl-set-ns "repl-set-ns" :exit t)
      ("f" cider-load-file "load-file" :exit t)
      ("a" cider-load-all-files "load-all-files" :exit t)
      ("mm" cider-macroexpand-1 "macroexpand-1" :exit t)
      ("ma" cider-macroexpand-all "macroexpand-all" :exit t)
      ("pe" cider-pprint-eval-last-sexp "pprint-eval-last-sexp" :exit t)
      ("pk" cider-pprint-eval-defun-at-point "pprint-eval-defun-at-point" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; The find hydra.
    (defhydra hydra-cider-find
      (:columns 3)
      "cider find"

      ("n" cider-find-ns "find-ns" :exit t)
      ("v" cider-find-var "find-var" :exit t)
      ("d" cider-find-dwim "find-dwim" :exit t)
      ("k" cider-find-keyword "find-keyword" :exit t)
      ("r" cider-find-resource "find-resource" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; The test hydra.
    (defhydra hydra-cider-test
      (:columns 3)
      "cider test"

      ("t" cider-test-run-test "test-run-test" :exit t)
      ("r" cider-test-rerun-test "test-rerun-test" :exit t)
      ("n" cider-test-run-ns-tests "test-run-ns-tests" :exit t)
      ("l" cider-test-run-loaded-tests "test-run-loaded-tests" :exit t)
      ("p" cider-test-run-project-tests "test-run-project-tests" :exit t)
      ("f" cider-test-rerun-failed-tests "test-rerun-failed-tests" :exit t)
      ("r" cider-test-show-report "test-show-report" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    ;; ;; The hydra.
    ;; (defhydra hydra-cider
    ;;   (:columns 3)
    ;;   "cider"

    ;;   ;; Cancel.
    ;;   ("q" nil "quit" :exit t))

    ;; complete-symbol
    ;; cider-run
    ;; cider-ping
    ;; cider-quit
    ;; cider-mode
    ;; cider-undef
    ;; cider-refresh
    ;; cider-scratch
    ;; cider-version
    ;; cider-jack-in
    ;; cider-inspect
    ;; cider-restart
    ;; cider-connect
    ;; cider-pop-back
    ;; cider-grimoire
    ;; cider-repl-tab
    ;; cider-selector
    ;; cider-interrupt
    ;; cider-repl-mode
    ;; cider-eval-file
    ;; cider-test-jump
    ;; cider-browse-ns
    ;; cider-classpath
    ;; cider-report-bug
    ;; cider-cheatsheet
    ;; cider-test-ediff
    ;; cider--debug-mode
    ;; cider-drink-a-sip
    ;; cider-repl-return
    ;; cider-eval-buffer
    ;; cider-view-manual
    ;; cider-browse-spec
    ;; cider-view-refcard
    ;; cider-format-defun
    ;; cider-docview-mode
    ;; cider-inspect-expr
    ;; cider-repl-history
    ;; cider-grimoire-web
    ;; cider-format-buffer
    ;; cider-browse-ns-all
    ;; cider-repl-set-type
    ;; cider-read-and-eval
    ;; cider-repl-bol-mark
    ;; cider-profile-clear
    ;; cider-format-region
    ;; cider-inspector-pop
    ;; cider-profile-toggle
    ;; cider-repl-mode-menu
    ;; cider-browse-ns-mode
    ;; cider-auto-test-mode
    ;; cider-enlighten-mode
    ;; cider-eval-all-files
    ;; cider-docview-source
    ;; cider-mode-menu-open
    ;; cider-inspector-mode
    ;; cider-repl-kill-input
    ;; cider-debug-move-here
    ;; cider-repl-next-input
    ;; cider-toggle-trace-ns
    ;; cider-profile-samples
    ;; cider-repl-set-config
    ;; cider-stacktrace-jump
    ;; cider-stacktrace-mode
    ;; cider-docview-javadoc
    ;; cider-profile-summary
    ;; cider-test-stacktrace
    ;; cider-debug-mode-menu
    ;; cider-browse-spec-all
    ;; cider--doc-make-xrefs
    ;; cider-test-report-mode
    ;; cider-repl-next-prompt
    ;; cider-browse-spec-mode
    ;; cider-toggle-trace-var
    ;; cider-docview-grimoire
    ;; cider-test-next-result
    ;; cider-repl-clear-output
    ;; cider-repl-history-quit
    ;; cider-format-edn-region
    ;; cider-repl-history-mode
    ;; cider-profile-ns-toggle
    ;; cider-popup-buffer-quit
    ;; cider-repl-history-save
    ;; cider-docview-mode-menu
    ;; cider-popup-buffer-mode
    ;; cider-inspector-refresh
    ;; cider-repl-end-of-defun
    ;; cider-format-edn-buffer
    ;; cider-inspect-last-sexp
    ;; cider-repl-history-load
    ;; cider-repl-clear-buffer
    ;; cider-test-ediff-cleanup
    ;; cider-connection-browser
    ;; cider-repl-forward-input
    ;; cider-repl-clear-banners
    ;; cider-visit-error-buffer
    ;; cider-repl-history-occur
    ;; cider-repl-closing-return
    ;; cider-repl-history-update
    ;; cider-debug-toggle-locals
    ;; cider-inspect-last-result
    ;; cider-inspector-next-page
    ;; cider-load-all-project-ns
    ;; cider-repl-previous-input
    ;; cider-close-nrepl-session
    ;; cider-mode-eval-menu-open
    ;; cider-profile-var-summary
    ;; cider-browse-ns-mode-menu
    ;; cider-repl-shortcuts-help
    ;; cider-inspector-prev-page
    ;; cider-repl-backward-input
    ;; cider-open-classpath-entry
    ;; cider-docview-grimoire-web
    ;; cider-debug-defun-at-point
    ;; cider-test-previous-result
    ;; cider-eval-print-last-sexp
    ;; cider-repl-history-forward
    ;; cider-insert-defun-in-repl
    ;; cider-replicate-connection
    ;; cider-repl-previous-prompt
    ;; cider-repl-handle-shortcut
    ;; cider-repl-switch-to-other
    ;; cider-stacktrace-mode-menu
    ;; cider-insert-region-in-repl
    ;; cider-debug-mode-send-reply
    ;; cider-connect-clojurescript
    ;; cider-stacktrace-next-cause
    ;; cider-stacktrace-toggle-all
    ;; cider-browse-spec-view-mode
    ;; cider-repl-history-previous
    ;; cider-test-report-mode-menu
    ;; cider-test-clear-highlights
    ;; cider-eval-defun-to-comment
    ;; cider-jack-in-clojurescript
    ;; cider-stacktrace-toggle-clj
    ;; cider-switch-to-repl-buffer
    ;; cider-browse-ns-doc-at-point
    ;; cider-browse-spec--browse-at
    ;; cider-browse-ns-handle-mouse
    ;; cider-inspect-defun-at-point
    ;; cider-clojure-mode-menu-open
    ;; cider-stacktrace-toggle-java
    ;; cider-insert-ns-form-in-repl
    ;; cider-stacktrace-toggle-repl
    ;; cider-profile-var-profiled-p
    ;; cider-find-dwim-other-window
    ;; cider-describe-nrepl-session
    ;; cider-repl-clear-help-banner
    ;; cider-repl-require-repl-utils
    ;; cider-display-connection-info
    ;; cider-repl-beginning-of-defun
    ;; cider-toggle-request-dispatch
    ;; cider-completion-flush-caches
    ;; cider-close-ancillary-buffers
    ;; cider-repl-newline-and-indent
    ;; cider-inspector-set-page-size
    ;; cider-browse-ns-find-at-point
    ;; cider-connections-buffer-mode
    ;; cider-make-connection-default
    ;; cider-jump-to-locref-at-point
    ;; cider-insert-last-sexp-in-repl
    ;; cider-browse-instrumented-defs
    ;; cider-stacktrace-cycle-cause-1
    ;; cider-stacktrace-cycle-cause-2
    ;; cider-stacktrace-cycle-cause-3
    ;; cider-stacktrace-cycle-cause-4
    ;; cider-stacktrace-cycle-cause-5
    ;; cider-create-sibling-cljs-repl
    ;; cider-repl-next-matching-input
    ;; cider-browse-spec-example-mode
    ;; cider-connections-make-default
    ;; cider-toggle-buffer-connection
    ;; cider-refresh-dynamic-font-lock
    ;; cider-stacktrace-previous-cause
    ;; cider-stacktrace-toggle-tooling
    ;; cider-rotate-default-connection
    ;; cider-jump-to-compilation-error
    ;; cider-repl-history-mouse-insert
    ;; cider--connections-make-default
    ;; cider-eval-last-sexp-in-context
    ;; cider-repl-history-clear-preview
    ;; cider-change-buffers-designation
    ;; cider-popup-buffer-quit-function
    ;; cider-browse-ns-operate-at-point
    ;; cider-inspector-operate-on-click
    ;; cider-find-and-clear-repl-output
    ;; cider-inspector-operate-on-point
    ;; cider-repl-toggle-pretty-printing
    ;; cider-mode-interactions-menu-open
    ;; cider-repl-history-search-forward
    ;; cider-stacktrace-cycle-all-causes
    ;; cider-connections-goto-connection
    ;; cider-read-and-eval-defun-at-point
    ;; cider-connections-close-connection
    ;; cider-repl-history-search-backward
    ;; cider-stacktrace-toggle-duplicates
    ;; cider-assoc-buffer-with-connection
    ;; cider-clear-compilation-highlights
    ;; cider-pprint-eval-defun-to-comment
    ;; cider-repl-history-insert-and-quit
    ;; cider-stacktrace-show-only-project
    ;; cider-repl-previous-matching-input
    ;; cider-pprint-eval-last-sexp-to-repl
    ;; cider--calculate-opening-delimiters
    ;; cider-eval-sexp-at-point-in-context
    ;; cider-assoc-project-with-connection
    ;; cider-switch-to-last-clojure-buffer
    ;; cider-clear-buffer-local-connection
    ;; cider-repl-history-undo-other-window
    ;; cider-stacktrace-cycle-current-cause
    ;; cider-repl-indent-and-complete-symbol
    ;; cider-pprint-eval-last-sexp-to-comment
    ;; cider-inspector-next-inspectable-object
    ;; cider-enable-on-existing-clojure-buffers
    ;; cider-disable-on-existing-clojure-buffers
    ;; cider-browse-spec--print-curr-spec-example
    ;; cider-inspector-previous-inspectable-object
    ;; helm-cider-mode
    ;; helm-cider-spec
    ;; helm-cider-spec-ns
    ;; helm-cider-apropos
    ;; helm-cider-cheatsheet
    ;; helm-cider-apropos-ns
    ;; helm-cider-spec-symbol
    ;; helm-cider-repl-history
    ;; helm-cider-apropos-symbol
    ;; helm-cider-apropos-symbol-doc

    ;; Clear the C-c keymaps.
    (define-key clojure-mode-map (kbd "C-c") nil)
    (define-key cider-mode-map (kbd "C-c") nil)
    (define-key cider-repl-mode-map (kbd "C-c") nil)

    ;; Only enable these in cider mode.
    (define-key cider-mode-map (kbd "C-c k") 'hydra-cider-eval/body)

    ;; Method to add common cider hydras.
    (-each (list cider-mode-map
                 cider-repl-mode-map)
      (lambda (x)
        ;; (define-key x (kbd "C-c l") 'hydra-cider/body)
        (define-key x (kbd "C-c f") 'hydra-cider-find/body)
        (define-key x (kbd "C-c u") 'hydra-cider-docs/body)
        (define-key x (kbd "C-c t") 'hydra-cider-test/body)))
    ))

;;
;; yaml-mode
;;

;; The yaml language.
(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

;;
;; web-mode
;;

;; Follow conventions please.
(setq js-indent-level 2)

;; For web-based languages.
(use-package web-mode
  :ensure t
  :mode ("\\.js$"
         "\\.jsx$"
         "\\.json$"
         "\\.css$"
         "\\.scss$"
         "\\.html"
         "\\.php")
  :init
  (progn
    ;; Make sure we see embedded jsx code in js files.
    (setq web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))

    ;; Indenting offsets.
    (add-hook
     'web-mode-hook
     (lambda ()
       (setq web-mode-markup-indent-offset 4)
       (setq web-mode-css-indent-offset 4)
       (setq web-mode-code-indent-offset 4)))))

;;
;; go-mode
;;

(use-package go-mode
  :ensure t
  :mode "\\.go$"
  :hook ((go-mode . lsp-deferred)
		 (go-mode . yas-minor-mode)
         (go-mode . toggle-truncate-lines)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (progn
    ;; Use electric pair mode.
    (electric-pair-mode 1)

    ;; Use this for running tests.
    (use-package gotest
      :ensure t)

    ;; The hydra for go tests.
    (defhydra hydra-go-test
      (:columns 3)
      "go-test"

      ("t" go-test-current-test "test-current-test" :exit t)
      ("c" go-test-current-test-cache "test-current-test-cache" :exit t)
      ("f" go-test-current-file "test-current-file" :exit t)
      ("p" go-test-current-project "test-current-project" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    (define-key go-mode-map (kbd "C-c t") 'hydra-go-test/body)))

;;
;; org-mode
;;

;; The style.
(add-hook
 'org-mode-hook
 (lambda ()
   ;; Set a reasonable width.
   (set-fill-column 65)

   ;; Auto fill please.
   (turn-on-auto-fill)

   ;; Only show rightmost stars.
   (org-indent-mode)))

;;
;; haskell-mode
;;

(use-package haskell-mode
  ;; :disabled
  :ensure t
  :mode "\\.hs$"
  :config
  (progn
    ;; Use electric pair mode.
    (electric-pair-mode 1)))

;;
;; markdown-mode
;;

(use-package markdown-mode
  :ensure t
  :mode "\\.md$")

;;
;; eshell
;;

;; TODO
;; * Look into eshell alises, not having "la" sucks.
;; * Check to make sure the initial directory is set.

;; The emacs shell.
(use-package eshell
  :disabled
  :ensure t
  :commands eshell
  :config
  (progn
    ;; We need these.
    (require 's)
    (require 'f)

    ;; History size.
    (setq eshell-history-size 100000)

    ;; Make our prompt look like the terminal.
    (setq
     eshell-prompt-function
     (lambda ()
       (let ((color-yellow "#ffff87")
             (color-green "#00af00")
             (color-red "#d75f5f"))
         (concat
          (propertize "[" 'face `(:foreground ,color-yellow :weight bold))
          (propertize
           (concat
            (user-login-name)
            "@"
            (s-trim (shell-command-to-string "hostname -s")))
           'face `(:foreground ,color-green :weight bold))
          " "
          (propertize
           (f-short (s-trim (shell-command-to-string "pwd")))
           'face `(:foreground ,color-red :weight bold))
          (propertize "]" 'face `(:foreground ,color-yellow :weight bold))
          "\n"
          (propertize "$" 'face `(:foreground ,color-red :weight bold))
          " "))))

    ;; Fix the prompt regex.
    (setq eshell-prompt-regexp "^[$] ")))

;;
;; erc
;;

(use-package erc
  :ensure t
  :init
  (progn
    ;; Fill chat messages based on window width.
    (make-variable-buffer-local 'erc-fill-column)
    (add-hook
     'window-configuration-change-hook
     '(lambda ()
        (save-excursion
          (walk-windows
           (lambda (w)
             (let ((buffer (window-buffer w)))
               (set-buffer buffer)
               (when (eq major-mode 'erc-mode)
                 (setq erc-fill-column (- (window-width w) 2)))))))))

    ;; Let's ignore the notice prefix.
    (setq erc-notice-prefix nil)

    ;; Don't rename used nicknames.
    (setq erc-try-new-nick-p nil)

    ;; Don't leak our name.
    (setq erc-user-full-name nil)

    ;; Don't leak your username.
    (setq erc-email-userid "user")

    ;; Don't leak your system name.
    (setq erc-system-name "emacs")

    ;; Always show timestamps.
    (setq erc-timestamp-only-if-changed-flag nil)

    ;; Show timestamps on the left.
    (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

    ;; Make the format non-military.
    (setq erc-timestamp-format "%I:%M ")

    ;; Add an indention prefix.
    (setq erc-fill-prefix "    + ")

    ;; Automatically join a few channels.
    (setq erc-autojoin-channels-alist
          '(("libera.chat" "#emacs" "#xmonad")))

    ;; Stop annoying keybindings.
    (setq erc-track-enable-keybindings nil)

    ;; Don't bring newly-created erc buffers to the foreground.
    (setq erc-join-buffer 'bury)

    ;; Let's use a sane prompt please.
    (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))))

(defun chat ()
  "Starts an erc session."
  (interactive)
  (erc :server "irc.libera.chat"
       :port 6667
       :nick my-erc-nick
       :password my-erc-password))

;;
;; restclient
;;

(use-package restclient
  :ensure t
  :mode ("\\.rest$" . restclient-mode)
  :commands restclient-mode
  :config
  (progn
    ;; Prevent restclient from hijacking our M-x binding.
    (define-key restclient-mode-map (kbd "C-c n") nil)))

(use-package restclient-helm
  :disabled
  :ensure t
  :after restclient)

;;
;; rego
;;

(use-package rego-mode
  :ensure t)

;;
;; terraform
;;

(use-package terraform-mode
  :ensure t)

;;
;; 2048
;;

(use-package 2048-game
  :ensure t)
