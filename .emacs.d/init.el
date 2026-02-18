;;; init.el --- My init file.

;;; Commentary:

;; Kevin Depue (2025)
;; https://github.com/wusticality

;;; Code:

;; TODO: Figure out mark history ring.
;; TODO: Experiment with project.el.
;; TODO: Explore restclient.el alternatives.

;;
;; variables
;;

(defvar is-mac (eq system-type 'darwin)
  "Non-nil if Emacs is running on mac.")

(defvar is-gnu (eq system-type 'gnu/linux)
  "Non-nil if Emacs is running on gnu.")

(defvar my-completion-engine 'ivy
  "Which completion engine to use.")

(defvar use-ivy (eq my-completion-engine 'ivy)
  "Non-nil if we are using ivy.")

(defvar use-vertico (eq my-completion-engine 'vertico)
  "Non-nil if we are using vertico.")

;;
;; straight
;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;
;; use-package
;;

(straight-use-package 'use-package)

(use-package straight
  :config
  (progn
    ;; Always use straight to install packages.
    (setq straight-use-package-by-default t)))

;;
;; libraries
;;

;; The string library.
(use-package s
  :demand t)

;; The file library.
(use-package f
  :demand t)

;; The list library.
(use-package dash
  :demand t)

;;
;; customize
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Confine customize settings to their own file.
    (setq custom-file (f-join user-emacs-directory "custom.el"))

    ;; Load customizations if they exist.
    (when (f-exists? custom-file)
      (load custom-file))))

;;
;; defaults
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
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

    (defun display-startup-echo-area-message ()
      "Clear the minibuffer on startup please."
      (message ""))

    ;; Make the scratch buffer empty.
    (setq initial-scratch-message "")

    ;; Highlight the line that point is on.
    (global-hl-line-mode t)

    ;; Answering 'y' or 'n' will do.
    (defalias 'yes-or-no-p 'y-or-n-p)

    ;; Use utf-8 encoding please.
    (prefer-coding-system 'utf-8)
    (set-language-environment "UTF-8")
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)

    ;; Always show line and column numbers.
    (setq line-number-mode t)
    (setq column-number-mode t)

    ;; Subword mode is the cat's meow.
    (global-subword-mode 1)

    ;; Shoot for an 80 character width.
    (setq-default fill-column 80)

    ;; Use a sane tab width please.
    (setq-default tab-width 4)

    ;; Use spaces for indentation.
    (setq-default indent-tabs-mode nil)

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
    (blink-cursor-mode -1)

    ;; Don't use a visual bell.
    (setq ring-bell-function 'ignore)

    ;; Cursor types.
    (setq-default cursor-type 'box)
    (setq-default cursor-in-non-selected-windows nil)

    ;; Remove trailing whitespace on save.
    (add-hook 'before-save-hook 'delete-trailing-whitespace)

    ;; Don't save credentials to ~/.authinfo!
    (setq auth-source-save-behavior nil)

    ;; Turn on electric pair mode.
    (electric-pair-mode 1)

    ;; Make vertical dividers more visually pleasing.
    (window-divider-mode)

    ;; Keep the warnings buffer from opening.
    ;; I don't like being interrupted by it.
    (setq display-buffer-alist
          '(("^\\*Warnings\\*$"
             (display-buffer-no-window)
             (allow-no-window . t))))

    ;; All focus to follow the mouse.
    (setq mouse-autoselect-window t)))

;;
;; global keybindings
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Bind C-M-h to M-<backspace>.
    (define-key key-translation-map [?\C-\M-h] [?\C-\M-?])

    ;; This keybinding is annoying as hell.
    (global-unset-key (kbd "C-x C-b"))

    ;; Bind other-frame to a useful hotkey.
    (global-set-key (kbd "C-c o") 'other-frame)

    (defun reload-emacs ()
      "Reload your init.el file."
      (interactive)
      (load user-init-file))

    (global-set-key (kbd "<f12>") 'reload-emacs)

    (defun indent-buffer ()
      (interactive)
      (save-excursion
        (indent-region (point-min) (point-max) nil)))

    ;; Hotkey to indent a buffer.
    (global-set-key (kbd "C-c i") 'indent-buffer)

    (defun rotate-buffers ()
      "Rotate buffers clockwise in the current frame."
      (interactive)
      (let* ((windows (window-list))
             (num-windows (length windows)))
        (when (> num-windows 1)
          ;; Store the current buffer and point position.
          (let ((current-buffer (current-buffer))
                (current-point (point))
                (buffers (mapcar #'window-buffer windows)))

            ;; Rotate the buffers
            (dotimes (i num-windows)
              (let* ((current-window (nth i windows))
                     (next-buffer (nth (mod (- i 1) num-windows) buffers)))
                (set-window-buffer current-window next-buffer)))

            ;; Move point to the corresponding window with the same buffer.
            (let ((new-window (get-buffer-window current-buffer)))
              (when new-window
                (select-window new-window)
                (goto-char current-point)))))))

    ;; Hotkey to rotate buffers in the current frame.
    (global-set-key (kbd "C-c y") 'rotate-buffers)))

;;
;; backups
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Never create backup files.
    (setq make-backup-files nil)

    ;; Inhibit backups for all files.
    (setq backup-inhibited t)

    ;; Disable versioned backups.
    (setq version-control nil)))

;;
;; autorevert
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Lower the auto revert interval.
    (setq auto-revert-interval 1)

    ;; Auto revert dired buffers.
    (setq global-auto-revert-non-file-buffers t)

    ;; Suppress revert messages.
    (setq auto-revert-verbose nil)

    ;; Auto revert everywhere.
    (global-auto-revert-mode 1)))

;;
;; autosave
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    (let ((auto-save-dir (f-join user-emacs-directory "auto-saves/")))
      ;; Create the auto save directory if it doesn't exist.
      (unless (f-exists? auto-save-dir)
        (f-mkdir-full-path auto-save-dir))

      ;; Put all auto save files in one place.
      (setq auto-save-file-name-transforms
            `((".*" ,auto-save-dir t)))

      ;; Put all auto save metadata in one place.
      (setq auto-save-list-file-prefix (f-join auto-save-dir "saves-")))

    ;; Never use lockfiles.
    (setq create-lockfiles nil)

    ;; Auto save after 10 seconds.
    (setq auto-save-timeout 10)

    ;; Auto save after 200 characters.
    (setq auto-save-interval 200)))

;;
;; font
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    (when is-mac
      (custom-set-faces
       '(default ((t (:height 130 :width normal :family "IBM Plex Mono"))))))

    (when is-gnu
      (custom-set-faces
       `(default ((t (:height 116 :width normal :family "IBM Plex Mono"))))))))

;;
;; paths
;;

(use-package exec-path-from-shell
  :demand t
  :if (display-graphic-p)
  :config
  (progn
    ;; Load our path from ~/.bash_profile.
    (exec-path-from-shell-initialize)))

;;
;; all-the-icons
;;

(use-package all-the-icons
  :demand t
  :if (or (display-graphic-p) (daemonp))
  :config
  (progn
    ;; Install the fonts if not present.
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts))))

;;
;; theme
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Setup our theme path.
    (setq custom-theme-directory (f-join user-emacs-directory "themes"))

    ;; Load our theme.
    (load-theme 'wusticality t)))

;;
;; projects
;;

(use-package project
  :straight (:type built-in)
  :config
  (progn
    ;; Don't hijack the C-x p prefix.
    (define-key global-map (kbd "C-x p") nil)))

;;
;; uniquify
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Customize the look of duplicate values.
    (setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")))

;;
;; ivy
;;

(use-package ivy
  :demand t
  :if use-ivy)

(use-package counsel
  :demand t
  :if use-ivy
  :after ivy
  :bind
  (("C-s" . swiper-isearch)
   ("M-x" . counsel-M-x)
   ("C-c n" . counsel-M-x)
   ("C-c b" . counsel-recentf)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-x b" . counsel-switch-buffer)
   ("C-h a" . counsel-apropos)
   ("C-h b" . counsel-descbinds)
   ("M-i" . swiper)
   ("C-c M-i" . counsel-git-grep)
   ("C-x p" . counsel-git))
  :init
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

  ;; Enable everywhere.
  (ivy-mode 1))

(use-package ivy-rich
  :demand t
  :if use-ivy
  :after ivy
  :config
  (ivy-rich-mode 1)

  ;; Columns for switch-buffer.
  (ivy-rich-set-columns
   'ivy-switch-buffer
   '((ivy-switch-buffer-transformer (:width 0.3))
     (ivy-rich-switch-buffer-path (:width 0.35 :face font-lock-comment-face))
     (ivy-rich-switch-buffer-indicators (:width 0.04 :face font-lock-builtin-face :align right))
     (ivy-rich-switch-buffer-size (:width 0.04 :face font-lock-comment-face))
     (ivy-rich-switch-buffer-major-mode (:width 0.12 :face font-lock-type-face))
     (ivy-rich-switch-buffer-project (:width 0.12 :face font-lock-string-face))))

  ;; Highlight the entire line in the minibuffer.
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-hydra
  :demand t
  :if use-ivy
  :after (ivy hydra))

(use-package lsp-ivy
  :demand t
  :if use-ivy
  :after ivy)

;;
;; vertico
;;

(use-package vertico
  :demand t
  :if use-vertico
  :init
  (progn
    ;; Show more candidates.
    (setq vertico-count 18)

    ;; Never resize.
    (setq vertico-resize nil)

    ;; Enable everywhere.
    (vertico-mode))
  :config
  (progn
    (defun my-find-file-bindings ()
      "Add ivy-like keybindings for find-file in vertico."
      (when (and (or (eq this-command 'find-file)
                     (eq this-command 'magit-status))
                 (boundp 'vertico-map)
                 (minibufferp))
        (define-key vertico-map (kbd "C-j") #'vertico-directory-enter)
        (define-key vertico-map (kbd "DEL") #'vertico-directory-up)))

    ;; Install the keybindings.
    (add-hook 'minibuffer-setup-hook #'my-find-file-bindings)))

;;
;; orderless
;;

(use-package orderless
  :demand t
  :if use-vertico
  :after vertico
  :init
  (setq completion-styles '(orderless)))

;;
;; consult
;;

;; (defun my-git-root ()
;;   "Find the git repository root for the current buffer."
;;   (or (locate-dominating-file default-directory ".git")
;;       default-directory))

;; (defun consult-git ()
;;   "List all files in a git repository."
;;   (interactive)
;;   (let ((dir (my-git-root))
;;         (files (split-string
;;                 (shell-command-to-string
;;                  "git ls-files -z --full-name --")
;;                 "\0")))
;;     (consult--read
;;      (consult--process-collection
;;       (consult--git-builder)
;;       :transform (lambda (x) (string-remove-prefix "./" x)))
;;      :prompt "Git files: "
;;      :require-match t
;;      :category 'file
;;      :state (consult--file-preview)
;;      :sort nil)
;;     ))

(use-package consult
  :demand t
  :if use-vertico
  :after vertico
  :bind (("C-c n" . execute-extended-command)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-i" . consult-line)
         ("C-c M-i" . consult-git-grep)))

;;
;; marginalia
;;

(use-package marginalia
  :demand t
  :if use-vertico
  :after vertico
  :init
  (marginalia-mode))

;;
;; hydra
;;

(use-package hydra
  :demand t)

;;
;; text-mode
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Wrap words in text mode please.
    (add-hook 'text-mode-hook 'turn-on-visual-line-mode)))

;;
;; org-mode
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; The style.
    (add-hook
     'org-mode-hook
     (lambda ()
       ;; Set a reasonable width.
       (set-fill-column 80)

       ;; Auto fill please.
       (turn-on-auto-fill)

       ;; Only show rightmost stars.
       (org-indent-mode)))))

;;
;; latex
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Only use one '%' in comments, please.
    (add-hook 'latex-mode-hook (lambda () (setq-local comment-add 0)))))

;;
;; yasnippet
;;

(use-package yasnippet
  :demand t)

;;
;; company
;;

;; This mode enables make-
;; believe intellisense.
(use-package company
  :demand t
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
;; company-box
;;

(use-package company-box
  :demand t
  :hook (company-mode . company-box-mode))

;;
;; magit
;;

;; The best git interface ever.
(use-package magit
  :demand t
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
;; lsp-mode
;;

(use-package lsp-mode
  :demand t
  :after yasnippet
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :bind-keymap ("C-c k" . lsp-command-map)
  :bind (("C-c M-u" . lsp-find-references)
         ("C-c M-y" . lsp-find-implementation)
         ("C-c M-." . xref-find-definitions-other-window)
         ("M-RET" . lsp-execute-code-action)
         ("<f2>" . lsp-rename))
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
    (setq lsp-lens-enable nil)))

;;
;; treesit
;;

(use-package treesit
  :straight (:type built-in)
  :init
  (progn
    ;; Set the treesit grammars we care about.
    (setq
     treesit-language-source-alist
     '((rust "https://github.com/tree-sitter/tree-sitter-rust")
       (toml "https://github.com/tree-sitter/tree-sitter-toml")
       (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
       (go "https://github.com/tree-sitter/tree-sitter-go")
       (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")
       (c "https://github.com/tree-sitter/tree-sitter-c")
       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       (html "https://github.com/tree-sitter/tree-sitter-html")
       (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       (css "https://github.com/tree-sitter/tree-sitter-css")
       (yaml "https://github.com/ikatyang/tree-sitter-yaml")
       (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
       (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
       (python "https://github.com/tree-sitter/tree-sitter-python")
       (java "https://github.com/tree-sitter/tree-sitter-java")

       ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
       ;; (regex "https://github.com/tree-sitter/tree-sitter-regex")
       ;; (make "https://github.com/alemuller/tree-sitter-make")
       ;; (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
       ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
       ))

    ;; We need to remap a few major modes. For shell scripts and c / c++,
    ;; let emacs figure out what kind of file it is, then simply remap.
    ;; Also remap all other old modes to the new typescript ones.
    (setq
     major-mode-remap-alist
     '((sh-mode . bash-ts-mode)
       (c-mode . c-ts-mode)
       (c++-mode . c++-ts-mode)
       (html-mode . html-ts-mode)
       (mhtml-mode . html-ts-mode)
       (js-mode . js-ts-mode)
       (css-mode . css-ts-mode)
       (csharp-mode . csharp-ts-mode)
       (python-mode . python-ts-mode)
       (java-mode . java-ts-mode)
       (glsl-mode . glsl-ts-mode)
       ))

    ;; cmake-ts-mode

    )
  :config
  (progn
    (defun treesit-install-all-missing-grammars ()
      "Install all missing treesit grammars."
      (interactive)
      (dolist (grammar treesit-language-source-alist)
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    (defun treesit-reinstall-all-grammars ()
      "Reinstall all treesit grammers."
      (interactive)
      (dolist (grammar treesit-language-source-alist)
        (treesit-install-language-grammar (car grammar))))

    ;; On startup, only install missing grammars.
    (treesit-install-all-missing-grammars)))

;;
;; copilot
;;

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :after company
  :init
  (progn
    ;; Be a bit less aggressive so I have a chance to
    ;; type . and ; without it stomping on me. :)
    (setq copilot-idle-delay 0.4))
  :config
  (progn
    ;; Enable copilot for programming modes.
    (add-hook 'prog-mode-hook 'copilot-mode)
    (add-hook 'text-mode-hook 'copilot-mode)

    ;; When to show / hide predicates.
    (add-to-list 'copilot-disable-predicates #'company--active-p)
    (add-to-list 'copilot-disable-display-predicates #'company--active-p)

    ;; Complete using the tab key.
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

    ;; Setup indentation settings.
    (add-to-list 'copilot-indentation-alist '(prog-mode 4))
    (add-to-list 'copilot-indentation-alist '(org-mode 2))
    (add-to-list 'copilot-indentation-alist '(text-mode 2))
    (add-to-list 'copilot-indentation-alist '(closure-mode 2))
    (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))))

;;
;; flycheck
;;

(use-package flycheck
  :demand t
  :init (global-flycheck-mode))

;; TODO: Fix this!

;;
;; c / c++
;;

;; ;; Open header files in c++ mode.
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ;; Style defaults.
;; (setq-default c-basic-offset 4)

;; ;; The style.
;; (add-hook
;;  'c-mode-common-hook
;;  (lambda ()
;;    (c-set-style "k&r")
;;    (setq tab-width 4)
;;    (setq indent-tabs-mode nil)
;;    (setq c-basic-offset 4)
;;    (c-set-offset 'inline-open '0)
;;    (c-set-offset 'case-label '4)
;;    (electric-pair-mode 1)))

;;
;; multiple-cursors
;;

(use-package multiple-cursors
  :demand t
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
;; expand-region
;;

(use-package expand-region
  :demand t
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
  "Reset any marked region."
  (interactive)
  (er/contract-region 0))

;;
;; dired-x
;;

(use-package dired-x
  :straight (:type built-in)
  :after dired
  :config
  (progn
    ;; When opening multiple files, open them in
    ;; the background, not in new windows please.
    (define-key dired-mode-map (kbd "F")
                #'(lambda () (interactive)
                    (dired-do-find-marked-files t)))))

;;
;; treemacs
;;

;; TODO: Customize this more.

(use-package treemacs
  :demand t
  :bind (("C-c SPC" . treemacs))
  :init
  (progn
    ;; I'm not quite sure that this does.
    (setq treemacs-follow-after-init t))
  :config
  (progn
    ;; Enable follow mode.
    (treemacs-follow-mode t)))

(use-package treemacs-all-the-icons
  :demand t
  :if (or (display-graphic-p) (daemonp))
  :after (treemacs all-the-icons)
  :config
  (progn
    (treemacs-load-theme "all-the-icons")))

;;
;; rainbow-delimiters
;;

(use-package rainbow-delimiters
  :demand t)

;; Turn this on for Emacs lisp.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;
;; paredit
;;

(use-package paredit
  :demand t
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
    (define-key lisp-interaction-mode-map [remap paredit-C-j] #'eval-print-last-sexp)

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
  "Return non-nil if point is on a sexp."
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
  :demand t
  :init
  (progn
    ;; Turn it on for all lisp modes.
    (add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
    (add-hook 'clojure-mode-hook 'paxedit-mode)
    (add-hook 'clojurescript-mode-hook 'paxedit-mode)
    (add-hook 'clojurec-mode-hook 'paxedit-mode)
    (add-hook 'cider-repl-mode-hook 'paxedit-mode)))

;;
;; ace-window
;;

(use-package ace-window
  :demand t
  :bind (("M-o" . ace-window))
  :init
  (progn
    (setq aw-dispatch-always t)))

;;
;; avy
;;

(use-package avy
  :demand t
  :bind (("C-;" . avy-goto-char)))

;;
;; rainbow-mode
;;

(use-package rainbow-mode
  :demand t
  :config
  (progn
    ;; Only style hex colors please.
    (setq rainbow-ansi-colors nil)
    (setq rainbow-latex-colors nil)
    (setq rainbow-html-colors nil)
    (setq rainbow-x-colors nil)))

;;
;; agent-shell
;;

(use-package agent-shell
  :demand t)

;;
;; gptel
;;

(use-package gptel
  :demand t
  :bind (("C-c RET" . gptel-send)
         ("C-c g" . hydra-gptel/body))
  :init
  (progn
    ;; Set the default mode.
    (setq gptel-default-mode 'org-mode)

    ;; Set prompts for each mode.
    (setq gptel-prompt-prefix-alist
          '((markdown-mode . "# ")
            (org-mode . "* ")
            (text-mode . "")))

    ;; Use gpt-4o by default.
    (setq gptel-model 'gpt-4o))
  :config
  (progn
    ;; The hydra.
    (defhydra hydra-gptel
      (:columns 3)
      "gptel"

      ;; Open gptel chats.
      ("g" gptel "gptel" :exit t)
      ("m" gptel-menu "gptel-menu" :exit t)
      ("k" gptel-abort "gptel-abort" :exit t)

      ;; Cancel.
      ("q" nil "quit" :exit t))

    (defun get-api-key (host user)
      "Read an API key from ~/.authinfo for HOST and USER."
      (let ((entry (car (auth-source-search
                         :host host
                         :user user
                         :require '(:secret)))))
        (when entry
          (let ((secret (plist-get entry :secret)))
            (if (functionp secret)
                (funcall secret)
              secret)))))

    ;; Setup Gemini.
    (let ((api-key (get-api-key "gemini.googleapis.com" "apikey")))
      (when api-key
        (gptel-make-gemini "Gemini" :key api-key :stream t)))))

;;
;; rust-ts-mode
;;

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode ("\\.rs\\'")
  :hook (rust-ts-mode . lsp-deferred)
  :config
  (progn
    ;; Setup new rust buffers.
    (add-hook
     'rust-ts-mode-hook
     (lambda ()
       ;; Truncate lines.
       (setq truncate-lines t)

       ;; Prevent lsp-mode from making edits before saving.
       (setq-local lsp-before-save-edits nil)

       ;; Let's organize imports and format on save.
       (add-hook 'before-save-hook 'lsp-organize-imports t t)
       (add-hook 'before-save-hook 'lsp-format-buffer t t)

       ;; Use the nightly toolchain for formatting.
       (setq-local lsp-rust-analyzer-rustfmt-extra-args '["+nightly"])

       ;; Increase the fontlock level.
       (setq-local treesit-font-lock-level 4)
       (treesit-font-lock-recompute-features)))))

;;
;; toml-ts-mode
;;

(use-package toml-ts-mode
  :straight (:type built-in)
  :mode ("\\.toml\\'"))

;;
;; go-ts-mode
;;

(use-package go-ts-mode
  :straight (:type built-in)
  :mode ("\\.go\\'")
  :hook(go-ts-mode . lsp-deferred)
  :config
  (progn
    ;; Use the correct tab width please.
    (setq go-ts-mode-indent-offset tab-width)

    ;; Setup new go buffers.
    (add-hook
     'go-ts-mode-hook
     (lambda ()
       ;; Truncate lines.
       (setq truncate-lines t)

       ;; Golang uses tabs, not spaces.
       (setq-default indent-tabs-mode t)

       ;; Prevent lsp-mode from making edits before saving.
       (setq-local lsp-before-save-edits nil)

       ;; Let's organize imports and format on save.
       (add-hook 'before-save-hook 'lsp-organize-imports t t)
       (add-hook 'before-save-hook 'lsp-format-buffer t t)

       ;; Increase the fontlock level.
       (setq-local treesit-font-lock-level 4)
       (treesit-font-lock-recompute-features)))))

;;
;; go-mod-ts-mode
;;

(use-package go-mod-ts-mode
  :straight (:type built-in)
  :mode ("go\\.mod\\'"
         "go\\.sum\\'"
         "go\\.work\\'"))

;;
;; json-ts-mode
;;

(use-package json-ts-mode
  :straight (:type built-in)
  :mode "\\.json\\'")

;;
;; js-ts-mode
;;

(use-package js-ts-mode
  :straight (:type built-in)
  :mode ("\\.js\\'"
         "\\.jsx\\'"))

;;
;; typescript-ts-mode
;;

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode "\\.ts\\'")

;;
;; tsx-ts-mode
;;

(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode "\\.tsx\\'")

;;
;; yaml-ts-mode
;;

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode ("\\.yml\\'"
         "\\.yaml\\'"))

;;
;; lua-ts-mode
;;

(use-package lua-ts-mode
  :straight (:type built-in)
  :mode "\\.lua\\'")

;;
;; dockerfile-ts-mode
;;

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode ("Dockerfile\\'"
         "Dockerfile\\..*\\'"
         "\\.dockerfile\\'"
         "\\.Dockerfile\\'"))

;;
;; glsl-mode
;;

(use-package glsl-mode
  :mode ("\\.vert\\'"
         "\\.frag\\'"
         "\\.geom\\'"
         "\\.comp\\'"
         "\\.tesc\\'"
         "\\.tese\\'"
         "\\.glsl\\'"))

;;
;; protobuf-mode
;;

(use-package protobuf-mode
  :mode "\\.proto")

;;
;; markdown-mode
;;

(use-package markdown-mode
  :mode "\\.md\\'")

;;
;; haskell-mode
;;

(use-package haskell-mode
  :mode "\\.hs\\'")

;;
;; mise
;;

(use-package mise
  :config
  (progn
    (global-mise-mode)))

;;
;; hl-todo
;;

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . hl-todo)
          ("NOTE" . hl-todo)
          ("FIXME" . hl-todo))))

;;
;; erc
;;

(use-package erc
  :straight (:type built-in)
  :init
  (progn
    ;; Fill chat messages based on window width.
    (make-variable-buffer-local 'erc-fill-column)
    (add-hook
     'window-configuration-change-hook
     (lambda ()
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
          '(("libera.chat" "#emacs")))

    ;; Stop annoying keybindings.
    (setq erc-track-enable-keybindings nil)

    ;; Don't bring newly-created erc buffers to the foreground.
    (setq erc-join-buffer 'bury)

    ;; Let's use a sane prompt please.
    (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))))

(defun chat ()
  "Start an erc session."
  (interactive)
  (let ((erc-file "~/.erc.el"))
    (if (f-exists? erc-file)
        (load erc-file)
      (error "No ~/.erc.el file found"))
    (unless (and (boundp 'erc-nick)(boundp 'erc-pass))
      (error "No erc-nick or erc-pass bound in ~/.erc.el"))
    (erc :server "irc.libera.chat"
         :port 6667
         :nick erc-nick
         :password erc-pass)))

;;
;; I'm unsure about these packages!
;;


;;
;; projectile
;;

;; (use-package projectile
;;   :demand t
;;   :init
;;   (progn
;;     ;; Add a global prefix.
;;     (global-set-key (kbd "C-c p") 'projectile-command-map)

;;     ;; Setup the completion system.
;;     (setq projectile-completion-system 'ivy)

;;     ;; Set our indexing mode.
;;     (setq projectile-indexing-method 'alien))
;;   :config
;;   (progn
;;     ;; Turn projectile on globally.
;;     (projectile-mode)))

;;
;; clang-format
;;

;; (defun my-clang-format-buffer ()
;;   "Reformat buffer if .clang-format exists in the projectile root."
;;   (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
;;     (clang-format-buffer)))

;; (let ((path "~/bin/clang-format.el"))
;;   (when (f-exists? path)
;;     ;; Load the package.
;;     (load path)

;;     ;; Format on save for c / c++.
;;     (-map
;;      (lambda (x)
;;        (add-hook x (lambda () (add-hook 'before-save-hook #'my-clang-format-buffer nil 'local))))
;;      '(c-mode-hook c++-mode-hook))))

;;
;; wgrep
;;

;; ;; Makes grep buffers editable.
;; (use-package wgrep
;;   :init
;;   (progn
;;     ;; Save modified buffers when you exit.
;;     (setq wgrep-auto-save-buffer t)))

;;
;; which-key
;;

;; ;; The replacement for guide-key. Given a key
;; ;; sequence, shows what commands are available.
;; (use-package which-key
;;   :disabled
;;   :config
;;   (progn
;;     ;; No delay please.
;;     (setq which-key-idle-delay 0)

;;     ;; Settings to make which-key opt-in.
;;     ;; (setq which-key-show-early-on-C-h t)
;;     ;; (setq which-key-idle-delay 10000)
;;     ;; (setq which-key-idle-secondary-delay 0)

;;     ;; Show count / total on the modeline.
;;     (setq which-key-show-remaining-keys t)

;;     ;; Allow 50% of the frame to display keys.
;;     (setq which-key-side-window-max-height 0.5)

;;     ;; Open it at the bottom.
;;     (which-key-setup-side-window-bottom)

;;     ;; Turn it on.
;;     (which-key-mode)))

;;
;; dap-mode
;;

;; (use-package dap-mode
;;   :demand t
;;   :after lsp-mode
;;   :init
;;   (progn
;;     ;; Control debug output.
;;     ;; (setq dap-print-io t)

;;     ;; Control which features are enabled.
;;     (setq dap-auto-configure-features
;;           '(sessions locals breakpoints controls tooltip)))
;;   :config
;;   (progn
;;     ;; Apply our settings above.
;;     (dap-auto-configure-mode)

;;     ;; Requirethe codelldb debugger.
;;     (require 'dap-codelldb)

;;     ;; TODO: Set this correctly per OS!

;;     ;; Require the lldb debugger.
;;     (setq dap-codelldb-debug-program "/usr/bin/codelldb")))

(provide 'init)

;;; init.el ends here
