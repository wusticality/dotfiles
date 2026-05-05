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

    ;; Remove the internal border (the couple-pixel stripe between
    ;; the OS frame edge and the first window). Apply to both new
    ;; frames and the current one.
    (add-to-list 'default-frame-alist '(internal-border-width . 0))
    (set-frame-parameter nil 'internal-border-width 0)

    ;; Suppress menu/tool bars on every new frame (TTY frames spawned
    ;; via emacsclient -t otherwise inherit a menu bar from the
    ;; daemon's frame defaults).
    (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
    (add-to-list 'default-frame-alist '(tool-bar-lines . 0))

    ;; Defensive: any newly-created frame (daemon + emacsclient + GUI
    ;; frame creation can flip global menu-bar-mode back on under
    ;; macOS) gets its menu/tool bars zeroed out at creation time.
    (defun my/zero-tty-bars (&rest _)
      "Force menu/tool bars off on every TTY frame."
      (dolist (frame (frame-list))
        (unless (display-graphic-p frame)
          (set-frame-parameter frame 'menu-bar-lines 0)
          (set-frame-parameter frame 'tool-bar-lines 0))))

    (add-hook 'after-make-frame-functions
              (lambda (_frame) (my/zero-tty-bars)))

    ;; emacs-plus's GUI frame creation flips menu-bar-mode back on
    ;; globally; the TTY frame inherits the menu bar after that.
    ;; Re-zero TTY frames whenever frame focus changes.
    (when (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function #'my/zero-tty-bars))

    ;; Belt-and-suspenders: any explicit (menu-bar-mode 1) call —
    ;; from anywhere — gets followed by re-zeroing TTY frames.
    (advice-add 'menu-bar-mode :after #'my/zero-tty-bars)

    ;; Move as far as possible when scrolling.
    (setq  scroll-error-top-bottom t)

    ;; Optimize redisplay under fast key-repeat. Without these,
    ;; holding C-n/C-p jumps and skips lines because pixel-precise
    ;; vscroll, font cache compaction, and per-keystroke
    ;; fontification can't keep up with a fast input queue.
    (setq auto-window-vscroll nil)
    (setq fast-but-imprecise-scrolling t)
    (setq redisplay-skip-fontification-on-input t)
    (setq inhibit-compacting-font-caches t)

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

    ;; Suppress hl-line while a region is active so the selection is
    ;; the only dark band visible — region and hl-line share the same
    ;; color, and otherwise hl-line leaks past the selection edges.
    (setq hl-line-range-function
          (lambda ()
            (unless (use-region-p)
              (cons (line-beginning-position) (line-beginning-position 2)))))

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

    ;; Add vertical space between lines.
    (setq-default line-spacing 2)

    ;; Subword mode is the cat's meow.
    (global-subword-mode 1)

    ;; Shoot for an 80 character width.
    (setq-default fill-column 80)

    ;; Use a sane tab width please.
    (setq-default tab-width 4)

    ;; Use spaces for indentation.
    (setq-default indent-tabs-mode nil)

    ;; Reuse existing splits; never auto-split.
    (setq display-buffer-base-action
          '((display-buffer-reuse-window display-buffer-use-some-window)))
    (setq split-height-threshold nil)
    (setq split-width-threshold nil)

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

    ;; Show matching delimiters instantly, and also
    ;; light them up when point is inside the pair.
    (setq show-paren-delay 0)
    (setq show-paren-when-point-inside-paren t)
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
    (setq mouse-autoselect-window t)

    ;; Disable GUI tooltip popups; help-echo still
    ;; renders in the echo area, matching TTY behavior.
    (tooltip-mode -1)))

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

    ;; Reserved for tmux's capture-mode toggle. bind-key* installs
    ;; this in `override-global-mode-map' so it beats minor-mode
    ;; bindings (paredit, hydra, etc.) that might otherwise grab C-;.
    (bind-key* "C-;" #'ignore)

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
;; clipboard
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    ;; Bridge emacs's kill-ring with the macOS clipboard for TTY
    ;; frames. GUI emacs handles this natively via the window system;
    ;; in a terminal frame we shell out to pbcopy/pbpaste so M-w
    ;; (kill-ring-save) writes to the system clipboard and C-y
    ;; (yank) pulls from it. Setting these globally is harmless for
    ;; GUI frames since they use different code paths.
    (when (and is-mac
               (executable-find "pbcopy")
               (executable-find "pbpaste"))
      (setq interprogram-cut-function
            (lambda (text)
              (with-temp-buffer
                (insert text)
                (call-process-region (point-min) (point-max) "pbcopy"))))
      (setq interprogram-paste-function
            (lambda ()
              (with-temp-buffer
                (call-process "pbpaste" nil t nil)
                (buffer-string)))))))

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
       '(default ((t (:height 145 :width normal :family "BlexMono Nerd Font Mono"))))))

    (when is-gnu
      (custom-set-faces
       `(default ((t (:height 116 :width normal :family "BlexMono Nerd Font Mono"))))))))

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
;; nerd-icons
;;

(use-package nerd-icons
  :demand t
  :config
  (progn
    (setq nerd-icons-font-family "Symbols Nerd Font Mono")))

;;
;; modeline
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    (defface wusticality-modeline-modified    '((t (:weight bold))) "Modified indicator.")
    (defface wusticality-modeline-buffer-name '((t (:weight bold))) "Buffer name.")
    (defface wusticality-modeline-position    '((t (:weight bold))) "Buffer % position.")
    (defface wusticality-modeline-line-column '((t (:weight bold))) "Line/column.")
    (defface wusticality-modeline-major-mode  '((t (:weight bold))) "Major mode.")
    (defface wusticality-modeline-git-icon    '((t (:weight bold))) "Git branch icon.")
    (defface wusticality-modeline-git-branch  '((t (:weight bold))) "Git branch name.")

    (defun wusticality-modeline-icon (icon-fn icon face &optional height)
      "Return a propertized nerd-icons ICON via ICON-FN inheriting FACE.
Optional HEIGHT defaults to 0.85."
      (when (featurep 'nerd-icons)
        (funcall icon-fn icon :face face :v-adjust 0.2 :height (or height 0.85))))

    (setq-default
     mode-line-format
     '(;; Modified indicator.
       (:eval (propertize "%* " 'face 'wusticality-modeline-modified))

       ;; Buffer name.
       (:eval (propertize "%b " 'face 'wusticality-modeline-buffer-name))

       ;; Percentage through the buffer.
       (:eval (propertize (concat
                           (format "%s"
                                   (if (<= (point-max) 1)
                                       0
                                     (round
                                      (* 100.0
                                         (/ (float (1- (point)))
                                            (float (1- (point-max))))))))
                           "%% ")
                          'face 'wusticality-modeline-position))

       ;; Line and column.
       (:eval (propertize "(%l, %c) " 'face 'wusticality-modeline-line-column))

       ;; Major mode.
       (:eval (concat
               (wusticality-modeline-icon #'nerd-icons-faicon
                                          "nf-fa-gear"
                                          'wusticality-modeline-major-mode
                                          0.75)
               " "
               (propertize
                (format "%s " (s-trim (s-downcase (format "%s" major-mode))))
                'face 'wusticality-modeline-major-mode)))

       ;; Git branch, if any.
       (:eval (when vc-mode
                (concat
                 (wusticality-modeline-icon #'nerd-icons-devicon
                                            "nf-dev-git_branch"
                                            'wusticality-modeline-git-icon)
                 " "
                 (propertize (car (vc-git-branches))
                             'face 'wusticality-modeline-git-branch))))))))

;;
;; header-line
;;

(use-package emacs
  :straight (:type built-in)
  :init
  (progn
    (defface wusticality-header-modified '((t (:weight bold))) "Header modified indicator.")
    (defface wusticality-header-file     '((t (:weight bold))) "Header file path.")

    (defun wusticality-header-truncate-path (path budget)
      "Truncate PATH from the left at component boundaries to fit BUDGET columns.
Preserves the leading `~/' or `/' root marker, with `.../' between the
marker and the kept suffix to signal omitted intermediate components.
Falls back to the bare filename if even that won't fit alongside the
marker."
      (if (<= (length path) budget)
          path
        (let* ((home-rooted (string-prefix-p "~/" path))
               (abs-rooted (and (not home-rooted) (string-prefix-p "/" path)))
               (prefix (cond (home-rooted "~/.../")
                             (abs-rooted "/.../")
                             (t ".../")))
               (rest (cond (home-rooted (substring path 2))
                           (abs-rooted (substring path 1))
                           (t path)))
               (components (split-string rest "/"))
               (avail (- budget (length prefix)))
               (kept '())
               (used 0)
               (done nil))
          (dolist (comp (reverse components))
            (unless done
              (let* ((sep (if kept 1 0))
                     (cost (+ (length comp) sep)))
                (if (<= (+ used cost) avail)
                    (progn (push comp kept)
                           (setq used (+ used cost)))
                  (setq done t)))))
          (if kept
              (concat prefix (mapconcat #'identity kept "/"))
            (or (car (last components)) path)))))

    (defun wusticality-enable-header-line ()
      "Configure `header-line-format' for file-backed buffers."
      (when buffer-file-name
        (setq header-line-format
              '(;; Modified indicator.
                (:eval (propertize "%* " 'face 'wusticality-header-modified))

                ;; File path, abbreviated to ~ if under home, then
                ;; truncated from the left to fit window width.
                (:eval (propertize
                        (wusticality-header-truncate-path
                         (abbreviate-file-name buffer-file-name)
                         (- (window-width) 4))
                        'face 'wusticality-header-file))))))

    (add-hook 'find-file-hook #'wusticality-enable-header-line)))

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
    (load-theme 'wusticality t)

    ;; Re-apply the theme once everything has loaded. The first load
    ;; above runs before most use-package forms, so packages whose
    ;; faces the theme targets (auto-dim-other-buffers, dirvish, ivy,
    ;; swiper, ...) haven't defined their defface specs yet and the
    ;; theme has nothing to bind to. emacs-startup-hook runs strictly
    ;; after after-init-hook so all :demand t packages are guaranteed
    ;; loaded; an idle-timer fallback covers any deferred loads.
    (defun wusticality--reload-theme ()
      (load-theme 'wusticality t))
    (defun wusticality--reload-theme-deferred ()
      (load-theme 'wusticality t)
      (run-with-idle-timer 0.1 nil #'wusticality--reload-theme))
    (add-hook 'emacs-startup-hook #'wusticality--reload-theme-deferred)))

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
   ("C-r" . swiper-isearch-backward)
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
   ("C-x p" . my-counsel-git))
  :init
  ;; Show the count format.
  (setq ivy-count-format "(%d/%d) ")

  ;; Make highlight extend all the way to the right.
  (setq ivy-format-function 'ivy-format-function-line)

  ;; Theme-side faces for the recolored match in the selected
  ;; swiper candidate (see custom format function below).
  (defface wusticality-ivy-selected-match
    '((t :inherit highlight :weight bold))
    "Face for matches inside the selected swiper candidate.")

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

  ;; Make C-r go to previous match in swiper instead of history search.
  (define-key swiper-map (kbd "C-r") #'ivy-previous-line)

  ;; Suspend hl-line during swiper sessions. Without this, the
  ;; original cursor line keeps its hl-line band while swiper
  ;; previews a different line, leaving two highlighted lines.
  (defun wusticality--swiper-without-hl-line (orig-fn &rest args)
    (let ((was-on global-hl-line-mode))
      (when was-on
        (global-hl-line-mode -1)
        (global-hl-line-unhighlight-all))
      (unwind-protect (apply orig-fn args)
        (when was-on (global-hl-line-mode 1)))))
  (advice-add 'swiper :around #'wusticality--swiper-without-hl-line)
  (advice-add 'swiper-isearch :around #'wusticality--swiper-without-hl-line)
  (advice-add 'swiper-isearch-backward :around #'wusticality--swiper-without-hl-line)

  ;; Skip the swiper-line-face line marker when there's no input
  ;; yet, so the original cursor line isn't highlighted before the
  ;; user types anything. Once a query is entered, normal preview
  ;; behavior resumes.
  (defun wusticality--swiper-skip-empty-line-overlay (&rest _)
    (not (string-empty-p (or ivy-text ""))))
  (advice-add 'swiper--add-line-overlay :before-while
              #'wusticality--swiper-skip-empty-line-overlay)

  ;; For backward swiper-isearch, point lands at match-beginning.
  ;; swiper--add-overlays only paints the orange "current" face when
  ;; (= (match-end 0) pt), so backward never gets the highlight.
  ;; After the action runs, nudge point to match-end and rebuild
  ;; overlays so backward looks the same as forward.
  (defun wusticality--swiper-isearch-backward-fixup (orig-fn x)
    (let ((result (funcall orig-fn x)))
      (ignore-errors
        (when (and (bound-and-true-p swiper--isearch-backward)
                   (not (eq ivy-exit 'done))
                   ivy-regex)
          (with-ivy-window
            (save-match-data
              (let ((re (ivy-re-to-str ivy-regex)))
                (when (and (stringp re) (looking-at re))
                  (goto-char (match-end 0))
                  (swiper--cleanup)
                  (swiper--delayed-add-overlays)))))))
      result))
  (advice-add 'swiper-isearch-action :around
              #'wusticality--swiper-isearch-backward-fixup)

  ;; Custom ivy format function for swiper that re-paints match
  ;; spans in the selected candidate using
  ;; `wusticality-ivy-selected-match', so the selected line's match
  ;; pops orange instead of blending into purple under
  ;; `ivy-current-match'.
  (defun wusticality-ivy-format-line (cands)
    (ivy--format-function-generic
     (lambda (str)
       (let* ((s (concat str "\n"))
              (len (length s))
              (pos 0)
              (match-faces '(ivy-minibuffer-match-face-1
                             ivy-minibuffer-match-face-2
                             ivy-minibuffer-match-face-3
                             ivy-minibuffer-match-face-4))
              (spans nil))
         ;; Collect match spans up front, before ivy--add-face mutates
         ;; the face structure into a blended cons form.
         (while (< pos len)
           (let* ((next (or (next-single-property-change pos 'face s) len))
                  (face (get-text-property pos 'face s)))
             (when (or (memq face match-faces)
                       (and (listp face) (seq-intersection face match-faces)))
               (push (cons pos next) spans))
             (setq pos next)))
         (ivy--add-face s 'ivy-current-match)
         (dolist (span spans)
           (put-text-property (car span) (cdr span) 'face
                              'wusticality-ivy-selected-match s))
         s))
     (lambda (str) (concat str "\n"))
     cands
     ""))
  (setf (alist-get t ivy-format-functions-alist)
        #'wusticality-ivy-format-line)

  ;; counsel-yank-pop ships its own :format-fn — point it at ours so
  ;; kill-ring matches get the same purple/orange treatment as
  ;; everywhere else. We drop the dashed separator and just rely on
  ;; the standard line-by-line layout.
  (ivy-configure 'counsel-yank-pop
    :format-fn #'wusticality-ivy-format-line)

  ;; For dynamic collections (counsel-git-grep, counsel-rg, ...) ivy
  ;; sets `ivy--old-re' to nil, so `ivy--highlight-ignore-order'
  ;; (our re-builder's highlighter) skips painting matches in
  ;; candidates. Fall back to deriving the regex list from ivy-text
  ;; for the duration of the call so grep candidates get the same
  ;; purple/orange match treatment as static collections.
  ;; Workaround for an ivy 0.15.1 bug: when `completing-read' is
  ;; passed a *function* as REQUIRE-MATCH (a per-input validator,
  ;; legal per the `completing-read' spec — magit uses it to reject
  ;; branch names that conflict with existing branches), ivy.el
  ;; line 756 does `(funcall require-match)' with zero args instead
  ;; of passing the candidate. Magit's 1-arg lambda then errors
  ;; with `Wrong number of arguments: (1 . 1), 0' on RET.
  ;;
  ;; Drop the function-form require-match before it reaches ivy.
  ;; The user loses magit's pre-RET validation but git itself still
  ;; rejects bad refnames, so the worst case is a deferred error.
  (defun wusticality--ivy-strip-fn-require-match (orig-fn prompt collection
                                                          &optional predicate
                                                          require-match
                                                          &rest rest)
    (apply orig-fn prompt collection predicate
           (and (not (functionp require-match)) require-match)
           rest))
  (advice-add 'ivy-completing-read :around
              #'wusticality--ivy-strip-fn-require-match)

  (defun wusticality--ivy-highlight-fallback (orig-fn str)
    (let ((ivy--old-re
           (if (consp ivy--old-re)
               ivy--old-re
             (when (and (stringp ivy-text)
                        (> (length ivy-text) 0))
               (mapcar (lambda (s) (cons s t))
                       (split-string ivy-text " " t))))))
      (funcall orig-fn str)))
  (advice-add 'ivy--highlight-ignore-order :around
              #'wusticality--ivy-highlight-fallback)

  (defun my-counsel-git ()
    "Find a file in a git project.
With C-u, pick from known projects. With C-u C-u, pick a directory."
    (interactive)
    (let ((default-directory
           (cond
            ((equal current-prefix-arg '(16))
             (read-directory-name "repo: "))
            ((equal current-prefix-arg '(4))
             (ivy-read "repo: " (project-known-project-roots)))
            (t default-directory)))
          (current-prefix-arg nil))
      (call-interactively #'counsel-git)))

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

  ;; Highlight the entire line in the minibuffer. Use our custom
  ;; format function (defined in the counsel block) so the selected
  ;; candidate's match still pops orange instead of being dimmed by
  ;; ivy-current-match's bg blend.
  (setcdr (assoc t ivy-format-functions-alist) #'wusticality-ivy-format-line))

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
  :bind (("C-x g" . my-magit-status))
  :init
  (progn
    ;; Turn these off, they're ugly.
    (setq magit-section-visibility-indicators nil))
  :config
  (progn
    ;; Open the status buffer in the current window and select it.
    ;; If a magit-status buffer is already visible somewhere, jump to
    ;; that window instead of opening a duplicate.
    (setq magit-display-buffer-function
          (lambda (buffer)
            (or (and (eq (buffer-local-value 'major-mode buffer)
                         'magit-status-mode)
                     (get-buffer-window buffer))
                (magit-display-buffer-same-window-except-diff-v1 buffer))))

    (defun my-magit-status ()
      "Open magit status. With C-u, pick from known repos. With C-u C-u, browse."
      (interactive)
      (cond
       ((equal current-prefix-arg '(16))
        (magit-status (read-directory-name "repo: ")))
       ((equal current-prefix-arg '(4))
        (magit-status (ivy-read "repo: " (project-known-project-roots))))
       (t
        (call-interactively 'magit-status)))
      (magit-section-show-level-2-all))))

;;
;; lsp-mode
;;

(use-package lsp-mode
  :demand t
  :after yasnippet
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . yas-minor-mode))
  :bind-keymap ("C-c k" . lsp-command-map)
  :bind (("C-c M-." . xref-find-definitions-other-window)
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

    ;; We use our own header, so don't use lsp's.
    (setq lsp-headerline-breadcrumb-enable nil)

    ;; Turn on highlighting please.
    (setq lsp-semantic-tokens-enable t)

    ;; Don't enable code lens.
    (setq lsp-lens-enable nil)))

;;
;; jump hydra
;;

(defhydra hydra-jump
  (:columns 3 :exit t)
  "jump"

  ;; LSP.
  ("r" lsp-find-references "references")
  ("i" lsp-find-implementation "implementation")
  ("s" lsp-ivy-workspace-symbol "workspace symbol")

  ;; Project.
  ("g" counsel-git-grep "git grep")
  ("f" counsel-git "git file")

  ;; Cancel.
  ("q" nil "quit" :exit t))

(global-set-key (kbd "C-c j") 'hydra-jump/body)

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
;; dirvish
;;

(use-package dirvish
  :demand t
  :bind (("C-c SPC" . dirvish-side-toggle-no-focus)
         ("C-c C-SPC" . dirvish-side))
  :init
  (progn
    ;; Sidebar width.
    (setq dirvish-side-width 35)

    ;; Auto-expand the sidebar to reveal the current file.
    (setq dirvish-side-auto-expand t)

    ;; What columns to show in the sidebar. Experiment with these later.
    ;; - vc-state
    ;; - nerd-icons
    ;; - collapse
    ;; - file-size
    ;; - file-time
    ;; - file-modes
    ;; - file-user
    ;; - file-group
    ;; - subtree-state
    ;; - symlink-target
    ;; - git-msg
    (setq dirvish-side-attributes
          '(vc-state nerd-icons collapse))

    ;; Don't pad the mode-line with a taller-than-text bar image;
    ;; let it use the natural text height so it matches the other
    ;; windows' mode-lines.
    (setq dirvish-mode-line-bar-image-width nil))
  :config
  (progn
    ;; Apply dirvish rendering ONLY inside side sessions. Vanilla
    ;; dired (M-x dired, C-x d) stays completely untouched.
    ;;
    ;; dirvish-override-dired-mode is the usual way to globally swap
    ;; dired for dirvish, but it adds ~7 advices and reshapes every
    ;; dired buffer. For a sidebar-only workflow we just need the
    ;; dired-noselect :around advice, conditionally gated on the
    ;; caller already being inside a 'side session (which means the
    ;; current buffer has :dv prop set to a side dv).
    (define-advice dired-noselect
        (:around (fn &rest args) dirvish-side-only)
      (if (and (fboundp 'dirvish-curr)
               (dirvish-curr)
               (eq (dv-type (dirvish-curr)) 'side))
          (apply #'dirvish-dired-noselect-a fn args)
        (apply fn args)))

    ;; Sidebar auto-follows the current file (buffer-list-update-hook).
    (dirvish-side-follow-mode 1)

    ;; Toggle sidebar visibility WITHOUT stealing focus.
    ;;
    ;; dirvish-side itself is a cycle (hide if focused, focus if
    ;; visible-unfocused, show if hidden) — not a pure toggle. This
    ;; wrapper makes it: hidden -> show-no-focus, visible -> hide.
    (defun dirvish-side-toggle-no-focus ()
      "Toggle `dirvish-side' visibility without stealing focus."
      (interactive)
      (if-let ((win (dirvish-side--session-visible-p)))
          (delete-window win)
        (save-selected-window (dirvish-side))))

    ;; Make RET in the SIDEBAR toggle subtree on directories and open
    ;; files through the side session's file handler (which routes to
    ;; the main window). Default dired behavior replaces the buffer
    ;; in place, which is wrong for a persistent sidebar.
    ;;
    ;; Regular dired/dirvish buffers (e.g. M-x dired) fall through to
    ;; the default dired-find-file.
    (defun dirvish-side-ret ()
      "RET handler: sidebar-aware, otherwise default dired-find-file."
      (interactive)
      (let ((dv (dirvish-curr)))
        (if (and dv (eq (dv-type dv) 'side))
            (when-let* ((entry (dired-get-filename nil t)))
              (if (file-directory-p entry)
                  (dirvish-subtree-toggle)
                (dirvish--find-entry 'find-file entry)))
          (call-interactively #'dired-find-file))))
    (define-key dirvish-mode-map (kbd "RET") #'dirvish-side-ret)

    ;; Re-root the sidebar on project switch.
    ;;
    ;; By default dirvish-side--auto-jump navigates to the deepest
    ;; existing prefix of the target file in `dired-subdir-alist',
    ;; which means: if the sidebar is rooted at ~/ and you open a
    ;; file in ~/repos/loyalty/..., the sidebar stays rooted at ~
    ;; and just moves into the subdir. The "Project:" header still
    ;; reads the sidebar's root, so it looks wrong and you can't
    ;; see the repo top-level.
    ;;
    ;; This advice runs before auto-jump: if the sidebar's root
    ;; differs from the current file's VC root, replace the side
    ;; session's dired buffer with one rooted at the file's VC
    ;; root. The original auto-jump then navigates+expands normally
    ;; against the fresh root.
    (define-advice dirvish-side--auto-jump
        (:before () reroot-on-project-change)
      (when-let* ((file buffer-file-name)
                  (file-root (dirvish--vc-root-dir))
                  (win (dirvish-side--session-visible-p))
                  (sidebar-root
                   (with-current-buffer (window-buffer win)
                     (expand-file-name default-directory))))
        (unless (string= (expand-file-name file-root) sidebar-root)
          (with-selected-window win
            (let (buffer-list-update-hook
                  window-buffer-change-functions)
              (dirvish--find-entry 'find-alternate-file file-root))))))))

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
    (define-key paredit-mode-map (kbd "C-c )") 'hydra-paredit/body)))

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
  :demand t)

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
;; auto-dim-other-buffers
;;

;; Tmux-like per-window backgrounds: selected window keeps the theme
;; default, non-selected windows get lifted to wusticality-slate.
;; Uses Emacs 27's :filtered face spec under the hood, so rendering
;; is truly per-window (same buffer in two windows still differs).
(use-package auto-dim-other-buffers
  :demand t
  :config
  (progn
    (auto-dim-other-buffers-mode 1)

    ;; Rebind adob to real window selection changes.
    ;;
    ;; adob out-of-the-box tracks the "selected window" via
    ;; buffer-list-update-hook. That hook fires on any buffer
    ;; mutation, including those that happen inside
    ;; (with-selected-window W ...) — and many packages use that
    ;; idiom to transiently select a window (to move point, render,
    ;; etc.) without intending to truly change focus. dirvish's
    ;; follow-mode is one such offender, but completion previews,
    ;; helm/ivy, tooltips and more do it too. Each transient
    ;; selection confuses adob into flipping its dim state, leaving
    ;; the wrong window looking "selected" until the next real
    ;; selection change.
    ;;
    ;; window-selection-change-functions (Emacs 27+) is the correct
    ;; hook: it fires only on REAL selection changes, not during
    ;; save-selected-window / with-selected-window. Swap adob onto
    ;; that.
    (remove-hook 'buffer-list-update-hook
                 #'adob--buffer-list-update-hook)
    (add-hook 'window-selection-change-functions
              (lambda (frame)
                (with-selected-frame frame
                  (adob--update))))))

;;
;; vterm
;;

(use-package vterm
  :ensure t
  :custom-face
  (vterm-color-black   ((t (:foreground "#282c34" :background "#282c34"))))
  (vterm-color-red     ((t (:foreground "#ef596f" :background "#ef596f"))))
  (vterm-color-green   ((t (:foreground "#89ca78" :background "#89ca78"))))
  (vterm-color-yellow  ((t (:foreground "#e5c07b" :background "#e5c07b"))))
  (vterm-color-blue    ((t (:foreground "#61afef" :background "#61afef"))))
  (vterm-color-magenta ((t (:foreground "#d55fde" :background "#d55fde"))))
  (vterm-color-cyan    ((t (:foreground "#2bbac5" :background "#2bbac5"))))
  (vterm-color-white   ((t (:foreground "#abb2bf" :background "#abb2bf")))))

;;
;; agent-shell
;;

(use-package agent-shell
  :disabled t
  :demand t
  :init
  (progn
    (setq agent-shell-highlight-blocks t)
    (setq agent-shell-thought-process-icon (all-the-icons-fileicon "brain"))
    ;; (setq agent-shell-permission-icon (all-the-icons-material "security"))
    )
  :config
  (progn
	;; Syntax highlight code correctly in agent-shell chat windows.
	;; Since we use tree-sitter, we have to provide these mappings.
    (dolist (mapping '(("rust" . "rust-ts")
                       ("toml" . "toml-ts")
                       ("go" . "go-ts")
                       ("json" . "json-ts")
                       ("javascript" . "js-ts")
                       ("js" . "js-ts")
                       ("typescript" . "typescript-ts")
                       ("tsx" . "tsx-ts")
                       ("yaml" . "yaml-ts")
                       ("lua" . "lua-ts")
                       ("dockerfile" . "dockerfile-ts")
                       ("bash" . "bash-ts")
                       ("sh" . "bash-ts")
                       ("c" . "c-ts")
                       ("css" . "css-ts")
                       ("csharp" . "csharp-ts")
                       ("python" . "python-ts")
                       ("java" . "java-ts")))
      (add-to-list 'markdown-overlays-language-mapping mapping))))

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
