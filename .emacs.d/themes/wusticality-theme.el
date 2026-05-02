;;; wusticality-theme.el --- My Emacs color theme.

;;; Commentary:

;; Kevin Depue (2025)
;; My own color them for Emacs.

;;; Code:

(deftheme wusticality "My own personal theme.")

;; Declare the colors.
(defvar wusticality-background "#282c34")
(defvar wusticality-foreground "#bbbbbb")
(defvar wusticality-hline "#1d2026")
(defvar wusticality-border "#313842")
(defvar wusticality-blue "#61afef")
(defvar wusticality-tan "#e5c07b")
(defvar wusticality-brown "#d19a66")
(defvar wusticality-gray "#5c6370")
(defvar wusticality-pink "#ef596f")
(defvar wusticality-magenta "#d55fde")
(defvar wusticality-green "#89ca78")
(defvar wusticality-orange "#ff8c00")
(defvar wusticality-red "#be5046")
(defvar wusticality-slate "#31353d")

;; (defvar wusticality-purple "#af87ff")
;; (defvar wusticality-cyan "#2bbac5")
;; (defvar wusticality-yellow "#ffff87")
;; (defvar wusticality-dark-red "#870000")
;; (defvar wusticality-dark-blue "#005faf")
;; (defvar wusticality-black "#080808")

;; Declare the theme.
(custom-theme-set-faces
 'wusticality

 ;;
 ;; defaults
 ;;

 `(default ((t (:background ,wusticality-background :foreground ,wusticality-foreground))))
 ;; success
 ;; warning
 ;; error
 ;; link
 ;; link-visited
 `(cursor ((t (:background ,wusticality-red))))
 `(fringe ((t (:background ,wusticality-background))))
 `(region ((t (:background ,wusticality-hline))))
 `(highlight ((t (:background ,wusticality-border))))
 `(hl-line ((t (:background ,wusticality-hline))))
 `(header-line ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :box nil))))
 `(vertical-border ((t (:foreground ,wusticality-background))))
 ;; secondary-selection
 ;; `(query-replace ((t (:background ,wusticality-black))))
 `(minibuffer-prompt ((t (:foreground ,wusticality-blue))))
 ;; tooltip
 ;; `(trailing-whitespace ((t (:background ,wusticality-background :foreground ,wusticality-foreground))))
 ;; `(lazy-highlight ((t (:background ,c-dark-blue))))
 ;; `(isearch ((t (:background ,wusticality-black))))
 ;; `(show-paren-match ((t (:background ,wusticality-dark-blue))))
 ;; `(show-paren-mismatch ((t (:background ,wusticality-dark-blue))))

 ;;
 ;; modeline
 ;;

 `(mode-line ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :bold t :box nil))))
 ;; mode-line-buffer-id
 ;; mode-line-emphasis
 ;; mode-line-highlight
 `(mode-line-inactive ((t (:background ,wusticality-hline :foreground ,wusticality-foreground :bold t :box nil))))
 `(wusticality-modeline-modified    ((t (:foreground ,wusticality-green   :weight bold))))
 `(wusticality-modeline-buffer-name ((t (:foreground ,wusticality-magenta :weight bold))))
 `(wusticality-modeline-position    ((t (:foreground ,wusticality-green   :weight bold))))
 `(wusticality-modeline-line-column ((t (:foreground ,wusticality-brown   :weight bold))))
 `(wusticality-modeline-major-mode  ((t (:foreground ,wusticality-blue    :weight bold))))
 `(wusticality-modeline-git-icon    ((t (:foreground ,wusticality-green   :weight bold))))
 `(wusticality-modeline-git-branch  ((t (:foreground ,wusticality-green   :weight bold))))

 ;;
 ;; header-line
 ;;

 `(wusticality-header-modified ((t (:foreground ,wusticality-green   :weight bold))))
 `(wusticality-header-file     ((t (:foreground ,wusticality-magenta :weight bold))))

 ;;
 ;; window-divider
 ;;

 `(window-divider ((t (:foreground ,wusticality-hline))))
 `(window-divider-first-pixel ((t (:foreground ,wusticality-hline))))
 `(window-divider-last-pixel ((t (:foreground ,wusticality-hline))))

 ;;
 ;; nerd-icons
 ;;

 ;; nerd-icons-blue
 ;; nerd-icons-blue-alt
 ;; nerd-icons-cyan
 ;; nerd-icons-cyan-alt
 ;; nerd-icons-dblue
 ;; nerd-icons-dcyan
 ;; nerd-icons-dgreen
 ;; nerd-icons-dmaroon
 ;; nerd-icons-dorange
 ;; nerd-icons-dpink
 ;; nerd-icons-dpurple
 ;; nerd-icons-dred
 ;; nerd-icons-dsilver
 ;; nerd-icons-dyellow
 ;; nerd-icons-green
 ;; nerd-icons-lblue
 ;; nerd-icons-lcyan
 ;; nerd-icons-lgreen
 ;; nerd-icons-lmaroon
 ;; nerd-icons-lorange
 ;; nerd-icons-lpink
 ;; nerd-icons-lpurple
 ;; nerd-icons-lred
 ;; nerd-icons-lsilver
 ;; nerd-icons-lyellow
 ;; nerd-icons-maroon
 ;; nerd-icons-orange
 ;; nerd-icons-pink
 ;; nerd-icons-purple
 ;; nerd-icons-purple-alt
 ;; nerd-icons-red
 ;; nerd-icons-red-alt
 ;; nerd-icons-silver
 ;; nerd-icons-yellow

 ;;
 ;; auto-dim-other-buffers
 ;;

 ;; adob--hack
 ;; auto-dim-other-buffers
 `(auto-dim-other-buffers-face ((t (:background ,wusticality-slate))))
 `(auto-dim-other-buffers-hide ((t (:foreground ,wusticality-slate :background ,wusticality-slate))))

 ;;
 ;; dirvish
 ;;

 ;; dirvish-file-device-number
 ;; dirvish-file-group-id
 ;; dirvish-file-inode-number
 ;; dirvish-file-link-number
 ;; dirvish-file-modes
 ;; dirvish-file-size
 ;; dirvish-file-time
 ;; dirvish-file-user-id
 ;; dirvish-free-space
 `(dirvish-hl-line ((t (:background ,wusticality-hline :extend t))))
 `(dirvish-hl-line-inactive ((t (:background ,wusticality-hline :extend t))))
 ;; dirvish-inactive
 ;; dirvish-media-info-heading
 ;; dirvish-media-info-property-key
 ;; dirvish-proc-failed
 ;; dirvish-proc-finished
 ;; dirvish-proc-running

 ;;
 ;; font lock
 ;;

 `(font-lock-builtin-face ((t (:foreground ,wusticality-blue))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-comment-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-constant-face ((t (:foreground ,wusticality-brown))))
 `(font-lock-doc-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-doc-markup-face ((t (:foreground ,wusticality-gray :italic t))))
 `(font-lock-function-name-face ((t (:foreground ,wusticality-blue))))
 `(font-lock-keyword-face ((t (:foreground ,wusticality-magenta :italic t))))
 ;; font-lock-negation-char-face
 `(font-lock-preprocessor-face ((t (:foreground ,wusticality-tan :italic t))))
 ;; font-lock-regexp-grouping-backslash
 ;; font-lock-regexp-grouping-construct
 `(font-lock-string-face ((t (:foreground ,wusticality-green))))
 `(font-lock-type-face ((t (:foreground ,wusticality-tan))))
 `(font-lock-variable-name-face ((t (:foreground ,wusticality-pink))))
 `(warning ((t (:foreground ,wusticality-orange))))

 ;;
 ;; lsp
 ;;

 ;; lsp-details-face
 ;; lsp-dired-path-error-face
 ;; lsp-dired-path-face
 ;; lsp-dired-path-hint-face
 ;; lsp-dired-path-info-face
 ;; lsp-dired-path-warning-face
 ;; lsp-face-highlight-read
 ;; lsp-face-highlight-textual
 ;; lsp-face-highlight-write
 ;; lsp-face-rename
 `(lsp-face-semhl-class ((t (:foreground ,wusticality-tan))))
 `(lsp-face-semhl-comment ((t (:foreground ,wusticality-gray :italic t))))
 `(lsp-face-semhl-constant ((t (:foreground ,wusticality-brown))))
 ;; lsp-face-semhl-decorator
 ;; lsp-face-semhl-default-library
 ;; lsp-face-semhl-definition
 ;; lsp-face-semhl-deprecated
 `(lsp-face-semhl-enum ((t (:foreground ,wusticality-tan))))
 ;; lsp-face-semhl-enum-member
 ;; lsp-face-semhl-event
 `(lsp-face-semhl-function ((t (:foreground ,wusticality-blue))))
 ;; lsp-face-semhl-implementation
 ;; lsp-face-semhl-interface
 `(lsp-face-semhl-keyword ((t (:foreground ,wusticality-magenta :italic t))))
 ;; lsp-face-semhl-label
 `(lsp-face-semhl-macro ((t (:foreground ,wusticality-blue))))
 `(lsp-face-semhl-member ((t (:foreground ,wusticality-pink))))
 `(lsp-face-semhl-method ((t (:foreground ,wusticality-blue))))
 ;; lsp-face-semhl-modifier
 ;; lsp-face-semhl-namespace
 `(lsp-face-semhl-number ((t (:foreground ,wusticality-brown))))
 ;; lsp-face-semhl-operator
 `(lsp-face-semhl-parameter ((t (:foreground ,wusticality-brown))))
 `(lsp-face-semhl-property ((t (:foreground ,wusticality-pink))))
 ;; lsp-face-semhl-regexp
 `(lsp-face-semhl-static ((t (:foreground ,wusticality-brown))))
 `(lsp-face-semhl-string ((t (:foreground ,wusticality-green))))
 `(lsp-face-semhl-struct ((t (:foreground ,wusticality-tan))))
 `(lsp-face-semhl-type ((t (:foreground ,wusticality-tan))))
 ;; lsp-face-semhl-type-parameter
 `(lsp-face-semhl-variable ((t (:foreground ,wusticality-foreground))))
 ;; lsp-headerline-breadcrumb-deprecated-face
 ;; lsp-headerline-breadcrumb-path-error-face
 ;; lsp-headerline-breadcrumb-path-face
 ;; lsp-headerline-breadcrumb-path-hint-face
 ;; lsp-headerline-breadcrumb-path-info-face
 ;; lsp-headerline-breadcrumb-path-warning-face
 ;; lsp-headerline-breadcrumb-project-prefix-face
 ;; lsp-headerline-breadcrumb-separator-face
 ;; lsp-headerline-breadcrumb-symbols-error-face
 ;; lsp-headerline-breadcrumb-symbols-face
 ;; lsp-headerline-breadcrumb-symbols-hint-face
 ;; lsp-headerline-breadcrumb-symbols-info-face
 ;; lsp-headerline-breadcrumb-symbols-warning-face
 ;; lsp-headerline-breadcrumb-unknown-project-prefix-face
 ;; lsp-inlay-hint-face
 ;; lsp-inlay-hint-parameter-face
 ;; lsp-inlay-hint-type-face
 ;; lsp-inline-completion-overlay-face
 ;; lsp-installation-buffer-face
 ;; lsp-installation-finished-buffer-face
 ;; lsp-javascript-inlay-face
 ;; lsp-javascript-inlay-parameter-face
 ;; lsp-javascript-inlay-type-face
 ;; lsp-lens-face
 ;; lsp-lens-mouse-face
 ;; lsp-modeline-code-actions-face
 ;; lsp-modeline-code-actions-preferred-face
 ;; lsp-rename-placeholder-face
 ;; lsp-rust-analyzer-abstract-modifier-face
 ;; lsp-rust-analyzer-async-modifier-face
 ;; lsp-rust-analyzer-attribute-modifier-face
 ;; lsp-rust-analyzer-callable-modifier-face
 ;; lsp-rust-analyzer-constant-modifier-face
 ;; lsp-rust-analyzer-consuming-modifier-face
 ;; lsp-rust-analyzer-control-flow-modifier-face
 ;; lsp-rust-analyzer-crate-root-modifier-face
 `(lsp-rust-analyzer-declaration-modifier-face ((t)))
 ;; lsp-rust-analyzer-default-library-modifier-face
 ;; lsp-rust-analyzer-definition-modifier-face
 ;; lsp-rust-analyzer-deprecated-modifier-face
 ;; lsp-rust-analyzer-documentation-modifier-face
 ;; lsp-rust-analyzer-injected-modifier-face
 ;; lsp-rust-analyzer-inlay-face
 ;; lsp-rust-analyzer-inlay-param-face
 ;; lsp-rust-analyzer-inlay-type-face
 ;; lsp-rust-analyzer-intra-doc-link-modifier-face
 ;; lsp-rust-analyzer-library-modifier-face
 `(lsp-rust-analyzer-mutable-modifier-face ((t)))
 ;; lsp-rust-analyzer-public-modifier-face
 ;; lsp-rust-analyzer-readonly-modifier-face
 `(lsp-rust-analyzer-reference-modifier-face ((t)))
 ;; lsp-rust-analyzer-static-modifier-face
 ;; lsp-rust-analyzer-trait-modifier-face
 ;; lsp-rust-analyzer-unsafe-modifier-face
 ;; lsp-signature-face
 ;; lsp-signature-highlight-function-argument
 ;; lsp-signature-posframe

 ;;
 ;; ivy
 ;;

 ;; ivy-action
 ;; ivy-completions-annotations
 ;; ivy-confirm-face
 `(ivy-current-match ((t (:background ,wusticality-hline))))
 ;; ivy-cursor
 ;; ivy-grep-info
 ;; ivy-grep-line-number
 ;; ivy-highlight-face
 ;; ivy-match-required-face
 ;; ivy-minibuffer-match-face-1
 ;; ivy-minibuffer-match-face-2
 ;; ivy-minibuffer-match-face-3
 ;; ivy-minibuffer-match-face-4
 ;; ivy-minibuffer-match-highlight
 ;; ivy-modified-buffer
 ;; ivy-modified-outside-buffer
 ;; ivy-org
 ;; ivy-prompt-match
 ;; ivy-remote
 ;; ivy-separator
 ;; ivy-subdir
 ;; ivy-virtual
 ;; ivy-yanked-word

 ;;
 ;; swiper
 ;;

 ;; swiper-background-match-face-1
 ;; swiper-background-match-face-2
 ;; swiper-background-match-face-3
 ;; swiper-background-match-face-4
 ;; swiper-line-face
 ;; swiper-match-face-1
 ;; swiper-match-face-2
 ;; swiper-match-face-3
 ;; swiper-match-face-4

 ;;
 ;; counsel
 ;;

 ;; counsel--mark-ring-highlight
 ;; counsel-active-mode
 ;; counsel-application-name
 ;; counsel-evil-register-face
 ;; counsel-key-binding
 ;; counsel-outline-1
 ;; counsel-outline-2
 ;; counsel-outline-3
 ;; counsel-outline-4
 ;; counsel-outline-5
 ;; counsel-outline-6
 ;; counsel-outline-7
 ;; counsel-outline-8
 ;; counsel-outline-default
 ;; counsel-variable-documentation

 ;;
 ;; vertico
 ;;

 ;; vertico-current
 ;; vertico-group-separator
 ;; vertico-group-title
 ;; vertico-multiline

 ;;
 ;; orderless
 ;;

 ;; orderless-match-face-0
 ;; orderless-match-face-1
 ;; orderless-match-face-2
 ;; orderless-match-face-3

 ;;
 ;; consult
 ;;

 ;; consult-async-failed
 ;; consult-async-finished
 ;; consult-async-option
 ;; consult-async-running
 ;; consult-async-split
 ;; consult-bookmark
 ;; consult-buffer
 ;; consult-file
 ;; consult-grep-context
 ;; consult-help
 ;; consult-highlight-mark
 ;; consult-highlight-match
 ;; consult-imenu-prefix
 ;; consult-key
 ;; consult-line-number
 ;; consult-line-number-prefix
 ;; consult-line-number-wrapped
 ;; consult-narrow-indicator
 ;; consult-preview-insertion
 ;; consult-preview-line
 ;; consult-preview-match

 ;;
 ;; marginalia
 ;;

 ;; marginalia-archive
 ;; marginalia-char
 ;; marginalia-date
 ;; marginalia-documentation
 ;; marginalia-file-name
 ;; marginalia-file-owner
 ;; marginalia-file-priv-dir
 ;; marginalia-file-priv-exec
 ;; marginalia-file-priv-link
 ;; marginalia-file-priv-no
 ;; marginalia-file-priv-other
 ;; marginalia-file-priv-rare
 ;; marginalia-file-priv-read
 ;; marginalia-file-priv-write
 ;; marginalia-function
 ;; marginalia-installed
 ;; marginalia-key
 ;; marginalia-lighter
 ;; marginalia-list
 ;; marginalia-mode
 ;; marginalia-modified
 ;; marginalia-null
 ;; marginalia-number
 ;; marginalia-off
 ;; marginalia-on
 ;; marginalia-size
 ;; marginalia-string
 ;; marginalia-symbol
 ;; marginalia-true
 ;; marginalia-type
 ;; marginalia-value
 ;; marginalia-version

 ;;
 ;; hydra
 ;;

 ;; hydra-face-amaranth
 ;; hydra-face-blue
 ;; hydra-face-pink
 ;; hydra-face-red
 ;; hydra-face-teal
 ;; lv-separator

 ;;
 ;; flycheck
 ;;

 ;; flycheck-delimited-error
 `(flycheck-error ((t (:underline (:color ,wusticality-red :style line) :bold t))))
 ;; flycheck-error-delimiter
 ;; flycheck-error-list-checker-name
 ;; flycheck-error-list-column-number
 ;; flycheck-error-list-error
 ;; flycheck-error-list-error-message
 ;; flycheck-error-list-filename
 ;; flycheck-error-list-highlight
 ;; flycheck-error-list-id
 ;; flycheck-error-list-id-with-explainer
 ;; flycheck-error-list-info
 ;; flycheck-error-list-line-number
 ;; flycheck-error-list-warning
 ;; flycheck-fringe-error
 ;; flycheck-fringe-info
 ;; flycheck-fringe-warning
 `(flycheck-info ((t (:underline (:color ,wusticality-green :style line) :bold t))))
 ;; flycheck-verify-select-checker
 `(flycheck-warning ((t (:underline (:color ,wusticality-brown :style line) :bold t))))
 ;; flymake-error
 ;; flymake-note
 ;; flymake-warning

 ;;
 ;; multiple-cursors
 ;;

 ;; mc/cursor-bar-face
 ;; mc/cursor-face
 ;; mc/region-face

 ;;
 ;; magit
 ;;

 ;; magit-bisect-bad
 ;; magit-bisect-good
 ;; magit-bisect-skip
 ;; magit-blame-date
 ;; magit-blame-dimmed
 ;; magit-blame-hash
 ;; magit-blame-heading
 ;; magit-blame-highlight
 ;; magit-blame-margin
 ;; magit-blame-name
 ;; magit-blame-summary
 ;; magit-branch-current
 ;; magit-branch-local
 ;; magit-branch-remote
 ;; magit-branch-remote-head
 ;; magit-branch-upstream
 ;; magit-branch-warning
 ;; magit-cherry-equivalent
 ;; magit-cherry-unmatched
 ;; `(magit-diff-added ((t (:foreground ,wusticality-green :bold t))))
 ;; `(magit-diff-added-highlight ((t (:foreground ,wusticality-green :bold t))))
 ;; magit-diff-added-indicator
 ;; magit-diff-base
 ;; magit-diff-base-heading
 ;; magit-diff-base-highlight
 ;; magit-diff-base-indicator
 ;; magit-diff-conflict-heading
 ;; magit-diff-conflict-heading-highlight
 ;; magit-diff-context
 ;; magit-diff-context-highlight
 ;; `(magit-diff-file-heading ((t (:foreground ,wusticality-foreground))))
 ;; `(magit-diff-file-heading-highlight ((t (:foreground ,wusticality-purple :bold t))))
 ;; `(magit-diff-file-heading-selection ((t (:foreground ,c-old :bold t))))
 ;; `(magit-diff-hunk-heading ((t (:background ,c-mid-gray :foreground ,wusticality-foreground))))
 ;; `(magit-diff-hunk-heading-highlight ((t (:background ,c-light-gray :foreground ,wusticality-purple :bold t))))
 ;; `(magit-diff-hunk-heading-selection ((t (:background ,c-light-gray :foreground ,c-old :bold t))))
 ;; `(magit-diff-hunk-region ((t (:background ,wusticality-black))))
 ;; `(magit-diff-lines-boundary ((t (:background ,c-light-gray :foreground ,wusticality-purple :bold t))))
 ;; `(magit-diff-lines-heading ((t (:background ,c-light-gray :foreground ,wusticality-purple :bold t))))
 ;; magit-diff-our
 ;; magit-diff-our-heading
 ;; magit-diff-our-highlight
 ;; magit-diff-our-indicator
 ;; `(magit-diff-removed ((t (:foreground ,wusticality-pink :bold t))))
 ;; `(magit-diff-removed-highlight ((t (:foreground ,wusticality-pink :bold t))))
 ;; magit-diff-removed-indicator
 ;; magit-diff-revision-summary
 ;; magit-diff-revision-summary-highlight
 ;; magit-diff-their
 ;; magit-diff-their-heading
 ;; magit-diff-their-highlight
 ;; magit-diff-their-indicator
 ;; magit-diff-whitespace-warning
 ;; magit-diffstat-added
 ;; magit-diffstat-removed
 ;; magit-dimmed
 ;; magit-filename
 ;; magit-hash
 ;; magit-head
 ;; magit-header-line
 ;; magit-header-line-key
 ;; magit-header-line-log-select
 ;; magit-keyword
 ;; magit-keyword-squash
 ;; magit-left-margin
 ;; magit-log-author
 ;; magit-log-date
 ;; magit-log-graph
 ;; magit-mode-line-process
 ;; magit-mode-line-process-error
 ;; magit-process-ng
 ;; magit-process-ok
 ;; magit-reflog-amend
 ;; magit-reflog-checkout
 ;; magit-reflog-cherry-pick
 ;; magit-reflog-commit
 ;; magit-reflog-merge
 ;; magit-reflog-other
 ;; magit-reflog-rebase
 ;; magit-reflog-remote
 ;; magit-reflog-reset
 ;; magit-refname
 ;; magit-refname-pullreq
 ;; magit-refname-stash
 ;; magit-refname-wip
 ;; magit-section-child-count
 ;; `(magit-section-heading ((t (:foreground ,wusticality-yellow :bold t))))
 ;; magit-section-heading-selection
 ;; magit-section-highlight
 ;; magit-section-secondary-heading
 ;; magit-sequence-done
 ;; magit-sequence-drop
 ;; magit-sequence-exec
 ;; magit-sequence-head
 ;; magit-sequence-onto
 ;; magit-sequence-part
 ;; magit-sequence-pick
 ;; magit-sequence-stop
 ;; magit-signature-bad
 ;; magit-signature-error
 ;; magit-signature-expired
 ;; magit-signature-expired-key
 ;; magit-signature-good
 ;; magit-signature-revoked
 ;; magit-signature-untrusted
 ;; magit-tag

 ;;
 ;; git-commit
 ;;

 ;; git-commit-comment-action
 ;; git-commit-comment-branch-local
 ;; git-commit-comment-branch-remote
 ;; git-commit-comment-button
 ;; git-commit-comment-detached
 ;; git-commit-comment-file
 ;; git-commit-comment-heading
 ;; git-commit-keyword
 ;; git-commit-nonempty-second-line
 ;; git-commit-overlong-summary
 ;; git-commit-summary
 ;; git-commit-trailer-token
 ;; git-commit-trailer-value

 ;;
 ;; git-rebase
 ;;

 ;; git-rebase-action
 ;; git-rebase-comment-hash
 ;; git-rebase-comment-heading
 ;; git-rebase-description
 ;; git-rebase-hash
 ;; git-rebase-killed-action
 ;; git-rebase-label

 ;;
 ;; company
 ;;

 ;; company-echo
 ;; company-echo-common
 ;; company-preview
 ;; company-preview-common
 ;; company-preview-search
 ;; company-template-field
 ;; company-tooltip
 ;; company-tooltip-annotation
 ;; company-tooltip-annotation-selection
 ;; company-tooltip-common
 ;; company-tooltip-common-selection
 ;; company-tooltip-deprecated
 ;; company-tooltip-mouse
 ;; company-tooltip-quick-access
 ;; company-tooltip-quick-access-selection
 ;; company-tooltip-scrollbar-thumb
 ;; company-tooltip-scrollbar-track
 ;; company-tooltip-search
 ;; company-tooltip-search-selection
 ;; company-tooltip-selection

 ;;
 ;; company-box
 ;;

 ;; company-box-annotation
 ;; company-box-background
 ;; company-box-candidate
 ;; company-box-numbers
 ;; company-box-scrollbar
 ;; company-box-selection

 ;;
 ;; yasnippet
 ;;

 ;; yas--field-debug-face
 ;; yas-field-highlight-face

 ;;
 ;; gptel
 ;;

 `(gptel-context-highlight-face ((t nil)))
 `(gptel-context-deletion-face ((t nil)))

 ;;
 ;; hl-todo
 ;;

 `(hl-todo ((t (:weight bold :foreground ,wusticality-orange))))
 ;; hl-todo-flymake-type

 ;;
 ;; dired
 ;;

 ;; dired-broken-symlink
 ;; dired-directory
 ;; dired-flagged
 ;; dired-header
 ;; dired-ignored
 ;; dired-mark
 ;; dired-marked
 ;; dired-perm-write
 ;; dired-set-id
 ;; dired-special
 ;; dired-symlink
 ;; dired-warning

 ;;
 ;; rainbow-delimiters
 ;;

 ;; rainbow-delimiters-base-error-face
 ;; rainbow-delimiters-base-face
 ;; rainbow-delimiters-mismatched-face
 ;; rainbow-delimiters-unmatched-face

 ;;
 ;; ace-window
 ;;

 ;; aw-background-face
 ;; aw-key-face
 ;; aw-leading-char-face
 ;; aw-minibuffer-leading-char-face
 ;; aw-mode-line-face

 ;;
 ;; avy
 ;;

 ;; avy-background-face
 ;; avy-goto-char-timer-face
 ;; avy-lead-face
 ;; avy-lead-face-0
 ;; avy-lead-face-1
 ;; avy-lead-face-2

 ;;
 ;; which
 ;;

 ;; which-func
 ;; which-key-command-description-face
 ;; which-key-docstring-face
 ;; which-key-group-description-face
 ;; which-key-highlighted-command-face
 ;; which-key-key-face
 ;; which-key-local-map-description-face
 ;; which-key-note-face
 ;; which-key-separator-face
 ;; which-key-special-key-face

 ;;
 ;; erc
 ;;

 ;; erc-action-face
 ;; erc-bold-face
 ;; erc-button
 ;; erc-button-nick-default-face
 ;; erc-capab-identify-unidentified
 ;; erc-command-indicator-face
 ;; `(erc-current-nick-face ((t (:foreground ,wusticality-blue :bold t))))
 ;; erc-dangerous-host-face
 ;; `(erc-default-face ((t (:foreground ,wusticality-foreground))))
 ;; `(erc-direct-msg-face ((t (:foreground ,wusticality-pink))))
 ;; erc-error-face
 ;; erc-fill-wrap-merge-indicator-face
 ;; erc-fool-face
 ;; erc-header-line
 ;; erc-information
 ;; `(erc-input-face ((t (:foreground ,wusticality-orange))))
 ;; erc-inverse-face
 ;; erc-italic-face
 ;; erc-keep-place-indicator-arrow
 ;; erc-keep-place-indicator-line
 ;; erc-keyword-face
 ;; erc-my-nick-face
 ;; erc-my-nick-prefix-face
 ;; `(erc-nick-default-face ((t (:foreground ,wusticality-purple :bold t))))
 ;; erc-nick-msg-face
 ;; erc-nick-prefix-face
 ;; `(erc-notice-face ((t (:foreground ,wusticality-brown))))
 ;; erc-pal-face
 ;; `(erc-prompt-face ((t (:foreground ,wusticality-yellow :bold t))))
 ;; erc-spoiler-face
 ;; `(erc-timestamp-face ((t (:foreground ,wusticality-green))))
 ;; erc-underline-face

 ;;
 ;; vterm
 ;;

 ;; vterm-color-black
 ;; vterm-color-blue
 ;; vterm-color-bright-black
 ;; vterm-color-bright-blue
 ;; vterm-color-bright-cyan
 ;; vterm-color-bright-green
 ;; vterm-color-bright-magenta
 ;; vterm-color-bright-red
 ;; vterm-color-bright-white
 ;; vterm-color-bright-yellow
 ;; vterm-color-cyan
 ;; vterm-color-green
 ;; vterm-color-inverse-video
 ;; vterm-color-magenta
 ;; vterm-color-red
 ;; vterm-color-underline
 ;; vterm-color-white
 ;; vterm-color-yellow

 ;;
 ;; rust
 ;;

 ;; rust-ampersand-face
 ;; rust-builtin-formatting-macro
 ;; rust-question-mark
 ;; rust-string-interpolation
 ;; rust-unsafe

 ;;
 ;; rustic
 ;;

 ;; rustic-cargo-outdated
 ;; rustic-cargo-outdated-upgrade
 ;; rustic-compilation-column
 ;; rustic-compilation-error
 ;; rustic-compilation-info
 ;; rustic-compilation-line
 ;; rustic-compilation-warning
 ;; rustic-errno-face
 ;; rustic-message
 ;; rustic-popup-key
 ;; rustic-popup-section
 ;; rustic-racer-help-heading
 ;; rustic-racer-tooltip

 ;;
 ;; markdown
 ;;

 ;; markdown-blockquote-face
 ;; markdown-bold-face
 ;; markdown-code-face
 ;; markdown-comment-face
 ;; markdown-footnote-marker-face
 ;; markdown-footnote-text-face
 ;; markdown-gfm-checkbox-face
 ;; markdown-header-delimiter-face
 ;; markdown-header-face
 ;; markdown-header-face-1
 ;; markdown-header-face-2
 ;; markdown-header-face-3
 ;; markdown-header-face-4
 ;; markdown-header-face-5
 ;; markdown-header-face-6
 ;; markdown-header-rule-face
 ;; markdown-highlight-face
 ;; markdown-highlighting-face
 ;; markdown-hr-face
 ;; markdown-html-attr-name-face
 ;; markdown-html-attr-value-face
 ;; markdown-html-entity-face
 ;; markdown-html-tag-delimiter-face
 ;; markdown-html-tag-name-face
 ;; markdown-inline-code-face
 ;; markdown-italic-face
 ;; markdown-language-info-face
 ;; markdown-language-keyword-face
 ;; markdown-line-break-face
 ;; markdown-link-face
 ;; markdown-link-title-face
 ;; markdown-list-face
 ;; markdown-markup-face
 ;; markdown-math-face
 ;; markdown-metadata-key-face
 ;; markdown-metadata-value-face
 ;; markdown-missing-link-face
 ;; markdown-plain-url-face
 ;; markdown-pre-face
 ;; markdown-reference-face
 ;; markdown-strike-through-face
 ;; markdown-table-face
 ;; markdown-url-face

 ;;
 ;; glsl-mode
 ;;

 ;; glsl-builtins-face
 ;; glsl-deprecated-builtins-face
 ;; glsl-deprecated-keyword-face
 ;; glsl-deprecated-variable-name-face
 ;; glsl-extension-face
 ;; glsl-keyword-face
 ;; glsl-preprocessor-face
 ;; glsl-qualifier-face
 ;; glsl-reserved-keyword-face
 ;; glsl-shader-variable-name-face
 ;; glsl-type-face
 ;; glsl-variable-name-face

 ;;
 ;; haskell-mode
 ;;

 ;; haskell-c2hs-hook-name-face
 ;; haskell-c2hs-hook-pair-face
 ;; haskell-constructor-face
 ;; haskell-debug-heading-face
 ;; haskell-debug-keybinding-face
 ;; haskell-debug-muted-face
 ;; haskell-debug-newline-face
 ;; haskell-debug-trace-number-face
 ;; haskell-debug-warning-face
 ;; haskell-definition-face
 ;; haskell-error-face
 ;; haskell-hole-face
 ;; haskell-interactive-face-compile-error
 ;; haskell-interactive-face-compile-warning
 ;; haskell-interactive-face-garbage
 ;; haskell-interactive-face-prompt
 ;; haskell-interactive-face-prompt-cont
 ;; haskell-interactive-face-result
 ;; haskell-keyword-face
 ;; haskell-liquid-haskell-annotation-face
 ;; haskell-literate-comment-face
 ;; haskell-operator-face
 ;; haskell-pragma-face
 ;; haskell-quasi-quote-face
 ;; haskell-type-face
 ;; haskell-warning-face

 ;;
 ;; web-mode
 ;;

 ;; web-mode-block-attr-name-face
 ;; web-mode-block-attr-value-face
 ;; web-mode-block-comment-face
 ;; web-mode-block-control-face
 ;; web-mode-block-delimiter-face
 ;; web-mode-block-face
 ;; web-mode-block-string-face
 ;; web-mode-bold-face
 ;; web-mode-builtin-face
 ;; web-mode-comment-face
 ;; web-mode-comment-keyword-face
 ;; web-mode-constant-face
 ;; web-mode-css-at-rule-face
 ;; web-mode-css-color-face
 ;; web-mode-css-comment-face
 ;; web-mode-css-function-face
 ;; web-mode-css-priority-face
 ;; web-mode-css-property-name-face
 ;; web-mode-css-pseudo-class-face
 ;; web-mode-css-selector-face
 ;; web-mode-css-string-face
 ;; web-mode-css-variable-face
 ;; web-mode-current-column-highlight-face
 ;; web-mode-current-element-highlight-face
 ;; web-mode-doctype-face
 ;; web-mode-error-face
 ;; web-mode-filter-face
 ;; web-mode-folded-face
 ;; web-mode-function-call-face
 ;; web-mode-function-name-face
 ;; web-mode-html-attr-custom-face
 ;; web-mode-html-attr-engine-face
 ;; web-mode-html-attr-equal-face
 ;; `(web-mode-html-attr-name-face ((t (:foreground ,wusticality-purple :bold t))))
 ;; web-mode-html-attr-value-face
 ;; web-mode-html-entity-face
 ;; web-mode-html-tag-bracket-face
 ;; web-mode-html-tag-custom-face
 ;; `(web-mode-html-tag-face ((t (:foreground ,wusticality-pink :bold t))))
 ;; `(web-mode-html-tag-namespaced-face ((t (:foreground ,wusticality-pink :bold t))))
 ;; web-mode-html-tag-face
 ;; web-mode-html-tag-namespaced-face
 ;; web-mode-inlay-face
 ;; web-mode-italic-face
 ;; web-mode-javascript-comment-face
 ;; web-mode-javascript-string-face
 ;; web-mode-json-comment-face
 ;; web-mode-json-context-face
 ;; web-mode-json-key-face
 ;; web-mode-json-string-face
 ;; web-mode-jsx-depth-1-face
 ;; web-mode-jsx-depth-2-face
 ;; web-mode-jsx-depth-3-face
 ;; web-mode-jsx-depth-4-face
 ;; web-mode-keyword-face
 ;; web-mode-param-name-face
 ;; web-mode-part-comment-face
 ;; web-mode-part-face
 ;; web-mode-part-string-face
 ;; web-mode-preprocessor-face
 ;; web-mode-script-face
 ;; web-mode-sql-keyword-face
 ;; web-mode-string-face
 ;; web-mode-style-face
 ;; web-mode-symbol-face
 ;; web-mode-type-face
 ;; web-mode-underline-face
 ;; web-mode-variable-name-face
 ;; web-mode-warning-face
 ;; web-mode-whitespace-face

 ;;
 ;; org
 ;;

 ;; org-agenda-calendar-event
 ;; org-agenda-calendar-sexp
 ;; org-agenda-clocking
 ;; org-agenda-column-dateline
 ;; org-agenda-current-time
 ;; org-agenda-date
 ;; org-agenda-date-today
 ;; org-agenda-date-weekend
 ;; org-agenda-date-weekend-today
 ;; org-agenda-diary
 ;; org-agenda-dimmed-todo-face
 ;; org-agenda-done
 ;; org-agenda-filter-category
 ;; org-agenda-filter-effort
 ;; org-agenda-filter-regexp
 ;; org-agenda-filter-tags
 ;; org-agenda-restriction-lock
 ;; org-agenda-structure
 ;; org-agenda-structure-filter
 ;; org-agenda-structure-secondary
 ;; org-archived
 ;; org-block
 ;; org-block-begin-line
 ;; org-block-end-line
 ;; org-checkbox
 ;; org-checkbox-statistics-done
 ;; org-checkbox-statistics-todo
 ;; org-cite
 ;; org-cite-key
 ;; org-clock-overlay
 ;; org-code
 ;; org-column
 ;; org-column-title
 ;; org-date
 ;; org-date-selected
 ;; org-default
 ;; org-dispatcher-highlight
 ;; org-document-info
 ;; org-document-info-keyword
 ;; org-document-title
 ;; org-done
 ;; org-drawer
 ;; org-ellipsis
 ;; org-footnote
 ;; org-formula
 ;; org-headline-done
 ;; org-headline-todo
 ;; org-hide
 ;; org-imminent-deadline
 ;; org-latex-and-related
 ;; org-level-1
 ;; org-level-2
 ;; org-level-3
 ;; org-level-4
 ;; org-level-5
 ;; org-level-6
 ;; org-level-7
 ;; org-level-8
 ;; org-link
 ;; org-list-dt
 ;; org-macro
 ;; org-meta-line
 ;; org-mode-line-clock
 ;; org-mode-line-clock-overrun
 ;; org-priority
 ;; org-property-value
 ;; org-quote
 ;; org-scheduled
 ;; org-scheduled-previously
 ;; org-scheduled-today
 ;; org-sexp-date
 ;; org-special-keyword
 ;; org-table
 ;; org-table-header
 ;; org-tag
 ;; org-tag-group
 ;; org-target
 ;; org-time-grid
 ;; org-todo
 ;; org-upcoming-deadline
 ;; org-upcoming-distant-deadline
 ;; org-verbatim
 ;; org-verse
 ;; org-warning
 )

(provide-theme 'wusticality)

;;; wusticality-theme.el ends here
