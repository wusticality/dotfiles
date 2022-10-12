;;; wusticality-theme.el --- wusticality theme

(deftheme wusticality "My own flavor of One Dark Vivid Italic")

(let ((c-bg "#282c34")
      (c-mid-gray "#444444")
      (c-light-gray "#4e4e4e")
      (c-fg "#bbbbbb")
      (c-red "#870000")
      (c-black "#080808")
      (c-yellow "#ffff87")
      (c-green "#89c078")
      (c-blue "#61afef")
      (c-dark-blue "#005faf")
      (c-orange "#e5c07b")
      (c-pink "#d55fde")
      (c-purple "#af87ff")
      (c-brown "#5c6370")
      (c-old "#af875f"))
  ;; The cider headerline.
  (add-hook
   'cider-mode-hook
   (lambda ()
     (setq
      header-line-format
      '((:eval (propertize "%* " 'face '(:foreground "#00cd00" :weight bold)))
        (:eval (propertize (cider-current-ns) 'face '(:foreground "#ffff87" :weight bold)))))))

  ;; The modeline.
  (setq-default
   mode-line-format
   '((:eval (propertize "%* " 'face '(:foreground "#00cd00" :weight bold)))
     (:eval (propertize "%b " 'face '(:foreground "#ffff87" :weight bold)))
     (:eval (propertize (concat
                         (format "%s"
                                 (round
                                  (* 100.0
                                     (/ (float (1- (point)))
                                        (float (1- (point-max)))))))
                         "%% ")
                        'face '(:foreground "#00cd00" :weight bold)))
     (:eval (propertize "(%l, %c) " 'face '(:foreground "#ff8700" :weight bold)))
     (:eval (propertize "%m " 'face '(:foreground "#af87ff" :weight bold)))
     (:eval
      (when vc-mode
        (propertize
         (s-trim (substring-no-properties vc-mode))
         'face
         '(:foreground "#00a7ff" :weight bold))))))
  ;; The theme.
  (custom-theme-set-faces
   'wusticality

   ;;
   ;; defaults
   ;;

   `(default ((t (:background ,c-bg :foreground ,c-fg))))
   `(cursor ((t (:background ,c-blue))))
   `(mode-line ((t (:background ,c-light-gray :foreground ,c-fg :bold t :box nil))))
   `(mode-line-inactive ((t (:background ,c-light-gray :foreground ,c-fg :bold t :box nil))))
   ;; mode-line-buffer-id
   ;; mode-line-emphasis
   ;; mode-line-highlight
   `(header-line ((t (:background ,c-light-gray :foreground ,c-fg :bold t))))
   `(minibuffer-prompt ((t (:foreground ,c-blue :bold t))))
   `(trailing-whitespace ((t (:background ,c-bg :foreground ,c-fg))))
   `(highlight ((t (:background ,c-red))))
   `(region ((t (:background ,c-black))))
   `(lazy-highlight ((t (:background ,c-dark-blue))))
   `(isearch ((t (:background ,c-black))))
   `(query-replace ((t (:background ,c-black))))
   `(show-paren-match ((t (:background ,c-green))))
   `(show-paren-mismatch ((t (:background ,c-green))))

   ;;
   ;; font lock
   ;;

   `(font-lock-warning-face ((t (:underline t))))
   `(font-lock-string-face ((t (:foreground ,c-green))))
   `(font-lock-comment-face ((t (:foreground ,c-brown))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,c-brown))))
   `(font-lock-doc-face ((t (:foreground ,c-brown))))
   `(font-lock-keyword-face ((t (:foreground ,c-pink))))
   `(font-lock-preprocessor-face ((t (:foreground ,c-pink))))
   `(font-lock-function-name-face ((t (:foreground ,c-blue))))
   `(font-lock-constant-face ((t (:foreground ,c-purple))))
   `(font-lock-builtin-face ((t (:foreground ,c-purple))))
   `(font-lock-type-face ((t (:foreground ,c-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,c-orange))))

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
   ;; helm
   ;;

   ;; helm-M-x-key
   ;; helm-action
   ;; helm-ag-edit-deleted-line
   ;; helm-bookmark-addressbook
   ;; helm-bookmark-directory
   ;; helm-bookmark-file
   ;; helm-bookmark-file-not-found
   ;; helm-bookmark-gnus
   ;; helm-bookmark-info
   ;; helm-bookmark-man
   ;; helm-bookmark-w3m
   `(helm-buffer-directory ((t (:foreground ,c-pink :bold t))))
   `(helm-buffer-file ((t (:foreground ,c-purple :bold t))))
   `(helm-buffer-modified ((t (:foreground ,c-green :bold t))))
   ;; helm-buffer-not-saved
   `(helm-buffer-process ((t (:foreground ,c-brown))))
   ;; helm-buffer-saved-out
   `(helm-buffer-size ((t (:foreground ,c-blue))))
   ;; helm-candidate-number
   ;; helm-candidate-number-suspended
   ;; helm-etags-file
   `(helm-ff-directory ((t (:foreground ,c-pink :bold t))))
   ;; helm-ff-dirs
   `(helm-ff-dotted-directory ((t (:foreground ,c-pink :bold t))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,c-yellow :bold t))))
   `(helm-ff-executable ((t (:foreground ,c-blue :bold t))))
   `(helm-ff-file ((t (:foreground ,c-fg))))
   `(helm-ff-invalid-symlink ((t (:foreground ,c-yellow :bold t))))
   `(helm-ff-prefix ((t (:foreground ,c-fg))))
   `(helm-ff-symlink ((t (:foreground ,c-yellow :bold t))))
   `(helm-grep-cmd-line ((t (:foreground ,c-blue :bold t))))
   `(helm-grep-file ((t (:foreground ,c-purple :bold t))))
   `(helm-grep-finish ((t (:foreground ,c-blue :bold t))))
   `(helm-grep-lineno ((t (:foreground ,c-orange))))
   `(helm-grep-match ((t (:background ,c-dark-blue))))
   ;; helm-header
   ;; helm-header-line-left-margin
   ;; helm-helper
   ;; helm-history-deleted
   ;; helm-history-remote
   ;; helm-lisp-completion-info
   ;; helm-lisp-show-completion
   ;; helm-locate-finish
   `(helm-ls-git-added-copied-face ((t (:foreground ,c-green :bold t))))
   `(helm-ls-git-added-modified-face ((t (:foreground ,c-green :bold t))))
   `(helm-ls-git-conflict-face ((t (:foreground ,c-orange :bold t))))
   `(helm-ls-git-deleted-and-staged-face ((t (:foreground ,c-pink :bold t))))
   `(helm-ls-git-deleted-not-staged-face ((t (:foreground ,c-pink :bold t))))
   `(helm-ls-git-modified-and-staged-face ((t (:foreground ,c-green :bold t))))
   `(helm-ls-git-modified-not-staged-face ((t (:foreground ,c-green :bold t))))
   `(helm-ls-git-renamed-modified-face ((t (:foreground ,c-green :bold t))))
   `(helm-ls-git-untracked-face ((t (:foreground ,c-old :bold t))))
   ;; helm-match
   ;; helm-match-item
   ;; helm-moccur-buffer
   ;; helm-non-file-buffer
   ;; helm-prefarg
   ;; helm-resume-need-update
   `(helm-selection ((t (:background ,c-red))))
   ;; helm-selection-line
   ;; helm-separator
   `(helm-source-header ((t (:background ,c-dark-blue :bold t))))
   ;; helm-swoop-line-number-face
   ;; helm-swoop-target-line-block-face
   `(helm-swoop-target-line-face ((t (:background ,c-red))))
   `(helm-swoop-target-word-face ((t (:background ,c-dark-blue))))
   `(helm-visible-mark ((t (:background ,c-black))))

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
   ;; magit-diff-added                                      
   ;; magit-diff-added-highlight                            
   ;; magit-diff-base                                       
   ;; magit-diff-base-highlight                             
   ;; magit-diff-conflict-heading                           
   ;; magit-diff-context                                    
   ;; magit-diff-context-highlight                          
   ;; magit-diff-file-heading                               
   ;; magit-diff-file-heading-highlight                     
   ;; magit-diff-file-heading-selection                     
   ;; magit-diff-hunk-heading                               
   ;; magit-diff-hunk-heading-highlight                     
   ;; magit-diff-hunk-heading-selection                     
   ;; magit-diff-hunk-region                                
   ;; magit-diff-lines-boundary                             
   ;; magit-diff-lines-heading                              
   ;; magit-diff-our                                        
   ;; magit-diff-our-highlight                              
   ;; magit-diff-removed                                    
   ;; magit-diff-removed-highlight                          
   ;; magit-diff-revision-summary                           
   ;; magit-diff-revision-summary-highlight                 
   ;; magit-diff-their                                      
   ;; magit-diff-their-highlight                            
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
   ;; magit-section-heading                                 
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
   ;; magit-cherry-equivalent
   ;; magit-cherry-unmatched
   `(magit-diff-added ((t (:foreground ,c-green :bold t))))
   `(magit-diff-added-highlight ((t (:foreground ,c-green :bold t))))
   ;; magit-diff-base
   ;; magit-diff-base-highlight
   ;; magit-diff-conflict-heading
   ;; magit-diff-context
   ;; magit-diff-context-highlight
   `(magit-diff-file-heading ((t (:foreground ,c-fg))))
   `(magit-diff-file-heading-highlight ((t (:foreground ,c-purple :bold t))))
   `(magit-diff-file-heading-selection ((t (:foreground ,c-old :bold t))))
   `(magit-diff-hunk-heading ((t (:background ,c-mid-gray :foreground ,c-fg))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,c-light-gray :foreground ,c-purple :bold t))))
   `(magit-diff-hunk-heading-selection ((t (:background ,c-light-gray :foreground ,c-old :bold t))))
   `(magit-diff-hunk-region ((t (:background ,c-black))))
   `(magit-diff-lines-boundary ((t (:background ,c-light-gray :foreground ,c-purple :bold t))))
   `(magit-diff-lines-heading ((t (:background ,c-light-gray :foreground ,c-purple :bold t))))
   ;; magit-diff-our
   ;; magit-diff-our-highlight
   `(magit-diff-removed ((t (:foreground ,c-pink :bold t))))
   `(magit-diff-removed-highlight ((t (:foreground ,c-pink :bold t))))
   ;; magit-diff-their
   ;; magit-diff-their-highlight
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
   ;; magit-log-author
   ;; magit-log-date
   ;; magit-log-graph
   ;; magit-mode-line-process
   ;; magit-mode-line-process-error
   ;; magit-popup-argument
   ;; magit-popup-disabled-argument
   ;; magit-popup-heading
   ;; magit-popup-key
   ;; magit-popup-option-value
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
   `(magit-section-heading ((t (:foreground ,c-yellow :bold t))))
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
   ;; lsp
   ;;
   
   ;; lsp-details-face                                      
   ;; lsp-face-highlight-read                               
   ;; lsp-face-highlight-textual                            
   ;; lsp-face-highlight-write                              
   ;; lsp-face-rename                                       
   ;; lsp-face-semhl-class                                  
   ;; lsp-face-semhl-comment                                
   ;; lsp-face-semhl-constant                               
   ;; lsp-face-semhl-default-library                        
   ;; lsp-face-semhl-definition                             
   ;; lsp-face-semhl-deprecated                             
   ;; lsp-face-semhl-enum                                   
   ;; lsp-face-semhl-event                                  
   ;; lsp-face-semhl-function                               
   ;; lsp-face-semhl-implementation                         
   ;; lsp-face-semhl-interface                              
   ;; lsp-face-semhl-keyword                                
   ;; lsp-face-semhl-label                                  
   ;; lsp-face-semhl-macro                                  
   ;; lsp-face-semhl-member                                 
   ;; lsp-face-semhl-method                                 
   ;; lsp-face-semhl-namespace                              
   ;; lsp-face-semhl-number                                 
   ;; lsp-face-semhl-operator                               
   ;; lsp-face-semhl-parameter                              
   ;; lsp-face-semhl-property                               
   ;; lsp-face-semhl-regexp                                 
   ;; lsp-face-semhl-static                                 
   ;; lsp-face-semhl-string                                 
   ;; lsp-face-semhl-struct                                 
   ;; lsp-face-semhl-type                                   
   ;; lsp-face-semhl-type-parameter                         
   ;; lsp-face-semhl-variable                               
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
   ;; lsp-installation-buffer-face                          
   ;; lsp-installation-finished-buffer-face                 
   ;; lsp-javascript-inlay-face                             
   ;; lsp-javascript-inlay-parameter-face                   
   ;; lsp-javascript-inlay-type-face                        
   ;; lsp-modeline-code-actions-face                        
   ;; lsp-modeline-code-actions-preferred-face              
   ;; lsp-rename-placeholder-face                           
   ;; lsp-rust-analyzer-inlay-face                          
   ;; lsp-rust-analyzer-inlay-param-face                    
   ;; lsp-rust-analyzer-inlay-type-face                     
   ;; lsp-signature-face                                    
   ;; lsp-signature-posframe                                

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
   ;; erc-command-indicator-face
   `(erc-current-nick-face ((t (:foreground ,c-blue :bold t))))
   ;; erc-dangerous-host-face
   `(erc-default-face ((t (:foreground ,c-fg))))
   `(erc-direct-msg-face ((t (:foreground ,c-pink))))
   ;; erc-error-face
   ;; erc-fool-face
   ;; erc-header-line
   `(erc-input-face ((t (:foreground ,c-orange))))
   ;; erc-inverse-face
   ;; erc-keyword-face
   ;; erc-my-nick-face
   ;; erc-my-nick-prefix-face
   `(erc-nick-default-face ((t (:foreground ,c-purple :bold t))))
   ;; erc-nick-msg-face
   ;; erc-nick-prefix-face
   `(erc-notice-face ((t (:foreground ,c-brown))))
   ;; erc-pal-face
   `(erc-prompt-face ((t (:foreground ,c-yellow :bold t))))
   `(erc-timestamp-face ((t (:foreground ,c-green))))
   ;; erc-underline-face

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
   `(web-mode-html-attr-name-face ((t (:foreground ,c-purple :bold t))))
   ;; web-mode-html-attr-value-face
   ;; web-mode-html-entity-face
   ;; web-mode-html-tag-bracket-face
   ;; web-mode-html-tag-custom-face
   `(web-mode-html-tag-face ((t (:foreground ,c-pink :bold t))))
   `(web-mode-html-tag-namespaced-face ((t (:foreground ,c-pink :bold t))))
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
   ))

(provide-theme 'wusticality)
