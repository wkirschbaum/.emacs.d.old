;;; Packages -- My packages

;;; Commentary:
;; This is my personal configuration.

;;; Code:


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package flx :straight t)
(use-package amx :straight t)

(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :straight t)

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (setq dired-dwim-target t))

(use-package which-key
  :straight t
  :config
  (which-key-mode 1))

(use-package define-word
  :straight t)

(use-package whitespace
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs tab-mark trailing empty)))

(use-package git-timemachine
  :straight t
  :defer t)

(use-package magit
  :straight t
  :demand t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-repository-directories '(("~/projects/" . 2))
        magit-revision-show-gravatars 'author))

(use-package forge
  :straight t)

(use-package diff-hl
  :straight t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package ag
  :straight t
  :commands (ag ag-regexp ag-project))

(use-package counsel
  :straight t
  :bind(("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-c k" . counsel-ag)
        ("M-i" . counsel-imenu)))

(use-package ivy
  :straight t
  :bind(("C-c r" . ivy-resume))
  :config
  (setq ivy-count-format "(%d/%d) "
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil ;; no default regex
        ivy-use-virtual-buffers t
        magit-completing-read-function 'ivy-completing-read)
  (ivy-mode 1))

(use-package projectile
  :straight t
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-file-exists-remote-cache-expire nil
        projectile-sort-order 'recently-active
        projectile-dynamic-mode-line t
        projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name)))
        projectile-indexing-method 'hybrid)
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))


(use-package inf-ruby
  :straight t)

(use-package f
  :straight t)

(use-package rake
  :straight t)

(use-package inflections
  :straight t)

(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

(use-package ibuffer
  :bind(("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-display-summary nil)
  :hook ibuffer . (lambda ()
                    (ibuffer-projectile-set-filter-groups)
                    (unless (eq ibuffer-sorting-mode 'recency)
                      (ibuffer-do-sort-by-recency))))

(use-package ibuffer-projectile
  :straight t
  :after (projectile ibuffer)
  :config)

(use-package minions
  :straight t
  :config
  (setq minions-mode-line-lighter "{*}"
        minions-direct '(projectile-mode))
  (minions-mode 1))

(use-package company
  :straight t
  :config
  (global-company-mode t))

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; erc
(use-package erc
  :config
  (setq-default erc-echo-notices-in-minibuffer-flag t)
  (setq erc-rename-buffers t
        erc-lurker-hide-list '("JOIN" "QUIT")
        erc-lurker-threshold-time 3600
        erc-input-line-position -2))

(use-package yasnippet  :straight t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1
        yas-wrap-around-region t))

(use-package yasnippet-snippets
  :straight t)

(use-package exec-path-from-shell
    :straight t
    :config
    (exec-path-from-shell-initialize))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode
  :straight t)

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config))

(use-package markdown-mode
  :straight t
  :config
  (add-hook 'markdown-mode-hook #'toggle-word-wrap)
  (setq markdown-command "/usr/bin/pandoc"))

(use-package markdown-preview-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package web-mode
  :straight t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode)
         )
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t))

(add-hook 'prog-mode (lambda () (setq truncate-lines t)))

(use-package emmet-mode
  :straight t
  :hook web-mode
  :hook css-mode)

(use-package bundler
  :straight t)

;; Ruby and Rails
(use-package inf-ruby
  :straight t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package inflections
  :straight t)

(use-package rspec-mode
  :straight t
  :config
  (setq-default rspec-use-spring-when-possible t)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package robe
  :straight t
  :config
  (global-robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package rubocop :straight t)
(use-package feature-mode :straight t) ;; cucumber

(use-package alchemist :straight t)
(use-package terraform-mode :straight t)
;; END

;; Python
(use-package elpy
  :straight t
  :config
  (elpy-enable))

(use-package dashboard
  :straight t
  :config
  (setq dashboard-banner-logo-title "Focus on using yasnippets this week!"
        dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-items '((agenda . 10)
                          (bookmarks . 5)
                          (projects . 5))
        show-week-agenda-p t)
  (dashboard-setup-startup-hook))

(use-package nyan-mode
  :straight t
  :config
  (setq nyan-wavy-trail t
        nyan-bar-length 24)
  (nyan-mode))

(use-package ledger-mode
  :straight t)

(use-package groovy-mode
  :straight t)

(use-package elfeed
  :straight t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://www.ruby-lang.org/en/feeds/news.rss"
          "https://rubyweekly.com/rss"
          "https://wilhelmbot.com/feed.xml")))

(use-package helpful
  :straight t
  :bind(("C-h f" . helpful-callable)
        ("C-h v" . helpful-variable)
        ("C-h k" . helpful-key)
        ("C-c C-d" . helpful-at-point)
        ("C-h C" . helpful-command)
        ("C-h F" . helpful-function))
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

(use-package easy-jekyll
  :straight t
  :init
  (setq easy-jekyll-basedir "~/projects/wkirschbaum/blog"
        easy-jekyll-url "https://wilhelmbot.com"
        easy-jekyll-root ""
        easy-jekyll-previewtime "300")
  :bind ("C-c C-e" . easy-jekyll))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-cache nil
        tramp-completion-reread-directory-timeout nil
        vc-ignore-dir-regexp (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package coverage
  :straight t
  :config
  :bind ("C-c , ,". coverage-mode))

(use-package keycast
  :straight t)

;; Replaces 'delete-blank-lines command
(use-package shrink-whitespace
  :straight t
  :bind ("C-x C-o" . shrink-whitespace))

(use-package restclient
  :straight t)

;;; packages.el ends here
