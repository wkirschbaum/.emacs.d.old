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
