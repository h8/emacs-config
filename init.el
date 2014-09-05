;; Package management
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives '("ess" . "http://kieranhealy.org/packages/") t)

(package-initialize)

;; Config load paths
(defvar emacs-dir (file-name-directory load-file-name) "top level emacs dir")
(defvar conf-edit-dir (concat emacs-dir "edit.d") "edit helpers")
(defvar lib-dir (concat emacs-dir "lib.d") "3rd party libs")
(defvar widget-dir (concat emacs-dir "widget.d") "visual widgets")

(add-to-list 'load-path lib-dir)

(load (concat emacs-dir "load-directory.el"))
(load-directory conf-edit-dir)
(load-directory widget-dir)

