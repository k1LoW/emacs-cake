;;; cake.el --- CakePHP Minor Mode
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2008-2014 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 1.4.1
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: https://github.com/k1LoW/emacs-cake
;; Package-Requires: ((cake-inflector "1.1.0") (historyf "0.0.8") (anything "1.3.9"))

;; Thanks to rubikitch for using header-name(anything.el) param advice.(1.0.5)
;; Thanks to xcezx for using tail.el patch.(1.0.1)
;; Thanks to custar for "search app path" idea.(0.1.6)
;; Thanks to xcezx for directory create patch.(0.0.8)

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'cake)
;; (global-cake t)
;;
;; If you use default key map, Put the following expression into your ~/.emacs.
;;
;; (cake-set-default-keymap)

;;; YASnippet
;; If you use yasnippet, Put snippets/ into YASnippet load-directory.
;; And put the following expression before yas/initialize()
;;
;; (add-hook 'cake-hook
;;              #'(lambda ()
;;                  (setq yas/mode-symbol 'cake)))
;;

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `cake'
;;    CakePHP minor mode.
;;  `cake-switch-to-model'
;;    Switch to model.
;;  `cake-switch-to-view'
;;    Switch to view.
;;  `cake-switch-to-controller'
;;    Switch to contoroller.
;;  `cake-switch-to-model-testcase'
;;    Switch to model testcase.
;;  `cake-switch-to-controller-testcase'
;;    Switch to contoroller testcase.
;;  `cake-switch-to-fixture'
;;    Switch to fixture.
;;  `cake-switch-to-function'
;;    Switch to function.
;;  `cake-switch-to-element'
;;    Switch to element. If region is active, make new element file.
;;  `cake-switch-to-javascript'
;;    Switch to javascript.
;;  `cake-switch-to-css'
;;    Switch to stylesheet.
;;  `cake-switch'
;;    Omni switch function.
;;  `cake-switch-testcase'
;;    Switch testcase <-> C/M. Or, switch form fixture to testcase.
;;  `cake-switch-to-file-history'
;;    Switch to file history.
;;  `cake-open-dir'
;;    Open directory.
;;  `cake-open-models-dir'
;;    Open models directory.
;;  `cake-open-views-dir'
;;    Open views directory.
;;  `cake-open-all-views-dir'
;;    Open all views directory.
;;  `cake-open-controllers-dir'
;;    Open contorollers directory.
;;  `cake-open-behaviors-dir'
;;    Open behaviors directory.
;;  `cake-open-helpers-dir'
;;    Open helpers directory.
;;  `cake-open-components-dir'
;;    Open components directory.
;;  `cake-open-libs-dir'
;;    Open libs dir.
;;  `cake-open-config-dir'
;;    Open config dir.
;;  `cake-open-layouts-dir'
;;    Open layouts directory.
;;  `cake-open-elements-dir'
;;    Open elements directory.
;;  `cake-open-js-dir'
;;    Open JavaScript directory.
;;  `cake-open-css-dir'
;;    Open css directory.
;;  `cake-open-tests-dir'
;;    Open tests directory.
;;  `cake-set-version'
;;    Set CakePHP version.
;;  `cake-complete'
;;    Insert CakePHP code.
;;  `cake-tail-log'
;;    Show log by "tail".
;;  `anything-c-cake-anything-only-source-cake'
;;    anything only anything-c-source-cake and anything-c-source-cake-model-function.
;;  `anything-c-cake-anything-only-function'
;;    anything only anything-c-source-cake-function.
;;  `anything-c-cake-anything-only-model-function'
;;    anything only anything-c-source-cake-model-function.
;;  `anything-c-cake-anything-only-po'
;;    anything only anything-c-source-cake-po.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `cake-app-path-search-limit'
;;    Search limit
;;    default = 5
;;  `cake-po-file-path'
;;    Po file path.
;;    default = "jpn/LC_MESSAGES/default.po"
;;  `cake-use-imenu'
;;    Use imenu function
;;    default = nil
;;  `cake-core-version'
;;    CakePHP version
;;    default = "1.3"

;;; Change Log
;; 1.4.1: Remove cake string functions
;; 1.4.0: Move cake-infrector.el to k1LoW/emacs-cake-infrector
;; 1.3.9: Backport from cake2.el 1.0.8 cake-open-all-views-dir.
;;        Improved cake-open-layouts-dir, cake-open-elements-dir
;; 1.3.8: Backport from cake2.el 1.0.6 cake-open-dir.
;; 1.3.6: Update function cake-open-tests-dir
;; 1.3.5: Change cake-set-default-keymap.
;; 1.3.4: Change cake-set-default-keymap.
;;        New function cake-open-libs-dir.
;;        Support recursive controllers/ directory
;; 1.3.3: Fix Doc.
;;        Refactor code.
;;        Update function some cake-open-*-dir (plugin directory support).
;;        Fix bug (same name action switching)
;;        Update function anything-c-cake-switch-to-view (themed directory support)
;;        Update function cake-is-views-dir (themed directory support)
;; 1.3.2: Update function cake-open-views-dir, cake-open-layouts-dir, cake-open-elements-dir (themed directory support).
;;        Update function cake-open-dir (multi directory support).
;; 1.3.1: Fix doc.
;;        New function cake-singularize, cake-pluralize.
;;        Remove function cake-convert-singular-to-plural, cake-convert-plural-to-singular.
;;        Refactor code.
;;        Fix bug (MVC switch)
;; 1.3.0: Merge anything-c-cake.el.
;;        Remove key map. New function cake-set-default-keymap.
;; 1.2.6: Support CakePHP 1.3.
;;        Refactor code.
;;        Modify function cake-switch-to-javascript.
;;        Modify valiables cake-source-javascript, cake-source-css.
;;        Update function cake-switch-to-element. If region is active, make new element file.
;;        Use historyf.el
;; 1.2.5: New function cake-switch-to-file-history.
;;        Refactor code.
;; 1.2.4: Add YASnippet snippets.
;;        New function cake-switch-to-css. Modify cake-switch.
;;        Modifiy cake-switch.
;; 1.2.3: Update ac-cake.el.
;; 1.2.2: Update anything-c-cake.el.
;; 1.2.1: New valiables cake-hook.
;;        Fix bug (cake-snake).
;;        Refactor code.
;; 1.2.0: cake-switch-to-element bug fix.
;;        New function cake-camelize. Refactor code.
;; 1.1.9: Recursive search dir when use cake-open-views-dir
;; 1.1.8: Remove (setq max-lisp-eval-depth ) (setq max-specpdl-size )
;; 1.1.7: Modify cake-complete.
;; 1.1.6: New valiables cake-app-path-search-limit. Refactor cake-is-app-path. Fix Doc
;; 1.1.5: Modifiy cake-source-javascript, cake-source-css, cake-source-elements
;; 1.1.4: New valiables cake-use-imenu.
;; 1.1.3: Fix bug.
;; 1.1.2: Modify cake-complete.
;; 1.1.1: Fix bug.
;; 1.1.0: Use anything-show-completion.el if available.
;; 1.0.9: New function cake-complete.
;; 1.0.8: Modifiy cake-switch-to-function.
;; 1.0.7: Fix bug.
;; 1.0.6: New valiables cake-po-file-path. Fix doc.
;; 1.0.5: Set header-name to cake-open-*-dir. advice from id:rubikitch.
;; 1.0.4: cake-is-app-path bug fix.
;; 1.0.3: Modify cake-switch-testcase.
;; 1.0.2: New function cake-switch-testcase, cake-switch-to-controller-testcase, cake-switch-to-model-testcase, cake-switch-to-fixture, cake-open-tests-dir.
;; 1.0.1: Applied a patch from xcezx.
;; 1.0.0: CakePHP 1.2 Final Release!!! Great Work, CakePHP team!! cake.el 1.0.0 released, too!! use define-minor-mode. cake-set-app-path bug fix.
;; 0.2.9: cake-is-controller-fileでcake-lower-camelized-action-nameがsetqされていないことによるbug fix
;; 0.2.8: cake-open-*-dirのファイル候補を引数recursiveによりディレクトリ再帰的に検索候補にするように修正
;; 0.2.7: cake-open-config-dir関数を実装
;; 0.2.6: cake-open-behaviors-dir関数を実装。cake-open-components-dir関数を実装
;; 0.2.5: cake-switch-to-function関数の挙動修正。cake-tail-log関数 bug fix
;; 0.2.4: cake-open-views-dir関数のロジックの修正。cake-is-model-file bug fix
;; 0.2.3: cake-switch-to-javascript関数のregexp bug fix
;; 0.2.2: executable-findを導入
;; 0.2.1: cake-switch-to-function関数を実装
;; 0.2.0: cake-open-*-dirをanything.el対応
;; 0.1.9: cl導入。anything導入。views/:controller/:extension/:action.ctp上でのswitch-to-view()関数の対応
;; 0.1.8: views/:controller/:extension/:action.ctp上でのswitch-to-controller()関数の対応
;; 0.1.7: cake-switch-to-extention関数をCakePHP1.2.x.xの書き方に対応。
;; 0.1.6: custar Idea適用。appディレクトリの判定ロジックを変更。
;; 0.1.5: 任意のCakePHPプロジェクトディレクトリ名に対応
;; 0.1.4: cake-tail-logの冗長なprognを削除。
;; 0.1.3: cake-tail-log関数を実装。cake-switch-to-modelの冗長なifを削除
;; 0.1.2: cake-set-app-pathが実ファイルがないバッファのときにエラーを出していたバグを修正
;; 0.1.1: cake-switch-to-javascript関数を実装。global-set-keyをコメントアウト
;; 0.1.0: controllerのアクションがfunction xxxXxxXxx()だったときにviewファイルでxxx_xxx_xxx.ctpも探索するように修正
;; 0.0.9: viewファイルがxxx_xxx_xxx.ctpだったときにcake-switch-to-controllerでfunction xxxXxxXxx()も探索するように修正
;; 0.0.8: id:xcezx Patch適用。cake-switch-to-viewのときにディレクトリが存在しない場合に作成するように修正
;; 0.0.7: cake-open-helpers-dir,cake-open-layouts-dir関数を実装,各regexpを修正
;; 0.0.6: cake-open-*-dir関数を実装
;; 0.0.5: cake-switch-to-*関数で対象ファイルがない場合に、新規ファイルを作成するかどうかを対話的に聞くように変更
;; 0.0.4: 単数系/複数系変換用辞書ファイル実装 cake-switch-to-model関数を実装
;; 0.0.3: cake-switch-to-view関数をCakePHP1.1.x.x,1.2.x.x両方の拡張子に対応

;;; TODO
;;

;;; Code:

;;require
(require 'cake-inflector)
(require 'cl)
(require 'anything)
(require 'historyf)
(require 'easy-mmode)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'cake-complete
                                '(length cake-initial-input)))

(defgroup cake nil
  "CakePHP minor mode"
  :group 'convenience
  :prefix "cake-")

(defcustom cake-app-path-search-limit 5
  "Search limit"
  :type 'integer
  :group 'cake)

(defcustom cake-po-file-path "jpn/LC_MESSAGES/default.po"
  "Po file path."
  :type 'string
  :group 'cake)

(defcustom cake-use-imenu nil
  "Use imenu function"
  :type 'boolean
  :group 'cake)

(defcustom cake-core-version "1.3"
  "CakePHP version"
  :type 'string
  :group 'cake)

;;(global-set-key "\C-c\C-v" 'cake)

;;;###autoload
(define-minor-mode cake
  "CakePHP minor mode."
  :lighter " Cake"
  :group 'cake
  (if cake
      (progn
        (setq minor-mode-map-alist
              (cons (cons 'cake cake-key-map)
                    minor-mode-map-alist))
        (run-hooks 'cake-hook))
    nil))

;;;###autoload
(when (fboundp 'define-global-minor-mode)
  (define-global-minor-mode global-cake
    cake cake-maybe
    :group 'cake))

(defun cake-maybe ()
  "What buffer `cake' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (cake-set-app-path))
      (cake 1)
    nil))

;; key-map
(defvar cake-key-map
  (make-sparse-keymap)
  "Keymap for Cake.")

(defvar cake-app-path nil
  "CakePHP app directory path.")

(defvar cake-action-name "index"
  "CakePHP action name.")

(defvar cake-lower-camelized-action-name "index"
  "CakePHP lower camelized action name.")

(defvar cake-snake-action-name "index"
  "CakePHP snake_case action name.")

(defvar cake-view-extension "ctp"
  "CakePHP view extension.")

(defvar cake-singular-name nil
  "CakePHP current singular name.")

(defvar cake-plural-name nil
  "CakePHP current plural name.")

(defvar cake-themed-name nil
  "CakePHP current view themed name.")

(defvar cake-model-regexp "^.+/app/models/\\([^/]+\\)\.php$"
  "Model file regExp.")

(defvar cake-view-regexp "^.+/app/views/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"
  "View file regExp.")

(defvar cake-themed-regexp "^.+/app/views/themed/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"
  "View file regExp.")

(defvar cake-controller-regexp "^.+/app/controllers/\\([^/]+\\)_controller\.php$"
  "Contoroller file regExp.")

(defvar cake-behavior-regexp "^.+/app/models/behaviors/\\([^/]+\\)\.php$"
  "Behavior file regExp.")

(defvar cake-helper-regexp "^.+/app/views/helpers/\\([^/]+\\)\.php$"
  "Helper file regExp.")

(defvar cake-component-regexp "^.+/app/controllers/components/\\([^/]+\\)\.php$"
  "Component file regExp.")

(defvar cake-model-testcase-regexp "^.+/app/tests/cases/models/\\([^/]+\\)\.test\.php$"
  "Model testcase file regExp.")

(defvar cake-controller-testcase-regexp "^.+/app/tests/cases/controllers/\\([^/]+\\)_controller\.test\.php$"
  "Contoroller testcase file regExp.")

(defvar cake-fixture-regexp "^.+/app/tests/fixtures/\\([^/]+\\)_fixture\.php$"
  "Fixture file regExp.")

(defvar cake-javascript-regexp "^.+/app/webroot/js/.+\.js$"
  "JavaScript file regExp.")

(defvar cake-css-regexp "^.+/app/webroot/css/.+\.css$"
  "Css file regExp.")

(defvar cake-current-file-type nil
  "Current file type.")

(defvar cake-file-history nil
  "Switch file history.")

(defvar cake-hook nil
  "Hook")

(defun cake-set-default-keymap ()
  "Set default key-map"
  (setq cake-key-map
        (let ((map (make-sparse-keymap)))
          (define-key map "\C-cs" 'cake-switch)
          (define-key map "\C-ct" 'cake-switch-testcase)
          (define-key map "\C-cm" 'cake-switch-to-model)
          (define-key map "\C-cv" 'cake-switch-to-view)
          (define-key map "\C-cc" 'cake-switch-to-controller)
          (define-key map "\C-cx" 'cake-switch-to-fixture)
          (define-key map "\C-cf" 'cake-switch-to-function)
          (define-key map "\C-ce" 'cake-switch-to-element)
          (define-key map "\C-cj" 'cake-switch-to-javascript)
          (define-key map "\C-cb" 'cake-switch-to-file-history)
          (define-key map "\C-cM" 'cake-open-models-dir)
          (define-key map "\C-cV" 'cake-open-views-dir)
          (define-key map "\C-u\C-cV" 'cake-open-all-views-dir)
          (define-key map "\C-c\C-l" 'cake-open-layouts-dir)
          (define-key map "\C-cC" 'cake-open-controllers-dir)
          (define-key map "\C-cB" 'cake-open-behaviors-dir)
          (define-key map "\C-cH" 'cake-open-helpers-dir)
          (define-key map "\C-cP" 'cake-open-components-dir)
          (define-key map "\C-cL" 'cake-open-libs-dir)
          (define-key map "\C-cE" 'cake-open-elements-dir)
          (define-key map "\C-cJ" 'cake-open-js-dir)
          (define-key map "\C-cS" 'cake-open-css-dir)
          (define-key map "\C-cT" 'cake-open-tests-dir)
          (define-key map "\C-c\C-g" 'cake-open-config-dir)
          (define-key map "\C-c\C-t" 'cake-tail-log)
          ;; anything-functions
          (define-key map "\C-cl" 'anything-c-cake-anything-only-source-cake)
          (define-key map "\C-co" 'anything-c-cake-anything-only-function)
          (define-key map "\C-cp" 'anything-c-cake-anything-only-po)
          map)))

(defun cake-is-model-file ()
  "Check whether current file is model file."
  (cake-set-app-path)
  (if (not (string-match cake-model-regexp (buffer-file-name)))
      nil
    (setq cake-singular-name (match-string 1 (buffer-file-name)))
    (setq cake-plural-name (cake-pluralize cake-singular-name))
    (setq cake-current-file-type 'model)))

(defun cake-is-view-file ()
  "Check whether current file is view file."
  (cake-set-app-path)
  (setq cake-themed-name nil)
  (if (string-match cake-themed-regexp (buffer-file-name))
      (progn
        (setq cake-themed-name (match-string 1 (buffer-file-name)))
        (setq cake-plural-name (match-string 2 (buffer-file-name)))
        (setq cake-action-name (match-string 4 (buffer-file-name)))
        (setq cake-view-extension (match-string 5 (buffer-file-name)))
        (setq cake-lower-camelized-action-name (cake-lower-camelize cake-action-name))
        (setq cake-singular-name (cake-singularize cake-plural-name))
        (setq cake-current-file-type 'view))
    (if (not (string-match cake-view-regexp (buffer-file-name)))
        nil
      (setq cake-plural-name (match-string 1 (buffer-file-name)))
      (setq cake-action-name (match-string 3 (buffer-file-name)))
      (setq cake-view-extension (match-string 4 (buffer-file-name)))
      (setq cake-lower-camelized-action-name (cake-lower-camelize cake-action-name))
      (setq cake-singular-name (cake-singularize cake-plural-name))
      (setq cake-current-file-type 'view))))

(defun cake-is-controller-file ()
  "Check whether current file is contoroller file."
  (cake-set-app-path)
  (if (not (string-match cake-controller-regexp (buffer-file-name)))
      nil
    (setq cake-plural-name (match-string 1 (buffer-file-name)))
    (save-excursion
      (if
          (not (re-search-backward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t))
          (re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)))
    (setq cake-action-name (match-string 1))
    (setq cake-lower-camelized-action-name (cake-lower-camelize cake-action-name))
    (setq cake-snake-action-name (cake-snake cake-action-name))
    (setq cake-singular-name (cake-singularize cake-plural-name))
    (setq cake-current-file-type 'controller)))

(defun cake-is-behavior-file ()
  "Check whether current file is Behavior file."
  (cake-set-app-path)
  (if (not (string-match cake-behavior-regexp (buffer-file-name)))
      nil
    (setq cake-current-file-type 'behavior)))

(defun cake-is-helper-file ()
  "Check whether current file is Helper file."
  (cake-set-app-path)
  (if (not (string-match cake-helper-regexp (buffer-file-name)))
      nil
    (setq cake-current-file-type 'helper)))

(defun cake-is-component-file ()
  "Check whether current file is Component file."
  (cake-set-app-path)
  (if (not (string-match cake-component-regexp (buffer-file-name)))
      nil
    (setq cake-current-file-type 'component)))

(defun cake-is-model-testcase-file ()
  "Check whether current file is model testcase file."
  (cake-set-app-path)
  (if (not (string-match cake-model-testcase-regexp (buffer-file-name)))
      nil
    (setq cake-singular-name (match-string 1 (buffer-file-name)))
    (setq cake-plural-name (cake-pluralize cake-singular-name))
    (setq cake-current-file-type 'model-testcase)))

(defun cake-is-controller-testcase-file ()
  "Check whether current file is controller testcase file."
  (cake-set-app-path)
  (if (not (string-match cake-controller-testcase-regexp (buffer-file-name)))
      nil
    (setq cake-plural-name (match-string 1 (buffer-file-name)))
    (setq cake-singular-name (cake-singularize cake-plural-name))
    (setq cake-current-file-type 'controller-testcase)))

(defun cake-is-fixture-file ()
  "Check whether current file is fixture file."
  (cake-set-app-path)
  (if (not (string-match cake-fixture-regexp (buffer-file-name)))
      nil
    (setq cake-singular-name (match-string 1 (buffer-file-name)))
    (setq cake-plural-name (cake-pluralize cake-singular-name))
    (setq cake-current-file-type 'fixture)))

(defun cake-is-javascript-file ()
  "Check whether current file is JavaScript file."
  (cake-set-app-path)
  (if (not (string-match cake-javascript-regexp (buffer-file-name)))
      nil
    (setq cake-current-file-type 'javascript)))

(defun cake-is-css-file ()
  "Check whether current file is JavaScript file."
  (cake-set-app-path)
  (if (not (string-match cake-css-regexp (buffer-file-name)))
      nil
    (setq cake-current-file-type 'css)))

(defun cake-is-file ()
  "Check whether current file is CakePHP's file."
  (if (or (cake-is-model-file)
          (cake-is-controller-file)
          (cake-is-view-file)
          (cake-is-behavior-file)
          (cake-is-helper-file)
          (cake-is-component-file)
          (cake-is-javascript-file)
          (cake-is-css-file)
          (cake-is-model-testcase-file)
          (cake-is-controller-testcase-file)
          (cake-is-fixture-file))
      t nil))

(defun cake-get-current-line ()
  "Get current line."
  (thing-at-point 'line))

(defun cake-set-app-path ()
  "Set app path."
  (cake-is-app-path))

(defun cake-is-app-path ()
  "Check app directory name and set regExp."
  (setq cake-app-path (cake-find-app-path))
  (if (not cake-app-path)
      nil
    (cake-set-regexp)))

(defun cake-find-app-path ()
  "Find app directory"
  (let ((current-dir default-directory))
    (loop with count = 0
          until (file-exists-p (concat current-dir "config/core.php"))
          ;; Return nil if outside the value of
          if (= count cake-app-path-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun cake-set-regexp ()
  "Set regExp."
  (setq cake-model-regexp (concat cake-app-path "models/\\([^/]+\\)\.php"))
  (setq cake-view-regexp (concat cake-app-path "views/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"))
  (setq cake-themed-regexp (concat cake-app-path "views/themed/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"))
  (setq cake-controller-regexp (concat cake-app-path "controllers/\\([^/]+\\)_controller\.php$"))
  (setq cake-behavior-regexp (concat cake-app-path "models/behaviors/\\([^/]+\\)\.php$"))
  (setq cake-helper-regexp (concat cake-app-path "views/helpers/\\([^/]+\\)\.php$"))
  (setq cake-component-regexp (concat cake-app-path "controllers/components/\\([^/]+\\)\.php$"))
  (setq cake-model-testcase-regexp (concat cake-app-path "tests/cases/models/\\([^/]+\\)\.test\.php$"))
  (setq cake-controller-testcase-regexp (concat cake-app-path "tests/cases/controllers/\\([^/]+\\)_controller\.test\.php$"))
  (setq cake-fixture-regexp (concat cake-app-path "tests/fixtures/\\([^/]+\\)_fixture\.php$"))
  (setq cake-javascript-regexp (concat cake-app-path "webroot/js/.+\.js$"))
  (setq cake-css-regexp (concat cake-app-path "webroot/css/.+\.css$"))
  t)

(defun cake-switch-to-model ()
  "Switch to model."
  (interactive)
  (if (cake-is-file)
      (cake-switch-to-file (concat cake-app-path "models/" cake-singular-name ".php"))
    (message "Can't find model name.")))

(defun cake-switch-to-view ()
  "Switch to view."
  (interactive)
  (let ((view-files nil))
    (if (cake-is-file)
        (progn
          (if (cake-is-model-file)
              (setq cake-plural-name (cake-pluralize cake-singular-name)))
          (setq view-files (cake-set-view-list))
          (if view-files
              (cond
               ((= 1 (length view-files))
                (find-file (concat cake-app-path "views/" cake-plural-name "/" (car view-files))))
               (t (anything
                   '(((name . "Switch to view")
                      (candidates . view-files)
                      (display-to-real . (lambda (candidate)
                                           (concat cake-app-path "views/" cake-plural-name "/" candidate)
                                           ))
                      (type . file)))
                   nil nil nil nil)
                  ))
            (if (y-or-n-p "Make new file?")
                (progn
                  (unless (file-directory-p (concat cake-app-path "views/" cake-plural-name "/"))
                    (make-directory (concat cake-app-path "views/" cake-plural-name "/")))
                  (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
              (message (format "Can't find %s" (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension))))))
      (message "Can't switch to view."))))

(defun cake-set-view-list ()
  "Set view list"
  (let ((dir (concat cake-app-path "views/" cake-plural-name))
        (view-dir nil)
        (view-files nil))
    (unless (not (file-directory-p dir))
      (setq view-dir (remove-if-not (lambda (x) (file-directory-p (concat cake-app-path "views/" cake-plural-name "/" x))) (directory-files dir)))
      (setq view-dir (remove-if (lambda (x) (equal x "..")) view-dir))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name "." cake-view-extension))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name "." cake-view-extension) y)) view-files)
                                       (push (concat x "/" cake-snake-action-name "." cake-view-extension) view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name ".thtml"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name ".thtml") y)) view-files)
                                       (push (concat x "/" cake-snake-action-name ".thtml") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-snake-action-name ".ctp"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-snake-action-name ".ctp") y)) view-files)
                                       (push (concat x "/" cake-snake-action-name ".ctp") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name "." cake-view-extension))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name "." cake-view-extension) y)) view-files)
                                       (push (concat x "/" cake-action-name "." cake-view-extension) view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name ".thtml"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name ".thtml") y)) view-files)
                                       (push (concat x "/" cake-action-name ".thtml") view-files))))
      (loop for x in view-dir do (if (file-exists-p (concat cake-app-path "views/" cake-plural-name "/" x "/" cake-action-name ".ctp"))
                                     (unless (some (lambda (y) (equal (concat x "/" cake-action-name ".ctp") y)) view-files)
                                       (push (concat x "/" cake-action-name ".ctp") view-files)))))
    view-files))

(defun cake-switch-to-controller ()
  "Switch to contoroller."
  (interactive)
  (if (cake-is-file)
      (progn
        (if (file-exists-p (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
            (progn
              (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
              (goto-char (point-min))
              (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
                  (progn
                    (goto-char (point-min))
                    (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t)))
              (recenter))
          (if (y-or-n-p "Make new file?")
              (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
            (message (format "Can't find %s" (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))))))
    (message "Can't switch to contoroller.")))

(defun cake-switch-to-model-testcase ()
  "Switch to model testcase."
  (interactive)
  (if (cake-is-file)
      (cake-switch-to-file (concat cake-app-path "tests/cases/models/" cake-singular-name ".test.php"))
    (message "Can't switch to model testcase.")))

(defun cake-switch-to-controller-testcase ()
  "Switch to contoroller testcase."
  (interactive)
  (if (cake-is-file)
      (cake-switch-to-file (concat cake-app-path "tests/cases/controllers/" cake-plural-name "_controller.test.php"))
    (message "Can't switch to contoroller testcase.")))

(defun cake-switch-to-fixture ()
  "Switch to fixture."
  (interactive)
  (if (cake-is-file)
      (cake-switch-to-file (concat cake-app-path "tests/fixtures/" cake-singular-name "_fixture.php"))
    (message "Can't switch to fixture.")))

(defun cake-switch-to-file (file-path)
  "Switch to file."
  (if (file-exists-p file-path)
      (find-file file-path)
    (if (y-or-n-p "Make new file?")
        (find-file file-path)
      (message (format "Can't find %s" file-path)))))

(defun cake-search-functions ()
  "Search function from current buffer."
  (let ((func-list nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)
        (push (match-string 1) func-list))
      func-list)))

(defun cake-switch-to-function ()
  "Switch to function."
  (interactive)
  (let ((current-func nil))
    (if (and cake-use-imenu
             (require 'imenu nil t))
        (anything 'anything-c-source-imenu)
      (if (or (cake-is-controller-file)
              (cake-is-model-file)
              (cake-is-javascript-file))
          (progn
            (setq current-func (cake-search-functions))
            (anything
             '(((name . "Switch to current function")
                (candidates . current-func)
                (display-to-real . (lambda (candidate)
                                     (concat "function[ \t]*" candidate "[ \t]*\(")))
                (action
                 ("Switch to Function" . (lambda (candidate)
                                           (goto-char (point-min))
                                           (re-search-forward candidate nil t)
                                           (recenter)
                                           )))))
             nil nil nil nil))
        (message "Can't switch to function.")))))

(defun cake-switch-to-element ()
  "Switch to element. If region is active, make new element file."
  (interactive)
  (let ((element-name nil) (themed-path ""))
    (if (cake-is-view-file)
        (progn
          (unless (not cake-themed-name)
            (setq themed-path (concat "themed/" cake-themed-name "/")))
          (if (or (string-match "renderElement( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake-get-current-line))
                  (string-match "element(['\"]\\( *[-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake-get-current-line)))
              (if (file-exists-p (concat cake-app-path "views/" themed-path "elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
                  (find-file (concat cake-app-path "views/" themed-path "elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
                (if (y-or-n-p "Make new file?")
                    (find-file (concat cake-app-path "views/" themed-path "elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension))
                  (message (format "Can't find %s" (concat cake-app-path "views/" themed-path "elements/" (match-string 1 (cake-get-current-line)) "." cake-view-extension)))))
            (if (not (and (region-active-p)
                          (y-or-n-p "Can't find element name. Make new file?")))
                (message "Can't find element name.")
              (setq element-name (read-string "Element name (no extension): " "element_name"))
              (if (not element-name)
                  (message "Can't find element name.")
                (kill-region (point) (mark))
                (insert (concat "<?php echo $this->element('" element-name "'); ?>"))
                (find-file (concat cake-app-path "views/" themed-path "elements/" element-name "." cake-view-extension))
                (yank)))))
      (message "Current buffer is not view."))))

(defun cake-switch-to-javascript ()
  "Switch to javascript."
  (interactive)
  (if (cake-set-app-path)
      (if (or (string-match "$javascript->link( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))
              (string-match "$this->Html->script( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))) ;;1.3x
          (cond
           ((file-exists-p (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line))))
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)))))
           ((file-exists-p (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js"))
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js")))
           ((y-or-n-p "Make new file?")
            (find-file (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js")))
           (message (format "Can't find %s" (concat cake-app-path "webroot/js/" (match-string 1 (cake-get-current-line)) ".js"))))
        (message "Can't find javascript name."))
    (message "Can't set app path.")))

(defun cake-switch-to-css ()
  "Switch to stylesheet."
  (interactive)
  (if (cake-set-app-path)
      (if (or (string-match "$html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))
              (string-match "$this->Html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line)))
          (cond
           ((file-exists-p (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line))))
            (find-file (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line)))))
           ((file-exists-p (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line)) ".css"))
            (find-file (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line)) ".css")))
           ((y-or-n-p "Make new file?")
            (find-file (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line)) ".css")))
           (message (format "Can't find %s" (concat cake-app-path "webroot/css/" (match-string 1 (cake-get-current-line)) ".css"))))
        (message "Can't find stylesheet  name."))
    (message "Can't set app path.")))

(defun cake-switch ()
  "Omni switch function."
  (interactive)
  (if (cake-set-app-path)
      (cond
       ;;cake-switch-to-javascript
       ((or (string-match "$javascript->link( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))
            (string-match "$this->Html->script( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))) (cake-switch-to-javascript)) ;;1.3x
       ;;cake-switch-to-element
       ((or (string-match "renderElement( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake-get-current-line))
            (string-match "element(['\"]\\( *[-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake-get-current-line))) (cake-switch-to-element))
       ;;cake-switch-to-css
       ((or (string-match "$html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))
            (string-match "$this->html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake-get-current-line))) (cake-switch-to-css)) ;;1.3x
       ;;cake-switch-to-controller
       ((cake-is-view-file) (cake-switch-to-controller))
       ;;cake-switch-to-view
       ((cake-is-controller-file) (cake-switch-to-view))
       (t (message "Current buffer is neither view nor controller.")))
    (message "Can't set app path.")))

(defun cake-switch-testcase ()
  "Switch testcase <-> C/M. Or, switch form fixture to testcase."
  (interactive)
  (cond ((cake-is-model-file) (cake-switch-to-model-testcase))
        ((cake-is-controller-file) (cake-switch-to-controller-testcase))
        ((cake-is-model-testcase-file) (cake-switch-to-model))
        ((cake-is-controller-testcase-file) (cake-switch-to-controller))
        ((cake-is-fixture-file) (cake-switch-to-model-testcase))
        (t (message "Current buffer is neither model nor controller."))))

(defun cake-switch-to-file-history ()
  "Switch to file history."
  (interactive)
  (historyf-back '(cake)))

(defun cake-open-dir (dir &optional recursive ignore)
  "Open directory."
  (interactive)
  (if (cake-set-app-path)
      (anything-other-buffer
       (cake-create-open-dir-anything-sources dir recursive ignore) nil)
    (message "Can't set app path.")))

(defun cake-create-open-dir-anything-sources (dir &optional recursive ignore)
  "Careate 'Open dir' anything-sources"
  (let (sources)
    (unless (listp dir)
      (setq dir (list dir)))
    (if (cake-set-app-path)
        (progn
          (loop for d in dir do
                (unless (not (file-directory-p (concat cake-app-path d)))
                  (push
                   `((name . ,(concat "Open directory: " d))
                     (candidates . ,(remove-if (lambda (x) (and ignore (string-match ignore x))) (cake-directory-files d recursive)))
                     (display-to-real . (lambda (candidate)
                                          (concat ,cake-app-path ,d candidate)))
                     (type . file))
                   sources)))
          (reverse sources))
      nil)))

(defun cake-directory-files (dir &optional recursive)
  "Get directory files recuresively."
  (let
      (file-list file)
    (if (not recursive)
        (remove-if (lambda (x) (equal x "..")) (directory-files (concat cake-app-path dir)))
      (loop for x in (cake-get-recuresive-path-list (concat cake-app-path dir))
            do (progn
                 (string-match (concat cake-app-path dir "\\(.+\\)") x)
                 (push (match-string 1 x) file-list)))
      (remove-if (lambda (x) (equal x "..")) file-list))))

(defun cake-get-recuresive-path-list (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (loop for x
          in file-list
          do (if (file-directory-p x)
                 (setq path-list
                       (append
                        (cake-get-recuresive-path-list
                         (remove-if
                          (lambda(y) (string-match "\\.$\\|\\.svn" y)) (directory-files x t)))
                        path-list))
               (setq path-list (push x path-list))))
    path-list))

(defun cake-open-models-dir ()
  "Open models directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "models/") nil))) plugin-list))
    (push "models/" plugin-list)
    (cake-open-dir plugin-list t "behaviors")))

(defun cake-open-views-dir ()
  "Open views directory."
  (interactive)
  (let ((themed-list (cake-find-themed-dir)) (plugin-list (cake-find-plugin-dir)))
    (if (not (or (cake-is-model-file) (cake-is-controller-file) (cake-is-view-file)))
        (cake-open-all-views-dir)
      (setq themed-list (mapcar (function (lambda (c) (if c (concat c cake-plural-name "/") nil))) themed-list))
      (push (concat "views/" cake-plural-name "/") themed-list)
      (cake-open-dir themed-list))))

(defun cake-open-all-views-dir ()
  "Open all views directory."
  (interactive)
  (let ((themed-list (cake-find-themed-dir)) (plugin-list (cake-find-plugin-dir)))
  (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "views/") nil))) plugin-list))
  (push "views/" plugin-list)
  (cake-open-dir plugin-list t "helpers")))

(defun cake-open-controllers-dir ()
  "Open contorollers directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir))
        (controller-list (cake-find-controller-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "controllers/") nil))) plugin-list))
    (setq controller-list (append controller-list plugin-list))
    (push "controllers/" controller-list)
    (cake-open-dir (remove-if (lambda (x) (string-match "components" x)) controller-list) t "components")))

(defun cake-open-behaviors-dir ()
  "Open behaviors directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "models/behaviors/") nil))) plugin-list))
    (push "models/behaviors/" plugin-list)
    (cake-open-dir plugin-list)))

(defun cake-open-helpers-dir ()
  "Open helpers directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "views/helpers/") nil))) plugin-list))
    (push "views/helpers/" plugin-list)
    (cake-open-dir plugin-list)))

(defun cake-open-components-dir ()
  "Open components directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "controllers/components/") nil))) plugin-list))
    (push "controllers/components/" plugin-list)
    (cake-open-dir plugin-list)))

(defun cake-open-libs-dir ()
  "Open libs dir."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "libs/") nil))) plugin-list))
    (push "libs/" plugin-list)
    (cake-open-dir plugin-list t)))

(defun cake-open-config-dir ()
  "Open config dir."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "config/") nil))) plugin-list))
    (push "config/" plugin-list)
    (cake-open-dir plugin-list t)))

(defun cake-open-layouts-dir ()
  "Open layouts directory."
  (interactive)
  (let ((layouts)
        (plugin-list (cake-find-plugin-dir))
        (themed-list (cake-find-themed-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "views/layouts/") nil))) plugin-list))
    (setq themed-list (mapcar (function (lambda (c) (if c (concat c "layouts/") nil))) themed-list))
    (setq layouts (append plugin-list themed-list))
    (push (concat "views/layouts/") layouts)
    (cake-open-dir layouts t)))

(defun cake-open-elements-dir ()
  "Open elements directory."
  (interactive)
  (let ((elements)
        (plugin-list (cake-find-plugin-dir))
        (themed-list (cake-find-themed-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "views/elements/") nil))) plugin-list))
    (setq themed-list (mapcar (function (lambda (c) (if c (concat c "elements/") nil))) themed-list))
    (setq elements (append plugin-list themed-list))
    (push (concat "views/elements/") elements)
    (cake-open-dir elements t)))

(defun cake-open-js-dir ()
  "Open JavaScript directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "webroot/js/") nil))) plugin-list))
    (push "webroot/js/" plugin-list)
    (cake-open-dir plugin-list t)))

(defun cake-open-css-dir ()
  "Open css directory."
  (interactive)
  (let ((plugin-list (cake-find-plugin-dir)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (concat c "webroot/css/") nil))) plugin-list))
    (push "webroot/css/" plugin-list)
    (cake-open-dir plugin-list t)))

(defun cake-open-tests-dir ()
  "Open tests directory."
  (interactive)
  (let ((tests nil)
        (plugin-list (cake-find-plugin-dir)))
    (setq tests (append (mapcar (function (lambda (c) (if c (concat c "tests/groups/") nil))) plugin-list) tests))
    (setq tests (append (mapcar (function (lambda (c) (if c (concat c "tests/fixtures/") nil))) plugin-list) tests))
    (setq tests (append (mapcar (function (lambda (c) (if c (concat c "tests/cases/") nil))) plugin-list) tests))
    (push "tests/groups/" tests)
    (push "tests/fixtures/" tests)
    (push "tests/cases/" tests)
    (cake-open-dir tests t)))

(defun cake-find-themed-dir ()
  "Find themed directory. like app/views/themed/m"
  (cake-find-dir-list "views/themed/"))

(defun cake-find-controller-dir ()
  "Find plugin directory. like app/controllers"
  (remove-if (lambda (x) (string-match "components" x)) (cake-find-dir-list "contollers/")))

(defun cake-find-plugin-dir ()
  "Find plugin directory. like app/plugins"
  (cake-find-dir-list "plugins/"))

(defun cake-find-dir-list (dir)
  "Find directory list."
  (let (d l)
    (if (and (cake-set-app-path) (file-directory-p (concat cake-app-path dir)))
        (progn
          (setq d (concat cake-app-path dir))
          (loop for x in (directory-files d)
                do (unless (or
                            (not (file-directory-p (concat d x)))
                            (string-match "\\.\\.?" x))
                     (push (concat dir x "/") l)))
          (reverse l))
      nil)))

(defvar cake-source-version
  '((name . "CakePHP core version")
    (candidates . (lambda () (list "1.2" "1.3")))
    (action
     ("Set Version" . (lambda (candidate)
                        (setq cake-core-version candidate))))))

(defun cake-set-version ()
  "Set CakePHP version."
  (interactive)
  (if (cake-set-app-path)
      (anything '(cake-source-version)
                nil "Version: " nil)))

(defvar cake-source-models
  '((name . "Cake Model")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (setq c (capitalize c))
                                        (while (string-match "_\\|\.Php" c)
                                          (setq c (replace-match "" nil nil c)))
                                        (concat "$this->loadModel('" c "');")
                                        ))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (file-directory-p (concat cake-app-path "models/" x))))
                                       (directory-files (concat cake-app-path "models/")))
                            )))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake-initial-input))
                        (insert candidate))))))

(defvar cake-source-javascript
  '((name . "Cake JavaScript")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\.js$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (if (string-match "1\.3" cake-core-version)
                                            (concat "$this->Html->script('" c "');")
                                          (concat "$javascript->link('" c "');"))))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (file-directory-p (concat cake-app-path "webroot/js/" x))))
                                       (cake-directory-files "webroot/js/" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake-initial-input))
                        (insert candidate))))))

(defvar cake-source-css
  '((name . "Cake Css")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\.css$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (if (string-match "1\.3" cake-core-version)
                                            (concat "$this->Html->css('" c "');")
                                          (concat "$html->css('" c "');"))))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (file-directory-p (concat cake-app-path "webroot/css/" x))))
                                       (cake-directory-files "webroot/css/" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake-initial-input))
                        (insert candidate))))))

(defvar cake-source-layouts
  '((name . "Cake Layout")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c) (concat "$this->layout = '" c "';")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (file-directory-p (concat cake-app-path "views/layouts/" x))))
                                       (cake-directory-files "views/layouts/" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake-initial-input))
                        (insert candidate))))))

(defvar cake-source-elements
  '((name . "Cake Element")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\." cake-view-extension "$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (concat "$this->element('" c "');")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (file-directory-p (concat cake-app-path "views/elements/" x))))
                                       (cake-directory-files "views/elements/" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake-initial-input))
                        (insert candidate))))))

;;php-completion.el code
(defvar cake-initial-input nil)
(defun cake-get-initial-input ()
  (setq cake-initial-input
        (buffer-substring-no-properties (point)
                                        (progn (save-excursion
                                                 (skip-syntax-backward "w_")
                                                 (point))))))

(defun cake-complete ()
  "Insert CakePHP code."
  (interactive)
  (if (cake-set-app-path)
      (cond
       ((cake-is-controller-file)
        (anything
         '(cake-source-layouts
           cake-source-models)
         (cake-get-initial-input) "Find Code: " nil))
       ((cake-is-view-file)
        (anything
         '(cake-source-javascript
           cake-source-css
           cake-source-elements)
         (cake-get-initial-input) "Find Code: " nil))
       (t
        (anything
         '(cake-source-javascript
           cake-source-css
           cake-source-elements
           cake-source-layouts
           cake-source-models)
         (cake-get-initial-input) "Find Code: " nil))
       )))

(defun cake-logs ()
  "Set logs list."
  (if (cake-set-app-path)
      (mapcar
       (function (lambda (el)
                   (if (listp el) el(cons el el))))
       (directory-files (concat cake-app-path "tmp/logs/") nil "\\.log$"))
    nil))

(defun cake-tail-log (log)
  "Show log by \"tail\"."
  (interactive
   (list (completing-read "tail log: " (cake-logs) nil t "debug.log")))
  (if (require 'tail nil t)             ;xcezx patch.
      (tail-file (concat cake-app-path "tmp/logs/" log))
    (let ((logbuffer (concat "*" log "*")))
      (if (and (cake-logs) (executable-find "tail"))
          (progn
            (unless (get-buffer logbuffer)
              (get-buffer-create logbuffer)
              (set-buffer logbuffer)
              (insert-before-markers (concat "tail -f" cake-app-path "tmp/logs/" log "\n"))
              (setq buffer-read-only t)
              (start-process "tail" logbuffer "tail" "-f" (concat cake-app-path "tmp/logs/" log)))
            (switch-to-buffer logbuffer))
        (message "Can't set log.")))))

;;; anything sources and functions

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-c-cake-anything-only-function
                                '(length cake-initial-input)))

(defvar cake-candidate-function-name nil)

(defvar anything-c-cake-po-file-buffer-name "*Cake Po*")

(defvar anything-c-source-cake
  '((name . "Cake Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake-app-path
                        "controllers/*_controller.php --with-filename")
                nil (current-buffer))
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake-app-path
                        "*_controller.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (re-search-forward ".+\\/\\([^\\/]+\\)_controller\.php:.*function *\\([^ ]+\\) *(.*).*$" nil t)
                 (replace-match (concat (match-string 1) " / " (match-string 2))))
               )
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names)
    (action
     ("Switch to Contoroller" . (lambda (candidate)
                                  (anything-c-cake-switch-to-controller)))
     ("Switch to View" . (lambda (candidate)
                           (anything-c-cake-switch-to-view)))
     ("Switch to Model" . (lambda (candidate)
                            (anything-c-cake-switch-to-model))))))

(defun anything-c-cake-set-names (candidate)
  "Set names by display-to-real"
  (progn
    (string-match "\\(.+\\) / \\(.+\\)" candidate)
    (setq cake-plural-name (match-string 1 candidate))
    (setq cake-action-name (match-string 2 candidate))
    (setq cake-singular-name (cake-singularize cake-plural-name))
    (setq cake-lower-camelized-action-name cake-action-name)
    (setq cake-snake-action-name (cake-snake cake-action-name))))

(defun anything-c-cake-switch-to-view ()
  "Switch to view."
  (let (themed-dir
        (plural-name cake-plural-name)
        (action-name cake-action-name)
        (snake-action-name cake-snake-action-name))
    (progn
      (cake-set-app-path)
      (if (and (cake-is-view-file) cake-themed-name)
          (setq themed-dir (concat "themed/" cake-themed-name "/")))
      (cond
       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name "." cake-view-extension))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name "." cake-view-extension)))
       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name ".thtml"))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name ".thtml")))
       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name ".ctp"))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" snake-action-name ".ctp")))

       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" action-name "." cake-view-extension))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" action-name "." cake-view-extension)))
       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" action-name ".thtml"))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" action-name ".thtml")))
       ((file-exists-p (concat cake-app-path "views/" themed-dir plural-name "/" action-name ".ctp"))
        (find-file (concat cake-app-path "views/" themed-dir plural-name "/" action-name ".ctp")))

       ((file-exists-p (concat cake-app-path "views/" plural-name "/" snake-action-name "." cake-view-extension))
        (find-file (concat cake-app-path "views/" plural-name "/" snake-action-name "." cake-view-extension)))
       ((file-exists-p (concat cake-app-path "views/" plural-name "/" snake-action-name ".thtml"))
        (find-file (concat cake-app-path "views/" plural-name "/" snake-action-name ".thtml")))
       ((file-exists-p (concat cake-app-path "views/" plural-name "/" snake-action-name ".ctp"))
        (find-file (concat cake-app-path "views/" plural-name "/" snake-action-name ".ctp")))

       ((file-exists-p (concat cake-app-path "views/" plural-name "/" action-name "." cake-view-extension))
        (find-file (concat cake-app-path "views/" plural-name "/" action-name "." cake-view-extension)))
       ((file-exists-p (concat cake-app-path "views/" plural-name "/" action-name ".thtml"))
        (find-file (concat cake-app-path "views/" plural-name "/" action-name ".thtml")))
       ((file-exists-p (concat cake-app-path "views/" plural-name "/" action-name ".ctp"))
        (find-file (concat cake-app-path "views/" plural-name "/" action-name ".ctp")))

       ((y-or-n-p "Make new file?")
        (unless (file-directory-p (concat cake-app-path "views/" plural-name "/"))
          (make-directory (concat cake-app-path "views/" plural-name "/")))
        (find-file (concat cake-app-path "views/" plural-name "/" action-name "." cake-view-extension)))
       (t (message (format "Can't find %s" (concat cake-app-path "views/" plural-name "/" action-name "." cake-view-extension))))))))

(defun anything-c-cake-switch-to-controller ()
  "Switch to contoroller."
  (cake-set-app-path)
  (if (file-exists-p (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
      (progn
        (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (goto-char (point-min))
        (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
            (progn
              (goto-char (point-min))
              (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t))))
    (if (file-exists-p (concat cake-app-path cake-plural-name "_controller.php"))
        (progn
          (find-file (concat cake-app-path cake-plural-name "_controller.php"))
          (goto-char (point-min))
          (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
              (progn
                (goto-char (point-min))
                (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t))))
      (if (y-or-n-p "Make new file?")
          (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (message (format "Can't find %s" (concat cake-app-path "controllers/" cake-plural-name "_controller.php")))))))

(defun anything-c-cake-switch-to-model ()
  "Switch to model."
  (cake-set-app-path)
  (if (file-exists-p (concat cake-app-path "models/" cake-singular-name ".php"))
      (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
    (if (y-or-n-p "Make new file?")
        (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
      (message (format "Can't find %s" (concat cake-app-path "models/" cake-singular-name ".php"))))))

(defun anything-c-cake-switch-to-file-function (dir)
  "Switch to file and search function."
  (cake-set-app-path)
  (if (not (file-exists-p (concat cake-app-path dir cake-singular-name ".php")))
      (if (y-or-n-p "Make new file?")
          (find-file (concat cake-app-path dir cake-singular-name ".php"))
        (message (format "Can't find %s" (concat cake-app-path dir cake-singular-name ".php"))))
    (find-file (concat cake-app-path dir cake-singular-name ".php"))
    (goto-char (point-min))
    (re-search-forward (concat "function[ \t]*" cake-candidate-function-name "[ \t]*\(") nil t)))

(defvar anything-c-source-cake-model-function
  '((name . "Cake Model Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake-app-path
                        "models/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (match-string 1)))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake-switch-to-file-function "models/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defvar anything-c-source-cake-component-function
  '((name . "Cake Component Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake-app-path
                        "controllers/components/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (match-string 1)))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake-switch-to-file-function "controllers/components/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defvar anything-c-source-cake-behavior-function
  '((name . "Cake Behavior Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake-app-path
                        "models/behaviors/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (match-string 1)))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake-switch-to-file-function "models/behaviors/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defun anything-c-cake-set-names2 (candidate)
  "Set names by display-to-real"
  (progn
    (string-match "\\(.+\\)->\\(.+\\)" candidate)
    (setq cake-camelized-singular-name (match-string 1 candidate))
    (setq cake-candidate-function-name (match-string 2 candidate))
    (setq cake-singular-name (cake-snake cake-camelized-singular-name))))

(defun anything-c-cake-create-po-file-buffer ()
  "Create buffer from po file."
  (let ((anything-buffer (anything-candidate-buffer 'global)))
    (catch 'invalid-po-file
      (unless (anything-c-cake-generate-po-file-buffer (concat cake-app-path "locale/" cake-po-file-path))
        (message "Can't find po file: %s" (concat cake-app-path "locale/" cake-po-file-path))
        (throw 'invalid-po-file nil))
      (with-current-buffer anything-buffer
        (set-syntax-table (with-current-buffer anything-current-buffer
                            (syntax-table)))
        (insert-buffer-substring anything-c-cake-po-file-buffer-name)))))

(defun anything-c-cake-generate-po-file-buffer (po-file)
  "Generate po file buffer"
  (when (and po-file
             (file-exists-p po-file)
             (file-regular-p po-file))
    (with-current-buffer (get-buffer-create anything-c-cake-po-file-buffer-name)

      (erase-buffer)
      (insert-file-contents po-file)

      (goto-char (point-min))
      (while (re-search-forward "^[^m].*\n" nil t)
        (replace-match ""))

      (goto-char (point-min))
      (while (re-search-forward "^msgid \"\\(.*\\)\"\nmsgstr \"\\(.*\\)\"$" nil t)
        (replace-match "\\1 / \\2"))
      )
    t))

(defvar anything-c-source-cake-po
  '((name . "Cake po file's msgid and msgstr")
    (init . (lambda ()
              (cake-set-app-path)
              (setq path cake-app-path)
              (anything-c-cake-create-po-file-buffer)))
    (candidates-in-buffer)
    (action
     ("Insert __('msgid')." . (lambda (candidate)
                                (insert (concat "__('" (anything-c-cake-get-msgid candidate) "')"))))
     ("Insert __('msgid',true)." . (lambda (candidate)
                                     (insert (concat "__('" (anything-c-cake-get-msgid candidate) "',true)"))))
     ("Insert msgid." . (lambda (candidate)
                          (insert (anything-c-cake-get-msgid candidate))))
     ("Insert msgstr." . (lambda (candidate)
                           (insert (anything-c-cake-get-msgstr candidate))))
     ("Goto po file" . (lambda (candidate)
                         (find-file (concat path "locale/" cake-po-file-path))
                         (goto-char (point-min))
                         (re-search-forward (concat "\"" (anything-c-cake-get-msgid candidate) "\"") nil t))))))

(defvar anything-c-source-cake-po-not-found
  '((name . "Create __()")
    (init . (lambda ()
              (cake-set-app-path)
              (setq path cake-app-path)))
    (dummy)
    (action
     ("Insert __('msgid')." . (lambda (candidate)
                                (insert (concat "__('" candidate "')"))))
     ("Insert __('msgid',true)." . (lambda (candidate)
                                     (insert (concat "__('" candidate "',true)"))))
     ("Insert msgid." . (lambda (candidate)
                          (insert candidate)))
     ("Goto po file" . (lambda (candidate)
                         (find-file (concat path "locale/" cake-po-file-path))
                         (goto-char (point-max)))))))

(defun anything-c-cake-get-msgid (candidate)
  "Set msgid"
  (progn
    (string-match "\\(.+\\) /" candidate)
    (match-string 1 candidate)))

(defun anything-c-cake-get-msgstr (candidate)
  "Set msgstr"
  (progn
    (string-match "/ \\(.+\\)$" candidate)
    (match-string 1 candidate)))

(defun anything-c-cake-anything-only-source-cake ()
  "anything only anything-c-source-cake and anything-c-source-cake-model-function."
  (interactive)
  (anything (list anything-c-source-cake
                  anything-c-source-cake-model-function
                  anything-c-source-cake-component-function
                  anything-c-source-cake-behavior-function)
            nil "Find CakePHP Sources: " nil nil))

(defun anything-c-cake-anything-only-function ()
  "anything only anything-c-source-cake-function."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything (list anything-c-source-cake-model-function
                    anything-c-source-cake-component-function
                    anything-c-source-cake-behavior-function) initial-pattern "Find Cake Functions: " nil)))

(defun anything-c-cake-anything-only-model-function ()
  "anything only anything-c-source-cake-model-function."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything '(anything-c-source-cake-model-function) initial-pattern "Find Model Functions: " nil)))

(defun anything-c-cake-anything-only-po ()
  "anything only anything-c-source-cake-po."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything (list anything-c-source-cake-po
                    anything-c-source-cake-po-not-found)
              initial-pattern "Find Msgid And Msgstr: " nil)))

;; mode provide
(provide 'cake)

;;; end
;;; cake.el ends here
