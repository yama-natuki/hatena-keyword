;;; hatena-keyword.el --- 

;; Copyright (C) 2009  yama

;; Author: yama  <yama.natuki+elisp＠gmail.com>
;; Keywords: 

;; 	$Id: hatena-keyword.el,v 1.28 2009/05/03 13:35:06 yama Exp yama $	

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; はてなキーワードをEmacsから検索して表示するlispです。
;; Emacsからはてなキーワードを辞書のように扱えます。
;; より詳しい情報は、http://ubulog.blogspot.com/

;;; Install

;; (1) emacs-w3m を利用しています。事前に用意して下さい。
;; (2) このファイルをload-pathの通った場所に置きます。
;; (3) ~/.emacs に以下を追記します。
;;        (require 'hatena-keyword)
;;        (global-set-key "\C-ck" 'hatekey)

;;; Usage:

;;   C-c k でカーソル付近の単語を調べます。
;;   TABキーでリンクされたキーワードにカーソルを移動します。
;;   Shift + TAb で逆順。
;;   リンク上でリターンキーを押すとそのキーワードを検索して表示します。
;;   リンクがキーワードではない場合はw3mでリンク先を開きます。
;;   ←キーで表示したキーワードを戻ることが出来ます。

;;; ToDo

;;   



;;; Code:

(require 'url)
(require 'xml)
(require 'w3m)

;;; configuration

(defvar hatenakey-window-height 14
  "*Height of window to show entrys and contents.
   検索結果表示ウインドウの高さ")

(defvar hatena-coding-system 'utf-8
  "文字エンコードの指定。utf-8,euc-jp,shift_jis.")



(defconst hatenakey-version "1.1")
(defconst hatenakey-buffer-name "*hatena-word*"
  "検索結果表示バッファの名前")

(defconst hatenakey-history-name "*hatena-history*"
  "履歴バッファの名前")

(defvar hatenakey-history '()
"履歴スタック")

(defvar hatena-keyword-mode nil
  "Non-nil if Hatena Keyword mode is enabled.
Don't change this variable directly, you must change it by one of the
functions that enable or disable Hatena Keyword mode.")

;;  Define keymap
(defvar hatekey-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map " " 'scroll-up)
	(define-key map "b" 'scroll-down)
	(define-key map "q" 'view-kill-window)
	(define-key map "\t" 'w3m-next-anchor)
	(define-key map [tab] 'w3m-next-anchor)
	(define-key map [(shift tab)] 'w3m-previous-anchor)
	(define-key map [backtab] 'w3m-previous-anchor)
	(define-key map [left] 'hatena-view-previous-page)
	(define-key map "\C-m" 'hatena-keyword-jump)
	map))


;;; main ------------------------------------------------------

(defun hatekey-version ()
  "バージョンを返す。"
  (interactive)
  (message "hatena-keyword %s" hatenakey-version))


(defun hatena-keyword-mode ()
  "はてなキーワード を表示するメジャーモードです.
調べたい文字列の上で \\[hatekey] を実行することによりキーワードを検索して表示します。
\\<hatekey-mode-map>

key     binding
---     -------
\\[view-kill-window]		   quit.
\\[scroll-up]		           scroll-up
\\[scroll-down]		           scroll-down
\\[w3m-next-anchor]		       Go to Next Keyword Link
\\[w3m-previous-anchor]	       Go to previous keyword Link
\\[hatena-view-previous-page]  Back to History.
\\[hatena-keyword-jump]		   jump to keyword
"
  (interactive)
  (setq major-mode 'hatena-keyword-mode
		mode-name "hatekey"
		buffer-read-only t)
  (use-local-map hatekey-mode-map)
  (run-hooks 'hatena-keyword-mode-hook))


(defun hatena-url-restructure (keyword)
  "はてなのurlを構築する。"
  (concat "http://d.hatena.ne.jp/keyword?word="
		  (w3m-url-encode-string keyword hatena-coding-system)
		  "&mode=rss2&ie="
		  (symbol-name hatena-coding-system)))

(defun my-keyword (url)
  "キーワードの解説を返す。"
  (interactive)
	(set-buffer (url-retrieve-synchronously url))
	(if (string-match "200 OK" (buffer-substring (point-min) 20))
		(progn
		  (let* ((root (xml-parse-region (point-min) (point-max)))
				 (base (car (xml-get-children (car root) 'item)))
				 (my-desc (nth 2 (car (xml-get-children base 'description)))))
			(if (eq my-desc nil) (setq my-desc "No Match") nil)
			(if (hatena-buffer-p) (setq buffer-read-only nil))
			(set-buffer (get-buffer-create hatenakey-buffer-name))
			(erase-buffer)
			(insert (decode-coding-string my-desc hatena-coding-system))
			(w3m-buffer)
			(if (hatena-window-p)
				(progn
				  (select-window (hatena-window-p))
				  (hatena-keyword-mode))
			  (split-window-vertically
			   (- (window-height) hatenakey-window-height))
			  (set-window-buffer (next-window) (current-buffer))
			  (select-window (next-window))
			  (hatena-keyword-mode))))
	  (message "%s" "Not Found")))


(defun hatena-buffer-p ()
  "はてなバッファの存在チェック"
  (if (get-buffer hatenakey-buffer-name)
	  (set-buffer hatenakey-buffer-name)))

(defun hatena-window-p ()
  "はてなウィンドウのチェック。"
  (get-buffer-window (get-buffer hatenakey-buffer-name)))

(defun hatekey (keyword &optional flag)
  "はてなキーワードを検索する。"
  (interactive
   (list (cond ((or (eq last-command 'mouse-drag-region)
					(and (boundp 'transient-mark-mode) transient-mark-mode
						 (boundp 'mark-active) mark-active)
					(eq last-command 'exchange-point-and-mark))
				(buffer-substring-no-properties
				 (region-beginning) (region-end)))
			   (t (thing-at-point 'word)))
		 current-prefix-arg))
  (if (eq (fboundp 'w3m-buffer) nil) (message "w3m-buffer関数が見つかりません。")
	(if flag nil
	  (let* ((keyword (read-from-minibuffer "Hatena Keyword: " keyword)))
		(my-keyword (hatena-url-restructure keyword))
		(hatenakey-history-push keyword)))))


(defun hatenakey-close-window ()
  "検索表示バッファを表示しているウインドウを消去する関数"
  (interactive)
  (let ((w (get-buffer-window hatenakey-buffer-name))
	(b (get-buffer hatenakey-buffer-name)))
    (if w
	(progn
	  (bury-buffer b)
	  (if (= (window-height w) hatenakey-window-height)
	      (delete-window w)
	    (set-window-buffer w (other-buffer))
	    (select-window (next-window)))))))

(defun hatena-this-keyword ()
  "現在位置のリンクのキーワードを返す。urlならw3mで開く。"
  (let ((url  (w3m-print-this-url)))
	(if (string-match "keyword" url)
		(replace-regexp-in-string
		 "http://d.hatena.ne.jp/keyword/" "" url)
	  (w3m-browse-url url))))

(defun hatena-keyword-jump ()
  "pointのキーワードを検索"
  (interactive)
  (if (hatena-this-keyword)
	  (progn
		(let ((key (hatena-this-keyword)))
		  (my-keyword (hatena-url-restructure key))
		  (hatenakey-history-push key)))))

(defun view-kill-window ()
  "検索結果表示ウィンドウを削除する関数"
  (interactive)
  (if (get-buffer hatenakey-buffer-name)
	  (progn
		(hatenakey-close-window)
		(kill-buffer hatenakey-buffer-name)
		(setq hatenakey-history '())
		(delete-other-windows))))

(defun hatenakey-history-name-p ()
  "履歴バッファが存在するかどうか"
  (set-buffer (get-buffer-create hatenakey-history-name)))

(defun hatenakey-history-push (keyword)
  "履歴にキーワードを追加する。"
  (setq hatenakey-history (cons keyword hatenakey-history)))

(defun hatenakey-history-pop ()
  "履歴からキーワードを取り出す。履歴からは消える。"
  (setq hatenakey-history (cdr hatenakey-history))
  (car hatenakey-history))
  
(defun hatena-view-previous-page ()
  "一つ前に見たキーワードへ戻る。"
  (interactive)
  (my-keyword (hatena-url-restructure (hatenakey-history-pop))))

(provide 'hatena-keyword)
;;; hatena-keyword.el ends here
